#################
# DeadReckoningPerDay

# Overview:
# Take each of the days of data and perform the calibration/reorientation as well as dead reckoning
# Save the corrected DR paths for analysis

# Requires:
# time-corrected, scaled IMU data and gps in day chunks
# predictions for the probable behaviour throughout

#################

# Read in the data sources ------------------------------------------------
accel_data <- fread(accel_day) %>% 
  arrange(utc_datetime) %>%
  dplyr::filter(utc_datetime >= start_time) # only use the times from deployment
predictions <- fread(predictions_day) %>% # these are the behavioural predictions
  arrange(Time) %>%
  dplyr::filter(Time >= start_time) %>%
  rename(utc_datetime = Time,
         Activity = predicted_class) %>%
  select(utc_datetime, Activity)
# join them together so that all the rows of the accel get a label from the predictions
setDT(accel_data)
setDT(predictions)
setkey(accel_data, utc_datetime)
setkey(predictions, utc_datetime)
all_data <- predictions[accel_data, roll = TRUE, on = "utc_datetime"]


# changing to be scaled not cleaned
# TODO: Go back to original code, rescale and remove the med filter.
all_data$RawAX.sc <- all_data$RawAX.sc * 4
all_data$RawAY.sc <- all_data$RawAY.sc * 4
all_data$RawAZ.sc <- all_data$RawAZ.sc * 4

base_cols <- c("RawAX", "RawAY", "RawAZ")
for (col in base_cols[1:3]) {
  all_data[, (paste0(col, ".sm")) :=
         rollapply(get(paste0(col, ".sc")),
                   width = 50,
                   FUN = mean,
                   align = "center",
                   fill = "extend")]
}
# Convert Activity to ME --------------------------------------------------
# calucaulte the VDBA 
# TODO: Update this when I have changed the cleaning logic
all_data$VDBA <- sqrt((all_data$RawAX.sc - all_data$RawAX.sm)^2 + 
                    (all_data$RawAY.sc - all_data$RawAY.sm)^2 +
                    (all_data$RawAZ.sc - all_data$RawAZ.sm)^2)                     
all_data$VDBA.sm <- rollapply(all_data$VDBA, width=50, FUN=mean, align="center", fill="extend")  # 1 s sm

## above this line is working
# TODO: add back in the butterworth low pass filter :)


# in Gundog.Tracks the ME is whether the aniumal is moving (>0) or not (0)
# I have made behavioural predictions and will choose which ones allow forward movement
all_data$ME <- ifelse(all_data$Activity %in% c("Locomotion_Walk", "Locomotion_Fast"), 1, 0) # "Foraging_Headdown"

# some of the foraging has walking in it, and some doesnt -----------------
# try to separate that here... currently working off the variance in the Z axis...
foraging <- all_data %>%
  dplyr::filter(Activity == "Foraging_Headdown") %>%
  select(utc_datetime, RawAZ.sc) %>%
  mutate(second_window = floor_date(utc_datetime, unit = "second")) %>%
  group_by(second_window) %>%
  mutate(z_sd    = sd(RawAZ.sc)) %>%
  ungroup()
window_summary <- foraging %>%
  distinct(second_window, z_sd) %>%
  na.omit()
foraging <- foraging %>%
  mutate(Move = if_else(z_sd < 0.15, "Foraging_Headdown_Static", "Foraging_Headdown_Moving"))

# ggplot(foraging, aes(x = seq(1:nrow(foraging)), colour = Move, group = 1)) +
#   geom_path(aes(y = RawAZ.cl))

# merge back in
all_data <- merge(all_data, foraging, by = "utc_datetime", all.x = TRUE)
all_data$ME <- ifelse(all_data$Activity == "Foraging_Headdown", 
                      ifelse(all_data$Move == "Foraging_Headdown_Moving", 1, 0), # if its foraging and moving then tag
                      all_data$ME) # leave as originally

save_data <- all_data

# Cleaning rhe GPS --------------------------------------------------------
play_data <- save_data
# getting rhe VDBA per 10 min incremment centered on the GPS points
window_n <- fs * 60 * 5
gps_rows <- which(!is.na(play_data$lon))
vdba_10min <- sapply(gps_rows, function(i) {
  idx <- (i - window_n):(i + window_n)
  idx <- idx[idx >= 1 & idx <= nrow(play_data)]  # clip at start/end of data
  mean(play_data$VDBA.sm[idx], na.rm = TRUE)
})
gps_data <- play_data[gps_rows, .(utc_datetime, lon, lat, ME)]
gps_data[, vdba_10min := vdba_10min]
# and now get the distance per 10 min (5 before and 5 after, previous to now + now to next)
library(geosphere)
gps_data[, dist_m := c(NA, distHaversine(cbind(lon[-.N], lat[-.N]), 
                                         cbind(lon[-1], lat[-1])))]
gps_data[, dist_10min := dist_m + shift(dist_m, n = 1, type = "lead")]
# tag them if they're suspicious
gps_data$sus_threshold <- ifelse(gps_data$dist_10min > 600, "sus", "okay")

# ggplot(gps_data, aes(x = vdba_10min, y = dist_10min, colour = sus_threshold)) +
#   geom_point()

# naughty gps points get averaged
gps_data$average.lon <- ifelse(
  gps_data$sus_threshold == "sus",
  rowMeans(cbind(dplyr::lag(gps_data$lon), gps_data$lon, dplyr::lead(gps_data$lon)), na.rm = TRUE),
  gps_data$lon
)
gps_data$average.lat <- ifelse(
  gps_data$sus_threshold == "sus",
  rowMeans(cbind(dplyr::lag(gps_data$lat), gps_data$lat, dplyr::lead(gps_data$lat)), na.rm = TRUE),
  gps_data$lat
)

# now we low pass filter # but we have to substract the linear fit
bw_order = 4
bf <- butter(bw_order, 0.6, type = "low")

gps_data <- gps_data[!is.na(gps_data$average.lat), ]

# latitde
model <- lm(average.lat ~ as.numeric(utc_datetime), data = gps_data)
gradient <- summary(model)$coefficients[2,1]
intercept <- summary(model)$coefficients[1,1]
linearFit <- as.numeric(gps_data$utc_datetime) * gradient + intercept
gps_data$average.lat <- filtfilt(bf, gps_data$average.lat - linearFit) + linearFit

# now for lon
model <- lm(average.lon ~ as.numeric(utc_datetime), data = gps_data)
gradient <- summary(model)$coefficients[2,1]
intercept <- summary(model)$coefficients[1,1]
linearFit <- as.numeric(gps_data$utc_datetime) * gradient + intercept
gps_data$average.lon <- filtfilt(bf, gps_data$average.lon - linearFit) + linearFit

# ggplot(gps_data) +
#   geom_path( aes(x = sm_lon, y = sm_lat)) +
#   geom_path( aes(x = average.lon, y = average.lat), colour = "red") + my_theme()
# 
# 
# ggplot(gps_data, aes(x = utc_datetime)) + 
#   geom_path(aes(y = average.lat), colour = "red") +
#   geom_path(aes(y = sm_lat)) + my_theme()

# can go a little bit more aggressive but not too much
gps_data <- gps_data %>% select("utc_datetime", "average.lon", "average.lat")
# now add these new gps points back to the original data
all_data <- merge(all_data, gps_data, by = "utc_datetime", all = TRUE)



# Compass alignment -------------------------------------------------------
# here I have the two methods for calcualting the DR.
# Gundog.Compass depends on an intial magnetometer calibration
# Onboard_calcs uses the attitute as estimated from the onboard calculated quaternions
compass_method = "Onboard_calcs"

if (compass_method == "Gundog.Compass"){
  
  # Extract the calibration data (which should have been smoothed in the same way)
  cal_data <- fread(file.path(base_path, "Data", "RawData", Collar, "calibration_data.csv"))
  cal_data$ME <- "M" # overwrite M for the calibration period
  both_data <- rbind(cal_data, all_data, fill = TRUE)
  
  # see the docx in the git for how we determined these orientations
  acc_orientation <- "NWU"
  mag_orientation <- "NED"
  gravity_direction <- "down"
  
  # pitch was determined from extracting known walking events and then taking the mean of axes during those times
  pitch <- atan2(-(-0.253599267), sqrt(0.259529436^2 + 0.891019352^2))
  pitch_deg <- pitch * 180 / pi
  
  # Gundog.Compass
  alldata_rotated <- with(both_data, Gundog.Compass(mag.x = RawMX.sm, mag.y = RawMY.sm, mag.z = RawMZ.sm,
                                                   acc.x = RawAX.sm, acc.y = RawAY.sm, acc.z = RawAZ.sm,
                                                   ME = ME,
                                                   acc.ref.frame = acc_orientation, 
                                                   positive.g = gravity_direction, 
                                                   mag.ref.frame = mag_orientation,
                                                   pitch.offset = -pitch_deg, 
                                                   roll.offset = 0,
                                                   yaw.offset = 0,
                                                   method = 2, # guessed and checked until I found the best one
                                                   algorithm = "standard",
                                                   plot = TRUE))
  
  # Remove the calibration data and now you have your corrected trial data.
  setDT(alldata_rotated)
  correcteddata <- cbind(both_data, alldata_rotated[, c("Pitch", "Roll", "Yaw")])
  correcteddata <- correcteddata %>% dplyr::filter(ME != "M")
  
  # frite(correcteddata, "Board_Accel_Section_Gundog.Compass.csv")
  
} else if (compass_method == "Onboard_calcs"){
  
  # using the columsn already calculated onboard the devices...
  # code adapted from ChrisB
  
  all_data[, c("Q9_1", "Q9_2", "Q9_3") := lapply(
    .SD, function(x) suppressWarnings(as.numeric(iconv(x, from = "", to = "UTF-8", sub = NA)))),
    .SDcols = c("Q9_1", "Q9_2", "Q9_3")]
  
  # get the quaternions
  q1 <- as.numeric(all_data$Q9_1)
  q2 <- as.numeric(all_data$Q9_2)
  q3 <- as.numeric(all_data$Q9_3)
  q0 = sqrt( 1.0 - ((q1 * q1) + (q2 * q2) + (q3 * q3)))
  qw = q0
  qx = q2
  qy = q1
  qz = -q3
  
  # Roll
  t0 = +2.0 * (qw * qx + qy * qz)
  t1 = +1.0 - 2.0 * (qx^2 + qy^2)
  all_data$Roll = atan2(t0, t1) * 180.0 / pi
  
  # Pitch
  t2 <- +2.0 * (qw * qy - qx * qz)
  t2 <- ifelse(t2 > 1.0, 1.0, t2)
  t2 <- ifelse(t2 < -1.0, -1.0, t2)
  all_data$Pitch <- asin(t2) * 180.0 / pi
  
  # # Yaw
  t3 <- +2.0 * (qw * qz + qx * qy)
  t4 <- +1.0 - 2.0 * (qy^2 + qz^2)
  all_data$Yaw <- atan2(t3, t4) * 180.0 / pi
  
  
  if(any(is.na(all_data$Yaw))){
    temp <- which(is.na(all_data$Yaw))
    all_data <- all_data[-temp, ]
  }
  # RPY <- cbind(accel_data$Roll,accel_data$Pitch,accel_data$Yaw)
  # dev.new()
  # matplot(RPY, type = "l", lty = 1, col = c("blue", "red","green"), lwd = 2, xlab = "Index", ylab = "Rotation")
  # legend("topright", legend = c("Roll", "Pitch", "Yaw"), col = c("blue", "red", "green"), lty = 1, lwd = 2, cex = 0.75, bg = "transparent", bty = "n")
  
  correcteddata <- all_data
} 

# projected_path <- with(correcteddata, Gundog.Tracks(TS = utc_datetime, h = Yaw, v = VDBA.sm,
#                                                     ME = ME,
#                                                     method = NULL,
#                                                     plot = TRUE))

# Gundog.Tracks -----------------------------------------------------------
# and then use the gps to do VPC
first_lo <- na.omit(correcteddata$average.lon)[1]
first_lat <- na.omit(correcteddata$average.lat)[1]

projected_path2 = with(correcteddata, Gundog.Tracks(TS = utc_datetime, 
                                                    h = Yaw, 
                                                    v = VDBA.sm, 
                                                    ME = ME,
                                                    lo = first_lo,
                                                    la = first_lat,
                                                    VP.lon = average.lon, 
                                                    VP.lat = average.lat,
                                                    method = "All",
                                                    plot = TRUE,
                                                    bound = FALSE))





## issue, they need the next GPS from the next day to constarin it ######
# thius still has some error we dont know why.





# Now add the predictions back in and save --------------------------------
save_data <- save_data %>% 
  select(utc_datetime, Activity, VDBA.sm, ME, average.lon, average.lat)
save_dr <- projected_path2 %>% 
  select(Timestamp, DR.longitude, DR.latitude, DR.longitude.corr, DR.latitude.corr) %>%
  rename(utc_datetime = Timestamp)

completed_data <- merge(save_data, save_dr, by = "utc_datetime", all = TRUE)


dat <- completed_data[seq(1, nrow(completed_data), by = 200), ]  # downsample
ggplot(dat, aes(x = DR.longitude.corr, y = DR.latitude.corr, colour = VDBA.sm)) + geom_point()









play_data <- save_data
# find the average VDBA in the 5 around (2.5 before, 2.5 after) each of the GPS points
play_data[, bins := ceiling(.I / (fs * (60*2.5)))] # split it into 2.5 min bins
# extract the bins with the gps
gps_bins <- unique(play_data$bins[!is.na(play_data$lon)])
# now get those bins, and the bins before and after
all_bins <- c(gps_bins - 1, gps_bins, gps_bins + 1)
# find the mean VDBA inside each of these bins
vdba_data <- play_data %>%
  dplyr::filter(bins %in% all_bins) %>% 
  group_by(bins) %>%
  summarise(lon = lon, lat = lat, mean = mean(VDBA.sm))


# getting rhe VDBA per 10 min incremment centered on the GPS points
window_n <- fs * 60 * 5
gps_rows <- which(!is.na(play_data$lon))
vdba_10min <- sapply(gps_rows, function(i) {
  idx <- (i - window_n):(i + window_n)
  idx <- idx[idx >= 1 & idx <= nrow(play_data)]  # clip at start/end of data
  mean(play_data$VDBA.sm[idx], na.rm = TRUE)
})
gps_data <- play_data[gps_rows, .(lon, lat)]
gps_data[, vdba_10min := vdba_5min]
# and now get the distance per 10 min (5 before and 5 after, previous to now + now to next)
library(geosphere)
gps_data[, dist_m := c(NA, distHaversine(cbind(lon[-.N], lat[-.N]), 
                                         cbind(lon[-1], lat[-1])))]
gps_data[, dist_10min := dist_m + shift(dist_m, n = 1, type = "lead")]

gps_data$sus_threshold <- ifelse(gps_data$dist_10min > 600, "sus", "okay")

ggplot(gps_data, aes(x = vdba_5min, y = dist_10min, colour = sus_threshold)) + 
  geom_point()


# naughty gps points get averaged
gps_data$average.lon <- ifelse(
  gps_data$sus_threshold == "sus",
  rowMeans(cbind(dplyr::lag(gps_data$lon), gps_data$lon, dplyr::lead(gps_data$lon)), na.rm = TRUE),
  gps_data$lon
)
gps_data$average.lat <- ifelse(
  gps_data$sus_threshold == "sus",
  rowMeans(cbind(dplyr::lag(gps_data$lat), gps_data$lat, dplyr::lead(gps_data$lat)), na.rm = TRUE),
  gps_data$lat
)

# now we low pass filter # but we have to substract the linear fit
bw_order = 4
bf <- butter(bw_order, 0.6, type = "low")

gps_data <- gps_data[!is.na(gps_data$average.lat), ]

# latitde
model <- lm(average.lat ~ as.numeric(utc_datetime), data = gps_data)
gradient <- summary(model)$coefficients[2,1]
intercept <- summary(model)$coefficients[1,1]
linearFit <- as.numeric(gps_data$utc_datetime) * gradient + intercept
gps_data$sm_lat <- filtfilt(bf, gps_data$average.lat - linearFit) + linearFit

# now for lon
model <- lm(average.lon ~ as.numeric(utc_datetime), data = gps_data)
gradient <- summary(model)$coefficients[2,1]
intercept <- summary(model)$coefficients[1,1]
linearFit <- as.numeric(gps_data$utc_datetime) * gradient + intercept
gps_data$sm_lon <- filtfilt(bf, gps_data$average.lon - linearFit) + linearFit

# ggplot(gps_data) +
#   geom_path( aes(x = sm_lon, y = sm_lat)) +
#   geom_path( aes(x = average.lon, y = average.lat), colour = "red") + my_theme()
# 
# 
# ggplot(gps_data, aes(x = utc_datetime)) + 
#   geom_path(aes(y = average.lat), colour = "red") +
#   geom_path(aes(y = sm_lat)) + my_theme()

# can go a little bit more aggressive but not too much



fwrite(completed_data, file.path(dr_output_dir, paste0(date, "_Gundogged.csv")))





















# Play zone ---------------------------------------------------------------

# is there something different between day 2 and day 3 such that the first does but the second doesnt work

# 
# # Are the ranges the same? ------------------------------------------------
# day2 <- fread(grep(dates[2], accel_files, value = TRUE))
# day3 <- fread(grep(dates[3], accel_files, value = TRUE))
# 
# library(geosphere)
# distGeo(c(range(day2$lon, na.rm = TRUE)[1], range(day2$lat, na.rm = TRUE)[1]), c(range(day2$lon, na.rm = TRUE)[2], range(day2$lat, na.rm = TRUE)[2])) / 1000  # in km
# distGeo(c(range(day3$lon, na.rm = TRUE)[1], range(day3$lat, na.rm = TRUE)[1]), c(range(day3$lon, na.rm = TRUE)[2], range(day3$lat, na.rm = TRUE)[2])) / 1000  # in km
# # Yes they are the same
# 
# 
# # Do they look the same? --------------------------------------------------
# day2_thin <- day2[seq(1, nrow(day2), by = 10), ]  # every 10th row
# plot(day2_thin$utc_datetime, day2_thin$RawAX.cl, type = "l", col = "red",
#      xlab = "Time", ylab = "Raw accel",
#      ylim = range(c(day2_thin$RawAX.cl, day2_thin$RawAY.cl, day2_thin$RawAZ.cl), na.rm = TRUE))
# lines(day2_thin$utc_datetime, day2_thin$RawAY.cl, col = "green")
# lines(day2_thin$utc_datetime, day2_thin$RawAZ.cl, col = "blue")
# 
# day3_thin <- day3[seq(1, nrow(day3), by = 10), ]  # every 10th row
# plot(day3_thin$utc_datetime, day3_thin$RawAX.cl, type = "l", col = "red",
#      xlab = "Time", ylab = "Raw accel",
#      ylim = range(c(day3_thin$RawAX.cl, day3_thin$RawAY.cl, day3_thin$RawAZ.cl), na.rm = TRUE))
# lines(day3_thin$utc_datetime, day3_thin$RawAY.cl, col = "green")
# lines(day3_thin$utc_datetime, day3_thin$RawAZ.cl, col = "blue")
# 
# 
# 
# 
# 
