#################
# DeadReckoningPerDay

# Overview:
# Take each of the days of data and perform the calibration/reorientation as well as dead reckoning
# Save the corrected DR paths for analysis

# Requires:
# time-corrected, scaled IMU data and gps in day chunks

#################

# Select the target data --------------------------------------------------
 # this will come in as accel_data
accel_data <- fread(day)
accel_data <- accel_data %>% arrange(utc_datetime)

# define and select the start times
accel_data <- accel_data[utc_datetime >= start_time]
if(nrow(accel_data)==0){ # if this was before deployment, then just delete it
  print("this was before deployment began, skipping")
  next
}
  
# Moving vs not moving ----------------------------------------------------
## Smooth
accel_data$RawAX.sm <- rollapply(accel_data$RawAX.cl, width=50, FUN=mean, align="center", fill="extend")
accel_data$RawAY.sm <- rollapply(accel_data$RawAY.cl, width=50, FUN=mean, align="center", fill="extend")
accel_data$RawAZ.sm <- rollapply(accel_data$RawAZ.cl, width=50, FUN=mean, align="center", fill="extend")
accel_data$RawMX.sm <- rollapply(accel_data$RawMX.cl, width=50, FUN=mean, align="center", fill="extend")
accel_data$RawMY.sm <- rollapply(accel_data$RawMY.cl, width=50, FUN=mean, align="center", fill="extend")
accel_data$RawMZ.sm <- rollapply(accel_data$RawMZ.cl, width=50, FUN=mean, align="center", fill="extend")
# calculate the VDBA
butt_cols <- paste0(base_acc, ".cl")
sm_cols <- paste0(base_acc, ".sm")
# calculate the Vectorial Dynamic Body Acceleration (and smoothed version, as well as the sd)
accel_data$VDBA <- sqrt((accel_data[[butt_cols[1]]] - accel_data[[sm_cols[1]]])^2 + 
                          (accel_data[[butt_cols[2]]] - accel_data[[sm_cols[1]]])^2 +
                          (accel_data[[butt_cols[3]]] - accel_data[[sm_cols[1]]])^2)  

# smooth and sd the VDBA                   
accel_data$VDBA.sm <- rollapply(accel_data$VDBA, width=50, FUN=mean, align="center", fill="extend")  # 1 s sm
accel_data$VDBA.sd <- rollapply(accel_data$VDBA.sm, width=250, FUN=sd, align="center", fill="extend") # over 5 sec

# find whenever it is sleepinng and tag as 0 ME. 1 for movement, 0 for non-movement
accel_data$RawAY.sd <- rollapply(accel_data$RawAY.cl, width=250, FUN=sd, align="center", fill="extend")
accel_data$ME <- ifelse(accel_data$RawAY.sd < 0.05, 0, 1) ## NOTE, play with this number and which variable to use

# and then smooth these so they're not rapidly flickering 
# Apply mode for each little section
fs <- 50 # in case not already defined
roll_mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
accel_data[, epoch := ceiling(.I / (fs * 60))]
accel_data[, ME := roll_mode(ME), by = epoch]
accel_data[, epoch := NULL]
accel_data[, group_id := cumsum(ME != shift(ME, fill = ME[1])) + 1]

# check what it looks like
# ggplot(accel_data[1:500000,], aes(x = rtc_datetime)) + 
#   geom_path(aes(y = VDBA.sm, colour = ME)) +
#   geom_path(aes(y = RawAX.sm), colour = "red") + 
#   geom_path(aes(y = RawAY.sm), colour = "blue") + 
#   geom_path(aes(y = RawAZ.sm), colour = "green")

# Cleaning up the GPS -----------------------------------------------------
# now we need to account for GPS error by smoothing the locations
# when the behaviours have been calculated, I can isolate walking periods to improve this section
# for now, if the ME is 0, then take the average of the GPS positions for that period
gps_data <- accel_data[!is.na(accel_data$lon), ] %>%
  select(utc_datetime, group_id, lon, lat, ME)
gps <- smooth_the_gps(gps_data, movement_column = "ME", no_movement = 0)
gps_data <- gps$gps_data
gps$plot
# merge back into the accelerometer
accel_data <- merge(accel_data, gps_data,
                    by = "utc_datetime", all.x = TRUE)
setorder(accel_data, rtc_datetime)

# clean up
keep_cols <- c("utc_datetime",
               "Q9_1", "Q9_2", "Q9_3",
               "RawAX.sm", "RawAY.sm", "RawAZ.sm",
               "RawMX.sm", "RawMY.sm", "RawMZ.sm",
               "RawGX", "RawGY", "RawGZ",
               "VDBA.sm",
               "lon.sm", "lat.sm",
               "ME")
accel_data <- accel_data[, ..keep_cols]
# make the quaternions numeric and remove the non-numeric characters
accel_data[, c("Q9_1", "Q9_2", "Q9_3") := lapply(
  .SD,
  function(x) suppressWarnings(as.numeric(iconv(x, from = "", to = "UTF-8", sub = NA)))
),
.SDcols = c("Q9_1", "Q9_2", "Q9_3")]


# Rotating and orienting the data -----------------------------------------
# remove the nas from the accel data
accel_data <- accel_data[complete.cases(accel_data[, c("RawAX.sm", "RawMX.sm")]), ]

# Combine with calib data
cal_data <- fread(file.path(collar_dir, "calibration_data.csv"))
cal_data$RawAX.sm <- rollapply(cal_data$RawAX.cl, width=50, FUN=mean, align="center", fill="extend")
cal_data$RawAY.sm <- rollapply(cal_data$RawAY.cl, width=50, FUN=mean, align="center", fill="extend")
cal_data$RawAZ.sm <- rollapply(cal_data$RawAZ.cl, width=50, FUN=mean, align="center", fill="extend")
cal_data$RawMX.sm <- rollapply(cal_data$RawMX.cl, width=50, FUN=mean, align="center", fill="extend")
cal_data$RawMY.sm <- rollapply(cal_data$RawMY.cl, width=50, FUN=mean, align="center", fill="extend")
cal_data$RawMZ.sm <- rollapply(cal_data$RawMZ.cl, width=50, FUN=mean, align="center", fill="extend")
# calculate the VDBA
# calculate the Vectorial Dynamic Body Acceleration (and smoothed version, as well as the sd)
cal_data$VDBA <- sqrt((cal_data$RawAX.cl - cal_data$RawAX.sm)^2 + 
                          (cal_data$RawAY.cl - cal_data$RawAY.sm)^2 +
                          (cal_data$RawAZ.cl - cal_data$RawAZ.sm)^2 ) 
cal_data$VDBA.sm <- rollapply(cal_data$VDBA, width=50, FUN=mean, align="center", fill="extend")
cols <- intersect(keep_cols, names(cal_data))
cal_data <- cal_data[, ..cols]
all_data <- rbind(cal_data, accel_data, fill = TRUE)

# Accounting for multiple orientations ------------------------------------
# See attached doc for information on how we determined these orientations...
acc_orientation <- "NWU" #ifelse(head_up == TRUE, "NWU", "DWN") # for the two possible orientations
mag_orientation <- "NED" #ifelse(head_up == TRUE, "NED", "DES")
gravity_direction <- "down"

# pitch was determined from extracting known walking events and then taking the mean of axes during those times
pitch <- atan2(-(-0.253599267), sqrt(0.259529436^2 + 0.891019352^2))
pitch_deg <- pitch * 180 / pi

# save_data <- all_data

# prepare to feed into the function ----------------------------------------
# save_data <- all_data
all_data[, c("Q9_1", "Q9_2", "Q9_3") := NULL,]
# remultiply them
all_data[, c("RawMX.sm", "RawMY.sm", "RawMZ.sm") :=
           lapply(.SD, `*`, 2048),
         .SDcols = c("RawMX.sm", "RawMY.sm", "RawMZ.sm")]
alldata_rotated <- with(all_data, Gundog.Compass(mag.x = RawMX.sm, mag.y = RawMY.sm, mag.z = RawMZ.sm,
                                                 acc.x = RawAX.sm, acc.y = RawAY.sm, acc.z = RawAZ.sm,
                                                 ME = ME,
                                                 acc.ref.frame = acc_orientation, 
                                                 positive.g = gravity_direction, 
                                                 mag.ref.frame = mag_orientation,
                                                 pitch.offset = -pitch_deg, 
                                                 roll.offset = 0,
                                                 yaw.offset = 0,
                                                 method = 2,
                                                 algorithm = "standard",
                                                 plot = TRUE))

# Remove the calibration data and now you have your corrected trial data.
setDT(alldata_rotated)
correcteddata <- cbind(all_data, alldata_rotated[, c("Pitch", "Roll", "Yaw")])
correcteddata <- correcteddata %>% dplyr::filter(ME != "M")

# projected_path <- with(correcteddata, Gundog.Tracks(TS = utc_datetime, h = Yaw, v = VDBA.sm,
#                                                     ME = ME,
#                                                     method = NULL,
#                                                     plot = TRUE))

# and then use the gps to do VPC
first_lo <- na.omit(correcteddata$lon.sm)[1]
first_lat <- na.omit(correcteddata$lat.sm)[1]

projected_path2 = with(correcteddata, Gundog.Tracks(TS = utc_datetime, h = Yaw, v = VDBA.sm, 
                                                    ME = ME,
                                                    lo = first_lo,
                                                    la = first_lat,
                                                    VP.lon = lon.sm, 
                                                    VP.lat = lat.sm,
                                                    method = "All",
                                                    plot = TRUE,
                                                    bound = FALSE))
