#################
# DeadReckoningPerDay

# Overview:
# Take each of the days of data and perform the calibration/reorientation as well as dead reckoning
# Save the corrected DR paths for analysis

# Requires:
# time-corrected, scaled IMU data and gps in day chunks

#################

# Select the target data --------------------------------------------------
load(day) # this will come in as accel_data
  
# define and select the start times
accel_data <- accel_data[gps_time_est >= start_time]
if(nrow(accel_data)==0){ # if this was before deployment, then just delete it
  print("this was before deployment began, skipping")
  next
}
  
accel_data <- accel_data %>% arrange(rtc_datetime)
  
# Prepping the IMU ------------------------------------------------------
# the same as what we did for the calibration
accel_data <- smooth_and_filter(data = accel_data, k = 5, fs = 50, bw_cutoff = 5, bw_order = 4)

# VDBA ------------------------------------------------------------------
# additionally, calculate the VDBA from the smoothed (static) acceleration
acc_cols <- c("RawAX", "RawAY", "RawAZ")
butt_cols <- paste0(acc_cols, ".butt")
sm_cols <- paste0(acc_cols, ".sm")
# calculate the Vectorial Dynamic Body Acceleration (and smoothed version, as well as the sd)
accel_data$VDBA <- sqrt((accel_data[[butt_cols[1]]] - accel_data[[sm_cols[1]]])^2 + 
                   (accel_data[[butt_cols[2]]] - accel_data[[sm_cols[1]]])^2 +
                   (accel_data[[butt_cols[3]]] - accel_data[[sm_cols[1]]])^2)                     
accel_data$VDBA.sm <- rollapply(accel_data$VDBA, width=50, FUN=mean, align="center", fill="extend")  # 1 s sm
accel_data$VDBA.sd <- rollapply(accel_data$VDBA.sm, width=250, FUN=sd, align="center", fill="extend") # over 5 sec

# Movement and non-movement -----------------------------------------------
# find whenever it is sleepinng and tag as 0 ME. 1 for movement, 0 for non-movement
# when I have finished the behavioural prediction analysis, I will be able to be more refined here
accel_data$ME <- ifelse(accel_data$VDBA.sd < 0.005, 0, 1)

# and then see if there are multiple in a row
# Apply mode for each little section
fs <- 50 # in case not already defined
roll_mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
accel_data[, epoch := ceiling(.I / (fs * 60))]
accel_data[, ME := roll_mode(ME), by = epoch]
accel_data[, epoch := NULL]

# check what it looks like
# ggplot(accel_data[1:500000,], aes(x = rtc_datetime)) + geom_path(aes(y = RawAX.sm, colour = ME)) + geom_path(aes(y = RawAY.sm, colour = ME)) + geom_path(aes(y = RawAZ.sm, colour = ME))
# this is not that good at the moment, but does the job

# Cleaning up the GPS -----------------------------------------------------
# now we need to account for GPS error by smoothing the locations
# when the behaviours have been calculated, I can isolate walking periods to improve this section
# for now, if the ME is 0, then take the average of the GPS positions for that period
accel_data[, group_id := cumsum(ME != shift(ME, fill = ME[1])) + 1]

# Average GPS positions when stationary (ME == 0), only where valid GPS exists
averaged_locations <- accel_data[ME == 0 & !is.na(lon),
                               .(avg_lon = mean(lon, na.rm = TRUE),
                                 avg_lat = mean(lat, na.rm = TRUE)),
                               by = group_id]

# Merge averaged locations back in
accel_data <- merge(accel_data, averaged_locations, by = "group_id", all.x = TRUE)
setorder(accel_data, rtc_datetime)

# Null out avg positions where original GPS was NA
accel_data[, avg_lon := fifelse(is.na(lon), NA_real_, avg_lon)]
accel_data[, avg_lat := fifelse(is.na(lat), NA_real_, avg_lat)]

# Stationary = averaged position, moving = raw GPS
accel_data[, lon_for_spline := fifelse(ME == 0, avg_lon, lon)]
accel_data[, lat_for_spline := fifelse(ME == 0, avg_lat, lat)]

# One representative row per stationary group, all moving rows
spline_input <- accel_data[
  !is.na(lon_for_spline) & !is.na(lat_for_spline) & !is.na(rtc_datetime)
][,
  row_for_spline := fifelse(ME == 0, .I == .I[ceiling(.N / 2)], TRUE),
  by = group_id
][row_for_spline == TRUE][
  order(rtc_datetime)
]

# Shared time reference
t0 <- min(accel_data$rtc_datetime, na.rm = TRUE)

# Recalculate t_sec AFTER filtering spline_input
spline_input[, t_sec := as.numeric(difftime(rtc_datetime, t0, units = "secs"))]

# Refit splines
lon.spline <- smooth.spline(spline_input$t_sec, spline_input$lon_for_spline, spar = 0.1)
lat.spline <- smooth.spline(spline_input$t_sec, spline_input$lat_for_spline, spar = 0.1)

# gps_data using same t0
gps_data <- accel_data[!is.na(lon_for_spline) & !is.na(lat_for_spline)]
gps_data[, t_sec := as.numeric(difftime(rtc_datetime, t0, units = "secs"))]

# predict the splines
gps_data[, lon.sm := predict(lon.spline, t_sec)$y]
gps_data[, lat.sm := predict(lat.spline, t_sec)$y]

# remove the predictions for when it was stationary
gps_data[, lon.sm := fifelse(ME == 0, avg_lon, lon.sm)]
gps_data[, lat.sm := fifelse(ME == 0, avg_lat, lat.sm)]

# Merge back into accel_data
accel_data <- merge(accel_data, gps_data[, .(rtc_datetime, lon.sm, lat.sm)],
                  by = "rtc_datetime", all.x = TRUE)
setorder(accel_data, rtc_datetime)

# plots to check
# # Longitude over time
# p1 <- ggplot(accel_data, aes(x = gps_time_est)) +
#   geom_point(aes(y = lon, colour = "original")) +
#   geom_point(aes(y = lon_for_spline, colour = "spline input"), size = 3) +
#   geom_point(aes(y = lon.sm, colour = "smoothed")) +
#   labs(x = "Time", y = "Longitude", colour = NULL) +
#   theme_minimal()
# p2 <- ggplot(accel_data, aes(x = gps_time_est)) +
#   geom_point(aes(y = lat, colour = "original")) +
#   geom_point(aes(y = lat_for_spline, colour = "spline input"), size = 3) +
#   geom_point(aes(y = lat.sm, colour = "smoothed")) +
#   labs(x = "Time", y = "Latitude", colour = NULL) +
#   theme_minimal()
# p1 + p2

# Map view
# ggplot(accel_data[!is.na(lon)]) +
#   geom_path(aes(x = lon, y = lat), colour = "grey60") +
#   geom_point(aes(x = lon_for_spline, y = lat_for_spline),
#              colour = "green", size = 2) +
#   geom_path(aes(x = lon.sm, y = lat.sm),
#             colour = "red", alpha = 0.6, linewidth = 1) +
#   geom_point(aes(x = lon.sm, y = lat.sm, colour = ME), size = 2) +
#   labs(x = "Longitude", y = "Latitude") +
#   theme_minimal()

# remove the cols before rerunning
# accel_data[,c("avg_lon.x", "avg_lat.x", "lon_for_spline", "lat_for_spline",
# "lon.sm.x", "lat.sm.x", "avg_lon.y","avg_lat.y", "avg_lon", "avg_lat", "lon.sm.y", "lat.sm.y", "lon.sm", "lat.sm"):= NULL]

# Combine with calib data -------------------------------------------------
# now combine it with the calibration data and clean up
all_data <- rbind(cal_data, accel_data, fill = TRUE)
keep_cols <- c("gps_time_est",
               "RawAX.sm", "RawAY.sm", "RawAZ.sm",
               "RawMX.sm", "RawMY.sm", "RawMZ.sm",
               "RawGX", "RawGY", "RawGZ",
               "VDBA.sm",
               "lon.sm", "lat.sm",
               "ME")
all_data <- all_data[, ..keep_cols]

# prepare to feed into the function ----------------------------------------
alldata_rotated <- with(all_data, Gundog.Compass(mag.x = RawMX.sm, mag.y = RawMY.sm, mag.z = RawMZ.sm,
                                                acc.x = RawAX.sm, acc.y = RawAY.sm, acc.z = RawAZ.sm,
                                                ME = ME,
                                                acc.ref.frame = acc_orientation, 
                                                positive.g = gravity_direction, 
                                                mag.ref.frame = mag_orientation,
                                                pitch.offset = -pitch_deg, 
                                                roll.offset = 0, # angles$roll_deg, # removed for now as seemed to make worse
                                                yaw.offset = 0,
                                                method = 2,
                                                algorithm = "standard",
                                                plot = TRUE))

# Remove the calibration data and now you have your corrected trial data.
setDT(alldata_rotated)
all_data <- cbind(all_data, alldata_rotated[, c("Pitch", "Roll", "Yaw")])
correcteddata <- all_data %>% dplyr::filter(ME != "M")

# projected_path <- with(correcteddata, Gundog.Tracks(TS = gps_time_est, h = Yaw, v = VDBA.sm,
#                                                     ME = ME,
#                                                     method = NULL,
#                                                     plot = TRUE))

# and then use the gps to do VPC
first_lo <- na.omit(correcteddata$lon.sm)[1]
first_lat <- na.omit(correcteddata$lat.sm)[1]

projected_path2 = with(correcteddata, Gundog.Tracks(TS = gps_time_est, h = Yaw, v = VDBA.sm, 
                                                    ME = ME,
                                                    lo = first_lo,
                                                    la = first_lat,
                                                    VP.lon = lon.sm, 
                                                    VP.lat = lat.sm,
                                                    method = "All",
                                                    plot = TRUE,
                                                    bound = FALSE))

