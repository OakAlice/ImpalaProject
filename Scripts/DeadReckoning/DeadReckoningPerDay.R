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
# smooth and sd the VDBA                   
accel_data$VDBA.sm <- rollapply(accel_data$VDBA, width=50, FUN=mean, align="center", fill="extend")  # 1 s sm
accel_data$VDBA.sd <- rollapply(accel_data$VDBA.sm, width=250, FUN=sd, align="center", fill="extend") # over 5 sec

# Orientation and head movement -----------------------------------------------
# find whenever it is sleepinng and tag as 0 ME. 1 for movement, 0 for non-movement
accel_data$ME <- ifelse(accel_data$VDBA.sd < 0.005, 0, 1)

# ggplot(accel_data[1000000:1100000,], aes(x = utc_datetime, colour = ME)) + 
#   geom_path(aes(y = RawAX.cl)) + 
#   geom_path(aes(y = VDBA.sm))

# thresholds to estimate whether the head was up or down... commented out script was for playing arounf 
# rstudioapi::navigateToFile(file = file.path(base_path, "Scripts", "DeadReckoning", "DetermineHeadOrientation.R"))
# meanAX <- rollapply(accel_data$RawAX.cl, width=50, FUN=mean, align="center", fill="extend")
# meanAY <- rollapply(accel_data$RawAY.cl, width=50, FUN=mean, align="center", fill="extend")
# meanAZ <- rollapply(accel_data$RawAY.cl, width=50, FUN=mean, align="center", fill="extend")
# accel_data$headpos <- ifelse(meanAY > meanAX, "2", "3") # 2 for head up and 3 for head down
# # combine those two bits of information
# accel_data$ME <- ifelse(accel_data$ME == 1, accel_data$headpos, 0)

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
# ggplot(accel_data[1:500000,], aes(x = rtc_datetime)) + geom_path(aes(y = RawAX.sm, colour = ME)) + geom_path(aes(y = RawAY.sm, colour = ME)) + geom_path(aes(y = RawAZ.sm, colour = ME))
# this is not that good at the moment, but does the job

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
               "RawAX.sm", "RawAY.sm", "RawAZ.sm",
               "RawMX.sm", "RawMY.sm", "RawMZ.sm",
               "RawGX", "RawGY", "RawGZ",
               "VDBA.sm",
               "lon.sm", "lat.sm",
               "ME")
accel_data <- accel_data[, ..keep_cols]

# Rotating and orienting the data -----------------------------------------
# this is where we use either the tilt-adjusted accelerometer method
# or the version that also includes a gyroscope
if(compass_method == "Gundog"){
  # Combine with calib data
  all_data <- rbind(cal_data, accel_data, fill = TRUE)
  
  # Accounting for multiple orientations ------------------------------------
  # See attached doc for information on how we determined these orientations...
  acc_orientation <- ifelse(head_up == TRUE, "NWU", "DWN") # for the two possible orientations
  mag_orientation <- ifelse(head_up == TRUE, "NED", "DES")
  gravity_direction <- "down"
  
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
  
} else if (compass_method == "Madgwick"){
  
  # find the date
  date <- str_split(tools::file_path_sans_ext(basename(day)), "_", simplify = TRUE)[3]
  # load in the data converted in python
  quaternions <- fread(file.path(chunked_dir_path, paste0(as.character(date), "_quaternions.csv")))
  
  correcteddata <- merge(accel_data, quaternions, by = "utc_datetime") # %>% rename(Yaw = Raw, Pitch = Ritch)
  
}

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

