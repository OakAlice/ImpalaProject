# Perform the dead reckoning per day --------------------------------------
# Now we run the adjustments ----------------------------------------------

for (day in all_days){
  # day <- all_days[11]
  
  load(day) # this will come in as accel_data
  day_data <- accel_data
  
  # define and select the start times (which we recorded when we were running the tests)
  day_data <- day_data[gps_time_est >= start_time]
  if(nrow(day_data)==0){ # if this was before deployment, then just delete it
    print("this was before deployment began, skipping")
    next
  }
  
  # smooth the data
  day_data <- smooth_data(day_data, acc_cols, mag_cols)
  # additionally, calculate the VDBA from the smoothed (static) acceleration
  day_data <- calculate_VDBA(day_data, acc_cols)
  
  # encode that this is not the calibration period
  day_data$ME <- 0
  
  # now combine it with the calibration data and clean up
  all_data <- rbind(cal_data, day_data, fill = TRUE)
  keep_cols <- c("gps_time_est", 
                 "Q9_1", "Q9_2", "Q9_3",
                 "HeadAcc", "RawAX", "RawAY", "RawAZ", "RawAX.sm", "RawAY.sm", "RawAZ.sm",
                 "RawMX", "RawMY", "RawMZ", "RawMX.sm", "RawMY.sm", "RawMZ.sm",
                 "RawGX", "RawGY", "RawGZ",
                 "VDBA", "VDBA.sm",
                 "gps_lon", "gps_lat",
                 "ME")
  all_data <- all_data[, ..keep_cols]

  # prepare to feed into the function
  pitch <- round(orientation_info$mean[orientation_info$series == "pitch"],1)
  roll <- round(orientation_info$mean[orientation_info$series == "roll"],1)
  yaw <- round(orientation_info$mean[orientation_info$series == "yaw"],1)
  
  alldata_rotated <- with(all_data, Gundog.Compass(mag.x = RawMX.sm, mag.y = RawMY.sm, mag.z = RawMZ.sm,
                                                  acc.x = RawAX.sm, acc.y = RawAY.sm, acc.z = RawAZ.sm,
                                                  ME = ME,
                                                  acc.ref.frame = orientation_frame, 
                                                  positive.g = "up", 
                                                  mag.ref.frame = orientation_frame,
                                                  pitch.offset = pitch, roll.offset = roll, yaw.offset = yaw,
                                                  method = 3,
                                                  algorithm = "SAAM",
                                                  plot = TRUE))
  
  # Remove the calibration data and now you have your corrected trial data.
  setDT(alldata_rotated)
  all_data <- cbind(all_data, alldata_rotated[, c("Pitch", "Roll", "Yaw")])
  correcteddata <- all_data %>% dplyr::filter(ME != "M")
  
  # projected_path <- with(correcteddata, Gundog.Tracks(TS = gps_time_est, h = Yaw, v = VDBA.sm,
  #                                                         method = NULL,
  #                                                         plot = TRUE))
  
  first_lo <- na.omit(correcteddata$lon)[1]
  first_lat <- na.omit(correcteddata$lat)[1]
  
  projected_path2 = with(correcteddata, Gundog.Tracks(TS = gps_time_est, h = Yaw, v = VDBA.sm, 
                                                          lo = first_lo,
                                                          la = first_lat,
                                                          VP.lon = lon, 
                                                          VP.lat = lat,
                                                          method = "All",
                                                          plot = TRUE,
                                                          bound = FALSE))
  
  # Plotting the GPS alone --------------------------------------------------
  # it can be very hard to tell whether an analysis worked (squiggly lines look squiggly)
  # but we can plot the GPS independently to see what we might roughly get
  gps_data <- day_data[!is.na(lon)]
  ggplot(gps_data, aes(x = lon, y = lat)) + geom_path()
  
}

