# Perform the dead reckoning per day --------------------------------------
# Now we run the adjustments ----------------------------------------------

for (day in all_days){
  # day <- all_days[3]
  
  # Select the target data --------------------------------------------------
  load(day) # this will come in as accel_data
  
  # define and select the start times (which we recorded when we were running the tests)
  day_data <- accel_data #[gps_time_est >= start_time]
  if(nrow(day_data)==0){ # if this was before deployment, then just delete it
    print("this was before deployment began, skipping")
    next
  }
  
  #| When I get the following smoothing code to work, it will be moved over to the original processing
  # TODO: Move over to the original processing code.
  
  day_data <- day_data %>% arrange(rtc_datetime)
  
  # select a subset for debugging
  # day_data <- accel_data[1:500000,]
  #day_data <- day_data[rtc_datetime > as.POSIXct("2000-01-03 20:00:00", tz = "UTC") &
  #                   rtc_datetime < as.POSIXct("2000-01-03 23:00:00", tz = "UTC")]

  # Prepping the IMU ------------------------------------------------------
  # adjust the axes (forgot to do this in original code)
  day_data$RawMX <- day_data$RawMX * 0.15
  day_data$RawMY <- day_data$RawMY * 0.15
  day_data$RawMZ <- day_data$RawMZ * 0.15
  
  day_data$RawAX <- day_data$RawAX * 0.25
  day_data$RawAY <- day_data$RawAY * 0.25
  day_data$RawAZ <- day_data$RawAZ * 0.25
  
  # smoothing the spikes out of the IMU data using the median filter
  day_data$RawAX.med <- runmed(x = day_data$RawAX, k = 5)
  day_data$RawAY.med <- runmed(x = day_data$RawAY, k = 5)
  day_data$RawAZ.med <- runmed(x = day_data$RawAZ, k = 5)
  day_data$RawMX.med <- runmed(x = day_data$RawMX, k = 5)
  day_data$RawMY.med <- runmed(x = day_data$RawMY, k = 5)
  day_data$RawMZ.med <- runmed(x = day_data$RawMZ, k = 5)
  
  # remove other noise with the butterworth filter
  # Define filter parameters # I played around with these
  fs <- 50
  # determining the cutoff with the PSD
  # psd <- spectrum(day_data$RawAX, spans = c(5,5), taper = 0.1, 
  #                 plot = FALSE)
  # plot(psd$freq * fs, 10*log10(psd$spec), type = "l",
  #      xlab = "Frequency (Hz)", ylab = "Power (dB)",
  #      main = "Power Spectral Density — RawAX")
  # abline(v = 0.5, col = "steelblue", lty = 2)   # candidate cutoff
  cutoff <- 5
  order <- 4
  # Create Butterworth filter
  nyquist <- fs / 2
  W <- cutoff / nyquist 
  bf <- butter(order, W, type = "low")   # low-pass filter
  
  # Apply filter
  day_data$RawAX.butt <- filtfilt(bf, day_data$RawAX.med)
  day_data$RawAY.butt <- filtfilt(bf, day_data$RawAY.med)
  day_data$RawAZ.butt <- filtfilt(bf, day_data$RawAZ.med)
  day_data$RawMX.butt <- filtfilt(bf, day_data$RawMX.med)
  day_data$RawMY.butt <- filtfilt(bf, day_data$RawMY.med)
  day_data$RawMZ.butt <- filtfilt(bf, day_data$RawMZ.med)
  
  # p0 <- ggplot(day_data[1:10000,], aes(x = gps_time_est)) + geom_path(aes(y = RawAX, colour = "X")) + geom_path(aes(y = RawAY, colour = "Y")) + geom_path(aes(y = RawAZ, colour = "Z"))
  # p1 <- ggplot(day_data[1:10000,], aes(x = gps_time_est)) + geom_path(aes(y = RawAX.med, colour = "X")) + geom_path(aes(y = RawAY.med, colour = "Y")) + geom_path(aes(y = RawAZ.med, colour = "Z"))
  # p2 <- ggplot(day_data[1:10000,], aes(x = gps_time_est)) + geom_path(aes(y = RawAX.butt, colour = "X")) + geom_path(aes(y = RawAY.butt, colour = "Y")) + geom_path(aes(y = RawAZ.butt, colour = "Z"))
  # p0/p1/p2
  
  # clean up the columns
  # smooth the data # again, this will be moved later...
  for (col in c("RawAX.butt", "RawAY.butt", "RawAZ.butt")) {
    colname <- str_split(col, "\\.", simplify = TRUE)[1]
    day_data[[paste0(colname, ".sm")]] <- rollapply(day_data[[col]], width = 50, FUN = mean, align = "center", fill = "extend")
  }
  
  for (col in c("RawMX.butt", "RawMY.butt", "RawMZ.butt")) {
    colname <- str_split(col, "\\.", simplify = TRUE)[1]
    day_data[[paste0(colname, ".sm")]] <- rollapply(day_data[[col]], width = 20, FUN = mean, align = "center", fill = "extend")
  }
  
  # VDBA ------------------------------------------------------------------
  # additionally, calculate the VDBA from the smoothed (static) acceleration
  sm_cols <- paste0(acc_cols, ".sm")
  butt_cols <- paste0(acc_cols, ".butt")
  # calculate the Vectorial Dynamic Body Acceleration (and smoothed version, as well as the sd)
  day_data$VDBA <- sqrt((day_data[[butt_cols[1]]] - day_data[[sm_cols[1]]])^2 + 
                     (day_data[[butt_cols[2]]] - day_data[[sm_cols[1]]])^2 +
                     (day_data[[butt_cols[3]]] - day_data[[sm_cols[1]]])^2)                     
  day_data$VDBA.sm <- rollapply(day_data$VDBA, width=50, FUN=mean, align="center", fill="extend")  # 1 s sm
  day_data$VDBA.sd <- rollapply(day_data$VDBA.sm, width=250, FUN=sd, align="center", fill="extend") # over 5 sec
  
  # Movement and non-movement -----------------------------------------------
  # find whenever it is sleepinng and tag as 0 ME. 1 for movement, 0 for non-movement
  day_data$ME <- ifelse(day_data$VDBA.sd < 0.015, 0, 1)
  
  # and then see if there are multiple in a row
  # Apply mode for each little section
  day_data[, epoch := ceiling(.I / (fs * 60))]
  # day_data[, ME := roll_mode(ME), by = epoch]
  day_data[, epoch := NULL]

  
  #play <- day_data
  # day_data <- play
  
  
  # Cleaning up the GPS -----------------------------------------------------
  # now we need to account for GPS error by smoothing the locations
  # when the behaviours have been calculated, I can isolate walking periods to improve this section
  # for now we just have to make it work
  # if the ME is 0, then take the average of the GPS positions for that period
  day_data[, group_id := cumsum(ME != shift(ME, fill = ME[1])) + 1]
  
  # Average GPS positions when stationary (ME == 0), only where valid GPS exists
  averaged_locations <- day_data[ME == 0 & !is.na(lon),
                                 .(avg_lon = mean(lon, na.rm = TRUE),
                                   avg_lat = mean(lat, na.rm = TRUE)),
                                 by = group_id]
  
  # Merge averaged locations back in
  day_data <- merge(day_data, averaged_locations, by = "group_id", all.x = TRUE)
  setorder(day_data, rtc_datetime)
  
  # Null out avg positions where original GPS was NA
  day_data[, avg_lon := fifelse(is.na(lon), NA_real_, avg_lon)]
  day_data[, avg_lat := fifelse(is.na(lat), NA_real_, avg_lat)]
  
  # Stationary = averaged position, moving = raw GPS
  day_data[, lon_for_spline := fifelse(ME == 0, avg_lon, lon)]
  day_data[, lat_for_spline := fifelse(ME == 0, avg_lat, lat)]
  
  # One representative row per stationary group, all moving rows
  spline_input <- day_data[
    !is.na(lon_for_spline) & !is.na(lat_for_spline) & !is.na(rtc_datetime)
  ][,
    row_for_spline := fifelse(ME == 0, .I == .I[ceiling(.N / 2)], TRUE),
    by = group_id
  ][row_for_spline == TRUE][
    order(rtc_datetime)
  ]
  
  # Shared time reference
  t0 <- min(day_data$rtc_datetime, na.rm = TRUE)
  
  # Recalculate t_sec AFTER filtering spline_input
  spline_input[, t_sec := as.numeric(difftime(rtc_datetime, t0, units = "secs"))]
  
  cat("Spline t_sec range:", range(spline_input$t_sec), "\n")
  
  # Refit splines
  lon.spline <- smooth.spline(spline_input$t_sec, spline_input$lon_for_spline, spar = 0.3)
  lat.spline <- smooth.spline(spline_input$t_sec, spline_input$lat_for_spline, spar = 0.3)
  
  # gps_data using same t0
  gps_data <- day_data[!is.na(lon_for_spline) & !is.na(lat_for_spline)]
  gps_data[, t_sec := as.numeric(difftime(rtc_datetime, t0, units = "secs"))]
  
  cat("gps_data t_sec range:", range(gps_data$t_sec), "\n")
  
  gps_data[, lon.sm := predict(lon.spline, t_sec)$y]
  gps_data[, lat.sm := predict(lat.spline, t_sec)$y]
  
  gps_data[, lon.sm := fifelse(ME == 0, avg_lon, lon.sm)]
  gps_data[, lat.sm := fifelse(ME == 0, avg_lat, lat.sm)]
  
  # Merge back into day_data
  day_data <- merge(day_data, gps_data[, .(rtc_datetime, lon.sm, lat.sm)],
                    by = "rtc_datetime", all.x = TRUE)
  setorder(day_data, rtc_datetime)
  
  # use t_sec throughout (renamed from t_sec_spline)
  # par(mfrow = c(1, 2))
  # 
  # plot(spline_input$t_sec, spline_input$lon_for_spline,
  #      pch = 19, cex = 0.5, col = "steelblue",
  #      xlab = "Time (seconds)", ylab = "Longitude",
  #      main = "Longitude spline fit")
  # lines(lon.spline, col = "red", lwd = 2)
  # 
  # plot(spline_input$t_sec, spline_input$lat_for_spline,
  #      pch = 19, cex = 0.5, col = "steelblue",
  #      xlab = "Time (seconds)", ylab = "Latitude",
  #      main = "Latitude spline fit")
  # lines(lat.spline, col = "red", lwd = 2)
  # 
  # par(mfrow = c(1, 1))
  # 
  # # Longitude over time
  # p1 <- ggplot(day_data, aes(x = gps_time_est)) +
  #   geom_point(aes(y = lon, colour = "original")) +
  #   geom_point(aes(y = lon_for_spline, colour = "spline input"), size = 3) +
  #   geom_point(aes(y = lon.sm, colour = "smoothed")) +
  #   labs(x = "Time", y = "Longitude", colour = NULL) +
  #   theme_minimal()
  # p2 <- ggplot(day_data, aes(x = gps_time_est)) +
  #   geom_point(aes(y = lat, colour = "original")) +
  #   geom_point(aes(y = lat_for_spline, colour = "spline input"), size = 3) +
  #   geom_point(aes(y = lat.sm, colour = "smoothed")) +
  #   labs(x = "Time", y = "Latitude", colour = NULL) +
  #   theme_minimal()
  # p1 + p2
  # 
  # # Map view
  # ggplot(day_data[!is.na(lon)]) +
  #   geom_path(aes(x = lon, y = lat), colour = "grey60") +
  #   geom_point(aes(x = lon_for_spline, y = lat_for_spline), 
  #              colour = "green", size = 2) +
  #   geom_path(aes(x = lon.sm, y = lat.sm), 
  #             colour = "red", alpha = 0.6, linewidth = 1) +
  #   geom_point(aes(x = lon.sm, y = lat.sm, colour = ME), size = 2) +
  #   labs(x = "Longitude", y = "Latitude") +
  #   theme_minimal()
  # 
  # 
  # day_data[,c("avg_lon.x", "avg_lat.x", "lon_for_spline", "lat_for_spline",
  # "lon.sm.x", "lat.sm.x", "avg_lon.y","avg_lat.y", "avg_lon", "avg_lat", "lon.sm.y", "lat.sm.y", "lon.sm", "lat.sm"):= NULL]
  

  
  # Combine with calib data -------------------------------------------------
  # prepare the calibration data
  cal_data <- fread(file.path(path_to_data, "calibration_data.csv"))
  cal_data$RawAX.sm <- cal_data$RawAX.sm * 0.25
  cal_data$RawAY.sm <- cal_data$RawAY.sm * 0.25
  cal_data$RawAZ.sm <- cal_data$RawAZ.sm * 0.25
  
  
  
  
  
  
  
  
  acc_orientation <- "NWU"
  mag_orientation <- "NED"
  gravity_direction <- "down"
  pitch <- -48
  
  
  
  
  # now combine it with the calibration data and clean up
  all_data <- rbind(cal_data, day_data, fill = TRUE)
  keep_cols <- c("gps_time_est", 
                 "Q9_1", "Q9_2", "Q9_3",
                 "HeadAcc", "RawAX.sm", "RawAY.sm", "RawAZ.sm",
                 "RawMX.sm", "RawMY.sm", "RawMZ.sm",
                 "RawGX", "RawGY", "RawGZ",
                 "VDBA.sm",
                 "lon.sm", "lat.sm",
                 "ME")
  
  all_data <- all_data[, ..keep_cols]
   
   
  

  # Orientatiuons -----------------------------------------------------------
  # move this out of the script later
  
  
  
  # prepare to feed into the function
  # pitch <- round(orientation_info$mean[orientation_info$series == "pitch"],1)
  
  alldata_rotated <- with(all_data, Gundog.Compass(mag.x = RawMX.sm, mag.y = RawMY.sm, mag.z = RawMZ.sm,
                                                  acc.x = RawAX.sm, acc.y = RawAY.sm, acc.z = RawAZ.sm,
                                                  ME = ME,
                                                  acc.ref.frame = acc_orientation, 
                                                  positive.g = gravity_direction, 
                                                  mag.ref.frame = mag_orientation,
                                                  pitch.offset = pitch, roll.offset = 0, yaw.offset = 0,
                                                  method = 3,
                                                  algorithm = "SAAM",
                                                  plot = FALSE))
  
  # Remove the calibration data and now you have your corrected trial data.
  setDT(alldata_rotated)
  all_data <- cbind(all_data, alldata_rotated[, c("Pitch", "Roll", "Yaw")])
  correcteddata <- all_data %>% dplyr::filter(ME != "M")
  
  # projected_path <- with(correcteddata, Gundog.Tracks(TS = gps_time_est, h = Yaw, v = VDBA.sm,
  #                                                     ME = ME,
  #                                                     method = NULL,
  #                                                     plot = TRUE))
  # 
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
  
  # Plotting the GPS alone --------------------------------------------------
  # it can be very hard to tell whether an analysis worked (squiggly lines look squiggly)
  # but we can plot the GPS independently to see what we might roughly get
  gps_data <- day_data[!is.na(lon.sm)]
  ggplot(gps_data) +
    #geom_path(aes(x = lon, y = lat, colour = gps_time_est)) +
    geom_path(aes(x = lon.sm, y = lat.sm, colour = gps_time_est), size = 2) +
    theme_minimal()
  
}


