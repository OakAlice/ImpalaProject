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
  
  
  ## prepare the dead reckoning data -----------------------------------
  acc_cols <- c("RawAX", "RawAY", "RawAZ")
  mag_cols <- c("RawMX", "RawMY", "RawMZ")
  for (col in c(acc_cols, mag_cols))
    day_data[[paste0(col, ".sm")]] <- rollapply(day_data[[col]], width = 50, FUN = mean, align = "center", fill = "extend")
  
  sm_cols <- paste0(acc_cols, ".sm")
  day_data$VDBA    <- sqrt((day_data[[acc_cols[1]]] - day_data[[sm_cols[1]]])^2 +
                          (day_data[[acc_cols[2]]] - day_data[[sm_cols[2]]])^2 +
                          (day_data[[acc_cols[3]]] - day_data[[sm_cols[3]]])^2)
  day_data$VDBA.sm <- rollapply(day_data$VDBA, width = 50, FUN = mean, align = "center", fill = "extend")
  
  # define that this is not the calibration data
  day_data$ME   <- 0
  
  ## prepare the calibration data --------------------------------------
  for (col in c(acc_cols, mag_cols))
    cal_data[[paste0(col, ".sm")]] <- rollapply(cal_data[[col]], width = 50, FUN = mean, align = "center", fill = "extend")
  cal_data$ME      <- "M"
  
  ## Calculate the angle the device might be on -------------------------------
  # because the device is not perfepctly flat this time, we need to account for the angles
  orientation <- check_orientation(day_data, acc_cols, mag_cols)
  # orientation$accel_graph
  # orientation$mag_graph
  orientation$orientation_table
  
  pitch <- compute_pitch(orientation$orientation_table[1,]$mean, 
                         orientation$orientation_table[2,]$mean, 
                         orientation$orientation_table[3,]$mean)
  
  ## 7. COMBINE CALIBRATION + TEST DATA --------------------------------------
  keep_cols <- c("RawAX.sm", "RawAY.sm", "RawAZ.sm", "RawMX.sm", "RawMY.sm", "RawMZ.sm", "ME")
  foo       <- rbind(
    cal_data[,      ..keep_cols],
    day_data[, ..keep_cols]
  )
  
  ## 8. RUN GUNDOG.COMPASS ---------------------------------------
  # NOTE: Jojo found mag axes differ from accel axes
  # which is something we had to consider too.
  foo.ang <- with(foo,
                  Gundog.Compass(
                    mag.x = RawMX.sm, mag.y = RawMY.sm, mag.z = RawMZ.sm,
                    acc.x = RawAX.sm, acc.y = RawAY.sm, acc.z = RawAZ.sm,
                    ME            = ME,
                    acc.ref.frame = "DEN",
                    positive.g    = "up",
                    mag.ref.frame = "NWU",
                    pitch.offset  = pitch, roll.offset = 0, yaw.offset = 0,
                    method        = 3,
                    algorithm     = "SAAM",
                    plot          = TRUE
                  )
  )
  
  ## get the corrected data ------------------------------
  foo.ang.test <- foo.ang[foo.ang$ME != "M", c("Roll", "Pitch", "Yaw")]
  day_data  <- cbind(day_data, foo.ang.test)
  
  # Inspect angles
  # plot1 <- ggplot(day_data, aes(x = rtc_datetime)) +
  #   geom_line(aes(y = Pitch, color = "Pitch")) +
  #   geom_line(aes(y = Roll,  color = "Roll"))  +
  #   scale_color_manual(values = c("Pitch" = "red", "Roll" = "green")) +
  #   labs(y = "Angle (degrees)", color = "Body Angle") +
  #   ylim(-180, 180) + theme_minimal()
  # 
  # plot2 <- ggplot(day_data, aes(x = rtc_datetime, y = Yaw)) +
  #   geom_line(color = "blue") +
  #   labs(y = "Heading (degrees)", x = "Time") +
  #   ylim(0, 360) + theme_minimal()
  # 
  # grid.arrange(plot1, plot2, ncol = 1)
  
  ## 10. DEAD RECKONING (no GPS correction) ----------------------
  day_data.dr <- with(day_data,
                   Gundog.Tracks(
                     TS     = rtc_datetime,
                     h      = Yaw,
                     v      = VDBA.sm,
                     method = NULL,
                     plot   = TRUE
                   )
  )
  
  ## 11. DEAD RECKONING (GPS-corrected) --------------------------
  first_lon <- head(day_data$lon[day_data$lon != 0 & !is.na(day_data$lon)], 1)
  first_lat <- head(day_data$lat[day_data$lat != 0 & !is.na(day_data$lat)], 1)
  
  day_data.dr.gps <- with(day_data,
                       Gundog.Tracks(
                         TS      = rtc_datetime,
                         h       = Yaw,
                         v       = VDBA.sm,
                         lo      = first_lon,
                         la      = first_lat,
                         VP.lon  = lon,
                         VP.lat  = lat,
                         method  = "All",
                         plot    = TRUE,
                         bound   = FALSE
                       )
  )
  
  
  # Plotting the GPS alone --------------------------------------------------
  # it can be very hard to tell whether an analysis worked (squiggly lines look squiggly)
  # but we can plot the GPS independently to see what we might roughly get
  gps_data <- day_data[!is.na(lon)]
  ggplot(gps_data, aes(x = lon, y = lat)) + geom_path()
  
}

