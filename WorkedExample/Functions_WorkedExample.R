
scale_variables <- function(data){
  # rescale each of the variables to be in the expected units
  # takes in raw measure,,ments
  # returns acc in Gs, mag in mT, and gyro in rads/sec
  
  data[, c("RawAX.sc", "RawAY.sc", "RawAZ.sc")] <- data[, c("RawAX", "RawAY", "RawAZ")] / 2048 # / 8192
  data[, c("RawMX.sc", "RawMY.sc", "RawMZ.sc")] <- data[, c("RawMX", "RawMY", "RawMZ")] / 2048 * 0.15
  data[, c("RawGX.sc", "RawGY.sc", "RawGZ.sc")] <- data[, c("RawGX", "RawGY", "RawGZ")] / 2^15 * 500 * 3.14159 / 180
  
  return(data)
}

clean_noise <- function(data, med_k = 5){

  setDT(data)
  
  base_cols <- c("RawAX", "RawAY", "RawAZ", "RawMX", "RawMY", "RawMZ")
  
  # Median smoothing
  for (col in base_cols) data[, (paste0(col, ".cl")) := runmed(get(paste0(col, ".sc")), k = med_k)]
  
  # Butterworth filter
  bw_cutoff = 5
  bw_order = 4
  fs = 50
  bf <- butter(bw_order, bw_cutoff/(fs/2), type = "low")
  
  for (col in base_cols)
    set(
      data,
      j = paste0(col, ".cl"),
      value = filtfilt(bf, data[[paste0(col, ".cl")]])
    )
  
  # Rolling mean: accelerometer
  for (col in base_cols[1:3]) {
    data[, (paste0(col, ".sm")) :=
           rollapply(get(paste0(col, ".cl")),
                     width = 50,
                     FUN = mean,
                     align = "center",
                     fill = "extend")]
  }
  
  # Rolling mean: magnetometer
  for (col in base_cols[4:6]) {
    data[, (paste0(col, ".sm")) :=
           rollapply(get(paste0(col, ".cl")),
                     width = 20,
                     FUN = mean,
                     align = "center",
                     fill = "extend")]
  }
  
  return(data)
}

activity_scoring <- function(data, threshold = 0.005){
  
  # calculate the Vectorial Dynamic Body Acceleration (and smoothed version, as well as the sd)
  data$VDBA <- sqrt((data$RawAX.cl - data$RawAX.sm)^2 + 
                      (data$RawAY.cl - data$RawAY.sm)^2 +
                      (data$RawAZ.cl - data$RawAZ.sm)^2)                     
  data$VDBA.sm <- rollapply(data$VDBA, width=50, FUN=mean, align="center", fill="extend")  # 1 s sm
  data$VDBA.sd <- rollapply(data$VDBA.sm, width=250, FUN=sd, align="center", fill="extend") # over 5 sec
  
  # find whenever it is sleepinng and tag as 0 ME. 1 for movement, 0 for non-movement
  # when I have finished the behavioural prediction analysis, I will be able to be more refined here
  data$ME <- ifelse(data$VDBA.sd < threshold, 0, 1)
  
  # and then see if there are multiple in a row (as in, only meaningful if it stops for a whole minute or more)
  # Apply mode for each little section
  fs <- 50 # in case not already defined
  roll_mode <- function(x) {
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
  }
  data[, epoch := ceiling(.I / (fs * 60))]
  data[, ME := roll_mode(ME), by = epoch]
  data[, epoch := NULL]
  data[, group_id := cumsum(ME != shift(ME, fill = ME[1])) + 1]
  
  # plot to see
  # ggplot(accel_data[1:1000000,], aes(x = utc_datetime)) +
  #   geom_path(aes(y = RawAX.sm , colour = as.factor(ME), group = 1))
  
  return(data)
}


smooth_the_gps <- function(gps_data, movement_column, no_movement, spar_setting = 0.1){
  
  # Average GPS positions when stationary (ME == 0), only where valid GPS exists
  averaged_locations <- gps_data[gps_data[[movement_column]] == no_movement & !is.na(lon),
                                 .(avg_lon = mean(lon, na.rm = TRUE),
                                   avg_lat = mean(lat, na.rm = TRUE)),
                                 by = group_id]
  
  # Merge averaged locations back in
  gps_data <- merge(gps_data, averaged_locations, by = "group_id", all.x = TRUE)
  setorder(gps_data, utc_datetime)
  
  # Null out avg positions where original GPS was NA
  gps_data[, avg_lon := fifelse(is.na(lon), NA_real_, avg_lon)]
  gps_data[, avg_lat := fifelse(is.na(lat), NA_real_, avg_lat)]
  
  # Stationary = averaged position, moving = raw GPS
  gps_data[, lon_for_spline := fifelse(gps_data[[movement_column]] == no_movement, avg_lon, lon)]
  gps_data[, lat_for_spline := fifelse(gps_data[[movement_column]] == no_movement, avg_lat, lat)]
  
  # One representative row per stationary group, all moving rows
  spline_input <- gps_data[
    !is.na(lon_for_spline) & !is.na(lat_for_spline) & !is.na(utc_datetime)
  ][,
    row_for_spline := fifelse(.SD[[movement_column]] == no_movement, .I == .I[ceiling(.N / 2)], TRUE),
    by = group_id,
    .SDcols = movement_column
  ][row_for_spline == TRUE][
    order(utc_datetime)
  ]
  
  # Shared time reference
  t0 <- min(gps_data$utc_datetime, na.rm = TRUE)
  
  # Recalculate t_sec AFTER filtering spline_input
  spline_input[, t_sec := as.numeric(difftime(utc_datetime, t0, units = "secs"))]
  
  # Refit splines
  lon.spline <- smooth.spline(spline_input$t_sec, spline_input$lon_for_spline, spar = spar_setting)
  lat.spline <- smooth.spline(spline_input$t_sec, spline_input$lat_for_spline, spar = spar_setting)
  
  # gps_data using same t0
  gps_data <- gps_data[!is.na(lon_for_spline) & !is.na(lat_for_spline)]
  gps_data[, t_sec := as.numeric(difftime(utc_datetime, t0, units = "secs"))]
  
  # predict the splines
  gps_data[, lon.sm := predict(lon.spline, t_sec)$y]
  gps_data[, lat.sm := predict(lat.spline, t_sec)$y]
  
  # remove the predictions for when it was stationary
  gps_data[, lon.sm := fifelse(ME == 0, avg_lon, lon.sm)]
  gps_data[, lat.sm := fifelse(ME == 0, avg_lat, lat.sm)]
  
  # plots to check
  # # Longitude over time
  # p1 <- ggplot(accel_data, aes(x = utc_datetime)) +
  #   geom_point(aes(y = lon, colour = "original")) +
  #   geom_point(aes(y = lon_for_spline, colour = "spline input"), size = 3) +
  #   geom_point(aes(y = lon.sm, colour = "smoothed")) +
  #   labs(x = "Time", y = "Longitude", colour = NULL) +
  #   theme_minimal()
  # p2 <- ggplot(accel_data, aes(x = utc_datetime)) +
  #   geom_point(aes(y = lat, colour = "original")) +
  #   geom_point(aes(y = lat_for_spline, colour = "spline input"), size = 3) +
  #   geom_point(aes(y = lat.sm, colour = "smoothed")) +
  #   labs(x = "Time", y = "Latitude", colour = NULL) +
  #   theme_minimal()
  # p1 + p2
  
  # Map view
  plot <- ggplot(gps_data[!is.na(lon)]) +
    geom_path(aes(x = lon, y = lat), colour = "grey60") +
    geom_point(aes(x = lon_for_spline, y = lat_for_spline),
               colour = "green", size = 2) +
    geom_path(aes(x = lon.sm, y = lat.sm),
              colour = "red", alpha = 0.6, linewidth = 1) +
    geom_point(aes(x = lon.sm, y = lat.sm, colour = ME), size = 2) +
    labs(x = "Longitude", y = "Latitude") +
    theme_minimal()
  
  # remove the cols before rerunning
  # accel_data[,c("avg_lon.x", "avg_lat.x", "lon_for_spline", "lat_for_spline",
  # "lon.sm.x", "lat.sm.x", "avg_lon.y","avg_lat.y", "avg_lon", "avg_lat", "lon.sm.y", "lat.sm.y", "lon.sm", "lat.sm"):= NULL]
  
  gps_data[,c("lon","lat","ME","avg_lon","avg_lat","lon_for_spline", "lat_for_spline","t_sec", "group_id") := NULL]
  
  return(list(gps_data = gps_data,
              plot = plot))
}
