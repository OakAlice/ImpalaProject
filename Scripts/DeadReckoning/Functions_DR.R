#################
# Functions_DR

# Overview:
# Custom functions to assist with the dead reckoning 

#################

# Smooth and filter the data for use in the Gundog system -----------------
smooth_and_filter <- function(data, k , fs, bw_cutoff = 5, bw_order = 4){
  
  setDT(data)
  # Median smoothing (k=5)
  med_cols <- c("RawAX", "RawAY", "RawAZ", "RawMX", "RawMY", "RawMZ")
  for (col in med_cols) set(data, j = paste0(col, ".med"), value = runmed(data[[col]], k = 5))
  
  # Butterworth low-pass filter
  # determining the cutoff with the PSD
  # psd <- spectrum(day_data$RawAX, spans = c(5,5), taper = 0.1, 
  #                 plot = FALSE)
  # plot(psd$freq * fs, 10*log10(psd$spec), type = "l",
  #      xlab = "Frequency (Hz)", ylab = "Power (dB)",
  #      main = "Power Spectral Density — RawAX")
  # abline(v = 0.5, col = "steelblue", lty = 2)   # candidate cutoff
  bf <- butter(bw_order, bw_cutoff / (fs / 2), type = "low")
  for (col in med_cols) set(data, j = paste0(col, ".butt"), value = filtfilt(bf, data[[paste0(col, ".med")]]))
  
  # Rolling mean smoothing
  for (col in paste0(med_cols[1:3], ".butt")) {
    base <- sub("\\.butt$", "", col)
    data[[paste0(base, ".sm")]] <- rollapply(data[[col]], width = 50, FUN = mean, align = "center", fill = "extend")
  }
  
  for (col in paste0(med_cols[4:6], ".butt")) {
    base <- sub("\\.butt$", "", col)
    data[[paste0(base, ".sm")]] <- rollapply(data[[col]], width = 20, FUN = mean, align = "center", fill = "extend")
  }
  
  # plot it to check the differences
  # p0 <- ggplot(accel_data[1:10000,], aes(x = gps_time_est)) + geom_path(aes(y = RawAX, colour = "X")) + geom_path(aes(y = RawAY, colour = "Y")) + geom_path(aes(y = RawAZ, colour = "Z"))
  # p1 <- ggplot(accel_data[1:10000,], aes(x = gps_time_est)) + geom_path(aes(y = RawAX.med, colour = "X")) + geom_path(aes(y = RawAY.med, colour = "Y")) + geom_path(aes(y = RawAZ.med, colour = "Z"))
  # p2 <- ggplot(accel_data[1:10000,], aes(x = gps_time_est)) + geom_path(aes(y = RawAX.butt, colour = "X")) + geom_path(aes(y = RawAY.butt, colour = "Y")) + geom_path(aes(y = RawAZ.butt, colour = "Z"))
  # p0/p1/p2
  
  # and just select the columns you want
  # data[, c(paste0(med_cols, ".med"), paste0(med_cols, ".butt")) := NULL]
  
  return(data)
}



activity_scoring <- function(data, threshold = 0.005, smooth_width = 100){
  data <- data %>% arrange(ID, utc_datetime)
  # calculate the Vectorial Dynamic Body Acceleration (and smoothed version, as well as the sd)
  data$VDBA <- sqrt((data$RawAX.cl - data$RawAX.sm)^2 + 
                      (data$RawAY.cl - data$RawAY.sm)^2 +
                      (data$RawAZ.cl - data$RawAZ.sm)^2)                     
  data$VDBA.sm <- rollapply(data$VDBA, width=50, FUN=mean, align="center", fill="extend")  # 1 s sm
  data$VDBA.sd <- rollapply(data$VDBA, width=smooth_width, FUN=sd, align="center", fill="extend") # over 5 sec
  
  # find whenever it is sleepinng and tag as 0 ME. 1 for movement, 0 for non-movement
  # when I have finished the behavioural prediction analysis, I will be able to be more refined here
  data$ME <- ifelse(data$VDBA.sd < threshold, 0, 1)
  
  ## NOTE: Removed this when switched to behavioural model method
  # and then see if there are multiple in a row (as in, only meaningful if it stops for a whole minute or more)
  # Apply mode for each little section
  # fs <- 50 # in case not already defined
  # roll_mode <- function(x) {
  #   ux <- unique(x)
  #   ux[which.max(tabulate(match(x, ux)))]
  # }
  # data[, epoch := ceiling(.I / (fs * 60))]
  # data[, ME := roll_mode(ME), by = epoch]
  # data[, epoch := NULL]
  # data[, group_id := cumsum(ME != shift(ME, fill = ME[1])) + 1]
  # 
  # plot to see
  # plot_data <- data[1000000:2000000,]
  # ggplot(plot_data, aes(x = seq(1:nrow(plot_data)))) +
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
