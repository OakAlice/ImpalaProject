# Finding the orientation variables ---------------------------------------

compute_pitch <- function(ax, ay, az) {
  # what is the tilt difference from 0, 0, -1
  pitch <- atan2(-ax, sqrt(ay^2 + az^2))
  pitch_deg = pitch * 180 / pi
  
  return(pitch_deg)
}

compute_roll <- function(ax, ay, az) {
  # Tilt around the X-axis (side-to-side)
  roll <- atan2(ay, sqrt(ax^2 + az^2))
  roll_deg <- roll * 180 / pi
  return(roll_deg)
}

compute_yaw <- function(ax, ay, az, mx, my, mz) {
  # Yaw can't be derived from accelerometer alone (it doesn't sense rotation
  # around gravity). You need a magnetometer (compass).
  
  # First, tilt-compensate the mag readings using pitch & roll
  pitch_rad <- atan2(-ax, sqrt(ay^2 + az^2))
  roll_rad  <- atan2(ay, sqrt(ax^2 + az^2))
  
  # Tilt-compensated magnetic field components
  mx2 <- mx * cos(pitch_rad) + mz * sin(pitch_rad)
  my2 <- mx * sin(roll_rad) * sin(pitch_rad) + my * cos(roll_rad) - mz * sin(roll_rad) * cos(pitch_rad)
  
  yaw <- atan2(-my2, mx2)
  yaw_deg <- yaw * 180 / pi
  return(yaw_deg)
}

orientation_variable_path <- file.path(collar_dir, "orientation_variables.csv")
if(!file.exists(orientation_variable_path)){
  ## define th day you want to use
  # pick something from the middle maybe ... at least after deployment 
  # TODO: change this
  orient_day <- all_days[11]
  
  load(orient_day) # this will come in as accel_data
  
  # define and select the start times (which we recorded when we were running the tests)
  day_data <- accel_data[gps_time_est >= start_time]
  if(nrow(day_data)==0){ # if this was before deployment, dont use it
    print("this was before deployment began, dont choose this day for the orientation test")
    next
  }
  
  # smooth the data
  day_data <- smooth_data(day_data, acc_cols, mag_cols)
  
  # select a small section of it
  orient_data <- day_data[nrow(day_data)-100000:nrow(day_data),]
  
  # calculate a few of the necessary variables
  orientations <- check_orientation(orient_data, columns = c("RawAX.sm", "RawAY.sm", "RawAZ.sm", "RawMX.sm", "RawMY.sm", "RawMZ.sm")) # whatever you want to plot
  acc_orientation_graph <- orientations$orientation_graph
  orientation_table <- orientations$orientation_table
  acc_orientation_graph_static <- orientations$orientation_graph_static
  
  # inspect this image # might be useless if there is an outlier
  ggsave(file.path(collar_dir, "board_orientation.png"), acc_orientation_graph_static)
  
  # calculate the pitch
  pitch <- compute_pitch(orientation_table[1,]$mean, orientation_table[2,]$mean, orientation_table[3,]$mean)
  yaw <- compute_yaw(orientation_table[1,]$mean, orientation_table[2,]$mean, orientation_table[3,]$mean,
                        orientation_table[4,]$mean, orientation_table[5,]$mean, orientation_table[6,]$mean)
  roll <- compute_roll(orientation_table[1,]$mean, orientation_table[2,]$mean, orientation_table[3,]$mean)
  
  # save these
  # TODO: This is currently not working lmao
  # save_image(acc_orientation_graph, file.path(path_to_data, "Orientations_plot.png"))
  
  orient_info <- rbind(
    orientation_table,
    data.frame(series = "pitch", mean = pitch),
    data.frame(series = "roll", mean = roll),
    data.frame(series = "yaw", mean = yaw)
  )
  
  fwrite(orient_info, orientation_variable_path)
}

