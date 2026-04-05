# Functions used in sanity checking and debugging -------------------------

smooth_data <- function(data, acc_cols, mag_cols = NULL){
  # Apply 1s smoothing to acceleration (extract static) and a lighter smoothing to magnetometer
  
  for (col in acc_cols)
    data[[paste0(col, ".sm")]] <- rollapply(data[[col]], width = 50, FUN = mean, align = "center", fill = "extend")
  
  for (col in mag_cols)
    data[[paste0(col, ".sm")]] <- rollapply(data[[col]], width = 20, FUN = mean, align = "center", fill = "extend")
  
  return(data)
}

calculate_VDBA <- function(data, acc_cols){
  
  sm_cols <- paste0(acc_cols, ".sm")
  
  # calculate the Vectorial Dynamic Body Acceleration (and smoothed version)
  data$VDBA = sqrt((data[[acc_cols[1]]] - data[[sm_cols[1]]])^2 + 
                     (data[[acc_cols[2]]] - data[[sm_cols[1]]])^2 +
                     (data[[acc_cols[3]]] - data[[sm_cols[1]]])^2)                     
  data$VDBA.sm = rollapply(data$VDBA, width=50, FUN=mean, align="center", fill="extend")  # 1 s sm
  return(data)
}

check_orientation <- function(data, columns){
  # Function for creating an interactive graph
  # Used to establish the orientation of the device
  
  # Subset relevant columns and rearrange
  accel_data <- data[, c("rtc_datetime", ..columns)]
  
  # normalise so between 0 and 1 
  # normalize <- function(x) (x - min(x)) / (max(x) - min(x))
  # accel_data <- accel_data %>%
  #   mutate(across(where(is.numeric), normalize))
           
  # rearrange
  accel_long <- melt(
    accel_data,
    id.vars = "rtc_datetime",
    variable.name = "series",
    value.name = "acceleration"
  )
  
  # Plot interactive
  orientation_graph <- plot_ly(accel_long, x = ~rtc_datetime, y = ~acceleration, 
                               color = ~series, type = "scatter", mode = "lines",
                               colors = c("#FF9999", "#99FF99", "#9999FF",
                                          "#FF7F50", "#2B9988", "darkblue"),
                               line = list(width = 1)) %>%
    layout(
      xaxis = list(title = "Time"),
      yaxis = list(title = "Acceleration"),
      legend = list(title = list(text = ""))
    )
  
  # and make a dataframe of it too
  orientation_table <- accel_long %>%
    group_by(series) %>%
    summarise(mean = mean(acceleration))
  
  orientation_accel <- ggplot(accel_long %>% filter(series %in% c("RawAX.sm", "RawAY.sm", "RawAZ.sm")), aes(x = rtc_datetime, y = acceleration, colour = series)) + 
    geom_path() +
    labs(title = "Accelerometer") +
    scale_colour_manual(values = c("#FF9999", "#99FF99", "#9999FF")) +
    theme_minimal()
  orientation_mag <- ggplot(accel_long %>% filter(series %in% c("RawMX.sm", "RawMY.sm", "RawMZ.sm")), aes(x = rtc_datetime, y = acceleration, colour = series)) + 
    geom_path() +
    labs(title = "Magnetometer") +
    scale_colour_manual(values = c("#FF7F50", "#2B9988", "darkblue")) +
    theme_minimal()
  
  orientation_graph_static <- orientation_accel + orientation_mag
    
  return(list(orientation_graph = orientation_graph,
              orientation_table = orientation_table,
              orientation_graph_static = orientation_graph_static)
         )
}

plot_roll_pitch_yaw <- function(data){
  # Function to plot the roll pitch and yaw of the device throughout the trial
  
  setDT(data)
  # Subset relevant columns and rearrange
  accel_data <- data[, c("Roll", "Pitch", "Yaw")]
  accel_data[, timestamp := .I] # add the row number in place of the timestamp
  accel_long <- melt(
    accel_data,
    id.vars = "timestamp",
    variable.name = "series",
    value.name = "acceleration"
  )
  
  plot <- ggplot(accel_long, aes(x = timestamp, y = acceleration)) +
    geom_line(size = 1) +
    labs(y = "Angle (degrees)", x = "Time") +
    theme_minimal() +
    facet_wrap(~series, scales = "free", ncol =1)
  
  return(plot)
}
