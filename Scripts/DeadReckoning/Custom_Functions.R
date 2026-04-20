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

check_orientation <- function(data, accel_cols, mag_cols) {
  
  make_plot <- function(cols, y_label, colors) {
    long <- melt(
      data[, c("rtc_datetime", ..cols)],
      id.vars      = "rtc_datetime",
      variable.name = "series",
      value.name   = y_label
    )
    plot_ly(long, x = ~rtc_datetime, y = as.formula(paste0("~", y_label)),
            color = ~series, type = "scatter", mode = "lines",
            colors = colors, line = list(width = 1)) %>%
      layout(
        xaxis  = list(title = "Time"),
        yaxis  = list(title = y_label),
        legend = list(title = list(text = ""))
      )
  }
  
  accel_plot <- make_plot(accel_cols, "Acceleration", c("#FF9999", "#99FF99", "#9999FF"))
  mag_plot   <- make_plot(mag_cols,   "Magnetism",    c("#FF7F50", "#2B9988", "darkblue"))
  
  # summary table for both
  all_cols <- c(accel_cols, mag_cols)
  orientation_table <- melt(
    data[, c("rtc_datetime", ..all_cols)],
    id.vars       = "rtc_datetime",
    variable.name = "series",
    value.name    = "value"
  ) %>%
    group_by(series) %>%
    summarise(mean = mean(value, na.rm = TRUE))
  
  return(list(
    accel_graph       = accel_plot,
    mag_graph         = mag_plot,
    orientation_table = orientation_table
  ))
}

compute_pitch <- function(ax, ay, az) atan2(-ax, sqrt(ay^2 + az^2)) * 180 / pi

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
