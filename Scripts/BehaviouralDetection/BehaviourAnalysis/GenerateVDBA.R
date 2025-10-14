# Generate Features -------------------------------------------------------

# calculate when the data is continuous
process_cont_VDBA <- function(data, window_length){
  # in this case, we can calculate static acceleration in the more traditional way
  # rolling mean instead of a direct average
  
  # basic vedba the way we used to do it 
  data[, basic_vedba := sqrt(Accel.X^2 + Accel.Y^2 + Accel.Z^2)]

  # rolling means (static acceleration)
  data[, ax_static := frollmean(Accel.X, n = window_length, align = "center", fill = NA)]
  data[, ay_static := frollmean(Accel.Y, n = window_length, align = "center", fill = NA)]
  data[, az_static := frollmean(Accel.Z, n = window_length, align = "center", fill = NA)]
    
  # dynamic acceleration
  data[, ax_dynamic := Accel.X - ax_static]
  data[, ay_dynamic := Accel.Y - ay_static]
  data[, az_dynamic := Accel.Z - az_static]
    
  # VDBA
  data[, vedba := sqrt(ax_dynamic^2 + ay_dynamic^2 + az_dynamic^2)]
  data[, odba := abs(ax_dynamic) + abs(ay_dynamic) + abs(az_dynamic)]

  return(data)
}

# summarise into windows of VDBA
summarise_cont_VDBA <- function(data, window){
  # chunk the data into window number of samples
  # calculate the mean, min, and max for vedba and odba columns
  data[, window_id := ((seq_len(.N) - 1) %/% window) + 1]
  
  # calculate within each of these wuindows
  summary <- data[, .(
    Time = first(Time),    # first timestamp in the window
    basic_vedba_mean = mean(basic_vedba, na.rm = TRUE),
    basic_vedba_min  = min(basic_vedba, na.rm = TRUE),
    basic_vedba_max  = max(basic_vedba, na.rm = TRUE),
    vedba_mean = mean(vedba, na.rm = TRUE),
    vedba_min  = min(vedba, na.rm = TRUE),
    vedba_max  = max(vedba, na.rm = TRUE),
    odba_mean  = mean(odba, na.rm = TRUE),
    odba_min   = min(odba, na.rm = TRUE),
    odba_max   = max(odba, na.rm = TRUE)
  ), by = .(ID, window_id)]
  
  # clean up the NA head and tail
  summary <- na.omit(summary)
  
  return(summary)
}

