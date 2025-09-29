# Calculating VDBA --------------------------------------------------------

# Functions ---------------------------------------------------------------
# calculate VDBA without the effect of gravity
process_cont_VDBA <- function(data, window_length){
  # rolling means (static acceleration)
  data[, ax_static := frollmean(X, n = window_length, align = "center", fill = NA)]
  data[, ay_static := frollmean(Y, n = window_length, align = "center", fill = NA)]
  data[, az_static := frollmean(Z, n = window_length, align = "center", fill = NA)]
  
  # dynamic acceleration
  data[, ax_dynamic := X - ax_static]
  data[, ay_dynamic := Y - ay_static]
  data[, az_dynamic := Z - az_static]
  
  # corrected VDBA & ODBA
  data[, vedba := sqrt(ax_dynamic^2 + ay_dynamic^2 + az_dynamic^2)]
  data[, odba  := abs(ax_dynamic) + abs(ay_dynamic) + abs(az_dynamic)]
  
  # raw (non-corrected) VDBA = vector magnitude of raw acceleration
  data[, raw_vedba := sqrt(X^2 + Y^2 + Z^2)]
  
  return(data)
}

summarise_cont_VDBA <- function(data, window){
  # chunk the data into window number of samples
  data[, window_id := ((seq_len(.N) - 1) %/% window) + 1]
  
  summary <- data[, .(
    Time = first(Time),
    ID = first(ID),
    
    # corrected VDBA
    vedba_mean = mean(vedba, na.rm = TRUE),
    vedba_min  = min(vedba, na.rm = TRUE),
    vedba_max  = max(vedba, na.rm = TRUE),
    
    # corrected ODBA
    odba_mean  = mean(odba, na.rm = TRUE),
    odba_min   = min(odba, na.rm = TRUE),
    odba_max   = max(odba, na.rm = TRUE),
    
    # raw (non-static corrected) VDBA
    raw_vedba_mean = mean(raw_vedba, na.rm = TRUE),
    raw_vedba_min  = min(raw_vedba, na.rm = TRUE),
    raw_vedba_max  = max(raw_vedba, na.rm = TRUE)
    
  ), by = .(ID, window_id)]
  
  summary <- na.omit(summary)
  return(summary)
}



# Code --------------------------------------------------------------------
unlabelled_files <- list.files(file.path(base_path, "RawData", collar, "Board", "Chunked"), full.names = TRUE, pattern = ".RDA")

# only process ones that have legitimate data in them (i.e., not before the collar was deployed)
start_dates <- fread(file.path(base_path, "RawData", "StartTimes.csv"))
start_date <- as.POSIXct(start_dates[Collar == collar_num, StartDate], format = "%d/%m/%Y")

energy <- lapply(unlabelled_files, function(x){
  
  filname <- tools::file_path_sans_ext(basename(x))
  print(filname)
  filname_date <- as.POSIXct(gsub("Board_Aligned_", "", filname), format = "%Y-%m-%d")
  
  # skip files before deployment
  if (filname_date < start_date) {
    print("this file is from before the device was deployed... skipping...")
    return(NULL)
  }
  
  out_file <- file.path(base_path, "Output", collar, paste0(filname, "_vdba.csv"))
  
  load(x)  # reads in accel_data
  accel_data <- accel_data[, c("RawAX", "RawAY", "RawAZ", "gps_time_est")]
  colnames(accel_data) <- c("X", "Y", "Z", "Time")
  accel_data$ID <- collar
  
  window_samples <- 60 * sample_rate # 1-minute smoothing
  processed_data <- process_cont_VDBA(accel_data, window_length = window_samples)
  summarised_data <- summarise_cont_VDBA(data = processed_data, window_samples)
  
  return(summarised_data)
})

#combine
energy <- energy[!sapply(energy, is.null)]
energy <- rbindlist(energy)

fwrite(energy, file.path(base_path, "Output", collar, paste("Vdba.csv")))
