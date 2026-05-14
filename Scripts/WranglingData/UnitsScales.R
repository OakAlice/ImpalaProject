# Preprocessing information stuff -----------------------------------------
# We need to know the settings and orientation of the device when they were first deployed.
# the device settings as reported don't seem to be the most reliable
# and also, they were all differnt.
# To this, Chris Bird said: "I wouldn't want to make it too easy for you"

# find the settings in the device information for each collar ---------------
run_once <- TRUE
if (!run_once){
  results <- list()
  for (Collar in collars) {
    collar_dir <- file.path(base_path, "Data", "RawData", Collar)
    
    # settings file
    settings_file <- file.path(collar_dir, "Board", "OLA_settings.txt")
    if (!file.exists(settings_file) || file.size(settings_file) == 0) {
      message("No settings file found for collar: ", Collar)
      next
    }
    settings <- fread(settings_file, sep = "=")
    
    # extract useful settings
    hz <- 1e6 / as.numeric(settings$V2[settings$V1 == "usBetweenReadings"])
    
    accel_scale <- c("0" = 16384, "1" = 8192, "2" = 4096, "3" = 2048)
    accel_fss   <- as.numeric(settings$V2[settings$V1 == "imuAccFSS"])
    acc_divide_by <- accel_scale[as.character(accel_fss)]
    
    mag_mult_by <- 0.15
    
    results[[Collar]] <- data.frame(
      collar        = Collar,
      Hz            = hz,
      accel_fss     = accel_fss,
      acc_divide_by = acc_divide_by,
      mag_mult_by   = mag_mult_by
    )
  }
  results <- rbindlist(results)
  fwrite(results, file.path("Data", "OLA_collar_settings.csv"))
  
  # find the collar settings based on the first day of deployment ---------------
  deployment_dates <- fread(file.path(base_path, "Notes", "ImpalaCollaringTimes.csv"))
  
  results2 <- list()
  for(Collar in collars){
    
    collar_dir <- file.path(base_path, "Data", "RawData", Collar, "Chunked")
    
    # find the time after it had been collared
    deployment_start <- as.POSIXct(deployment_dates$DeploymentStart[deployment_dates$Collar == Collar], format = "%d.%m.%Y %H:%M:%S")
    deployment_date <- as.Date(deployment_start)
    
    accel_days <- list.files(collar_dir, full.names = TRUE)
    accel_day <- accel_days[grep(deployment_date, accel_days)]
    
    if (!dir.exists(collar_dir) || length(accel_day) == 0) {
      message("No accel found for collar: ", Collar)
      next
    }
    
    load(accel_day) # comes in as accel_data
    accel_data <- accel_data[accel_data$gps_time_est > deployment_start, ]
    
    # now extract the important details
    results2[[Collar]] <- accel_data[, .(
      CollarNum = Collar,
      # sampling rate from timestamp differences
      avg_hz = 1 / as.numeric(mean(diff(rtc_datetime), na.rm = TRUE)),
      
      # accelerometer means and ranges
      mean_AX = mean(RawAX, na.rm = TRUE),
      mean_AY = mean(RawAY, na.rm = TRUE),
      mean_AZ = mean(RawAZ, na.rm = TRUE),
      min_AX  = min(RawAX,  na.rm = TRUE),
      max_AX  = max(RawAX,  na.rm = TRUE),
      min_AY  = min(RawAY,  na.rm = TRUE),
      max_AY  = max(RawAY,  na.rm = TRUE),
      min_AZ  = min(RawAZ,  na.rm = TRUE),
      max_AZ  = max(RawAZ,  na.rm = TRUE),
      
      # magnetometer means and ranges
      mean_MX = mean(RawMX, na.rm = TRUE),
      mean_MY = mean(RawMY, na.rm = TRUE),
      mean_MZ = mean(RawMZ, na.rm = TRUE),
      min_MX  = min(RawMX,  na.rm = TRUE),
      max_MX  = max(RawMX,  na.rm = TRUE),
      min_MY  = min(RawMY,  na.rm = TRUE),
      max_MY  = max(RawMY,  na.rm = TRUE),
      min_MZ  = min(RawMZ,  na.rm = TRUE),
      max_MZ  = max(RawMZ,  na.rm = TRUE)
    )]
    
  }
  results2 <- rbindlist(results2)
  
  # insert conversion logic for the results2 based on the real data
  #### NOTE: logic specific to this dataset
  results2 <- results2 %>%
    mutate(Acc_Conv = case_when(
      abs(max_AX) < 16 & abs(max_AX) > 4 ~ "+/- 16g",
      abs(max_AX) < 4  ~ "+/- 4g",
      abs(max_AX) > 30000 ~ "/2048 (then +/- 16g)",
      TRUE                  ~ "unknown"
    )) %>%
    mutate(Mag_Conv = case_when(
      abs(max_MX) > 20000 ~ "/2048 then *0.15",
      TRUE                  ~ "*0.15"
    ))
  
  fwrite(results2, file.path("Data", "True_collar_settings.csv"))
  
} else {
  print("this has already been run")
}
