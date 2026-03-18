# Aligning the timestamps from the data -----------------------------------
# Read artemis accel files together ---------------------------------------
path_to_data <- file.path(base_path, Collar, "Board")
accel_files <- list.files(path_to_data, pattern = "dataLog", full.names = TRUE)
gps_files <- list.files(path_to_data, pattern = "serialLog", full.names = TRUE)

# Read artemis accel files together ---------------------------------------
if(!file.exists(file.path(base_path, Collar, "Artemis_Accel.RDA"))){
  accel_data <- stitch_artemis_accels(accel_files)
  setDT(accel_data)
  # convert the units of acceleration
  accel_data[, c("RawAX", "RawAY", "RawAZ")] <- accel_data[, c("RawAX", "RawAY", "RawAZ")] / 2048
  # remove the empty column
  accel_data[, V17 := NULL]
  
  # convert the internal timestamp
  accel_data[, rtc_datetime :=
               as.POSIXct(paste(rtcDate, rtcTime), format = "%m/%d/%Y %H:%M:%OS", tz = "UTC")
  ]
  
  # Find whenever the device resets and label those as separate sampling events
  accel_data[, time_diff :=  c(NA_real_, diff(unclass(rtc_datetime)))]
  accel_data[, reset := as.integer(time_diff < 0)]
  accel_data[is.na(reset), reset := 0L]
  accel_data[, reset_events := cumsum(reset)]
  
  # clean it up
  accel_data[, c("time_diff", "reset", "output_Hz") := NULL]
  
  # save as an RDA file
  save(accel_data, file = file.path(base_path, Collar, "Artemis_Accel.RDA"), compress = FALSE) # make it faster to read and write
  # fwrite(accel_data, file = file.path(base_path, Collar, "Artemis_Accel.csv"))
} else {
  print("already made the accel data")
}

# Read the GPS files together ---------------------------------------------
if(!file.exists(file.path(base_path, Collar, "Artemis_GPS.csv"))){
  gps_data <- stitch_artemis_gps(gps_files)
  
  if(nrow(gps_data) < 1){
    print("no suffessful hits :( sad")
    next
  }
  gps_data <- gps_data[complete.cases(gps_data), ]
  
  ##NOTE: Because our study was international, we set up the collars in Australia, tested them several times, and then flew them to South Africa
  # This created a fair bit of chaos for our collars with the time resetting continuously
  # I have to remove everything that occurred prior to Africa (without removing the calibration event either)
  # therefore we have to account for the fact that collars will revert to 0 multiple times
  gps_data[, time_diff :=  c(NA_real_, diff(unclass(internal_timestamp)))]
  gps_data[, reset := as.integer(time_diff < 0)]
  gps_data[is.na(reset), reset := 0L]
  gps_data[, reset_events := cumsum(reset)]
  
  # clean it up
  gps_data[, c("time_diff", "reset") := NULL]
  
  fwrite(gps_data, file.path(Collar, "Artemis_GPS.csv"))
} else {
  print("aloready made the gps before")
}

# Combing the GPS and Accel files based on timestamp ----------------------
# saves all aligned data as a single RDA, as well each 24-hr period as its own file
if (!file.exists(file.path(base_path, Collar, "Board_Aligned.RDA"))){
  # source(file = file.path(base_path, "Scripts", "RoughAlignment", "CombiningAccelGPS.R")) # new
  source(file = file.path(base_path, "Scripts", "RoughAlignment", "CombiningArtemisAccel_GPS.R")) # old
}

# delete the files that don't matter (outside of the sampling days of interest)
all_files <- list.files(file.path(base_path, Collar, "ArtemisAlignedChunked"), full.names = TRUE, recursive = TRUE)
start_date <- sampling_start %>% dplyr::filter(CollarNumber == str_split(basename(Collar), "_", simplify = TRUE)[2]) %>% pull(StartDate)
file_dates <- as.Date(stringr::str_extract(all_files, "\\d{4}-\\d{2}-\\d{2}"))
selected_files <- all_files[as.Date(file_dates, format = "%Y-%m-%d") >= as.Date(start_date, format = "%Y-%m-%d")]
file.remove(setdiff(all_files, selected_files))

# get the metadata for each of the videos -----------------------------
video_dir <- file.path(base_path, Collar, "Videos")
if (!file.exists(file.path(video_dir, "Video_metadata.csv"))){
  source(file = file.path(base_path, "Scripts", "RoughAlignment", "VideoInfoExtraction.R"))
}



