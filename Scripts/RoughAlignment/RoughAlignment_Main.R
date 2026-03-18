# Aligning the timestamps from the data -----------------------------------
for (Collar in collars){
  
  # Read artemis accel files together ---------------------------------------
  path_to_data <- file.path(base_path, Collar, "Board")
  accel_files <- list.files(path_to_data, pattern = "dataLog", full.names = TRUE)
  gps_files <- list.files(path_to_data, pattern = "serialLog", full.names = TRUE)
  
  # Read artemis accel files together ---------------------------------------
  if(!file.exists(file.path(base_path, Collar, "Artemis_Accel.csv"))){
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
    save(accel_data, file = file.path(collar, "Artemis_Accel.RDA"), compress = FALSE) # make it faster to read and write
    fwrite(accel_data, file = file.path(collar, "Artemis_Accel.csv"))
  } else {
    print("already made the accel data")
  }
  
  
  # Read the GPS files together ---------------------------------------------
  if (!file.exists(file.path(accel_dir, "Board_GPS.csv"))){
    source(file = file.path(base_path, "Scripts", "RoughAlignment", "CombiningArtemisGPSFiles.R"))
  }
  
  # Combing the GPS and Accel files based on timestamp ----------------------
  # saves all aligned data as a single RDA, as well each 24-hr period as its own file
  if (!file.exists(file.path(accel_dir, "Board_Aligned.RDA"))){
    source(file = file.path(base_path, "Scripts", "RoughAlignment", "CombiningArtemisAccel_GPS.R"))
  }
  
  # delete the files that don't matter (outside of the sampling days of interest)
  all_files <- list.files(file.path(base_path, Collar, "ArtemisAlignedChunked"), full.names = TRUE, recursive = TRUE)
  start_date <- sampling_start %>% filter(Collar == str_split(basename(collar), "_", simplify = TRUE)[2]) %>% pull(StartDate)
  file_dates <- as.Date(stringr::str_extract(all_files, "\\d{4}-\\d{2}-\\d{2}"))
  selected_files <- all_files[file_dates >= start_date & file_dates <= end_date]
  file.remove(setdiff(all_files, selected_files))
  
  # get the metadata for each of the videos -----------------------------
  video_dir <- file.path(base_path, "RawData", Collar, "Videos")
  if (!file.exists(file.path(video_dir, "Video_metadata.csv"))){
    source(file = file.path(base_path, "Scripts", "RoughAlignment", "VideoInfoExtraction.R"))
  }
} 

# Manually Assessing Alignment / Determining Delay ------------------------
# the reality of working with technology is that some of the clocks drifted
# we need to determine the amount by which it drifted and it is easiest to do this manually
# use the following script to explore and play around with the different files
file <- file.path(base_path, "Scripts", "RoughAlignment", "AccelDelayFinder.R")


