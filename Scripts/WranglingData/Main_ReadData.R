#################
# Main_ReadData

# Overview:
# Strip the information from the artemis board txt files, clean misreads, and format
# Scale the data to the right units 
# Join the imu and gps data sources together based on the utc timestamp

# Requires:
# Text files from the artemis board

# Note:
# As much as I have endeavoured to make a lot of the code in this repo transferable
# The nonsense that went on with these boards means this is likely a custom solution

# Output:
# Saves the accelerometer in day chunks 
# Accelerometer in Gs
# Magnetometer in mT
# Gyroscope in the original units
# quaternions in original units

#################
# functions
source(file = file.path(base_path, "Scripts", "WranglingData", "DataReadFunctions.R"))
source(file = file.path(base_path, "Scripts", "WranglingData", "UnitsScales.R")) # only need to run this once # figured this out after generating and exploring some of the data

for (Collar in collars){ # giant loop
  print(Collar)
  collar_dir <- file.path(base_path, "Data", "RawData", Collar)
  chunked_dir_path <- file.path(collar_dir, "Chunked")
  
  if(!dir.exists(file.path(collar_dir, "Board"))){ # if there wasnt any board then skip
    next
  }
  
  # remove the incorrect files --------------------------------------------
  if(!file.exists(file.path(collar_dir, "Board_Accel.csv"))){
    
    print("accel")
    
    accel_files <- list.files(path = file.path(collar_dir, "Board"), pattern = "^dataLog\\d+\\.TXT$",  # matches dataLog00000.TXT etc.
      full.names = TRUE, recursive = FALSE)
    
    # find if any files written in 2022 and move them into subfolder "2022_Files"
    # weird bug that I don't understand yet
    file_times <- file.mtime(accel_files)
    files_2022 <- accel_files[format(file_times, "%Y") == "2022"]
    if (length(files_2022) > 0) {
      dir_2022 <- file.path(collar_dir, "Board", "2022_Files")
      dir.create(dir_2022, showWarnings = FALSE)
      file.rename(files_2022, file.path(dir_2022, basename(files_2022)))
    }
    
    # stitch the raw files together ----------------------------------------
    accel_files <- setdiff(accel_files, files_2022)
    accel_data <- stitch_artemis_accel(accel_files)
    
    # Make the adjustments ----------------------------------------------------
    accel_data <- scale_variables(accel_data)
    # filter the noise with a median filter and then a low-pass butterworth filter
    accel_data <- clean_noise(accel_data, med_k = 5)
    
    
    
    
    
    
    ##### NOTE: CHANGE AND CHECK HERE
    
    
    
    
    
    
    
    
    
    # select the columns to remove aand save the data
    accel_data[, c("rtcDate", "rtcTime") := NULL]
    
    fwrite(accel_data, file.path(collar_dir, "Board_Accel.csv"))
  } else{
    accel_data <- fread(file = file.path(collar_dir, "Board_Accel.csv"))
  }
  
  # Check orientation -------------------------------------------
  # did this in the DetermineOrientation script
  # did this manually and took notes in "Notes/Worklog.docx"
  # they were all the same so I didnt have to change anything but here is where that modification would be made
  
  if(!file.exists(file.path(collar_dir, "Board_GPS.csv"))){
    print("gps")
    # Read the GPS files together ---------------------------------------------
    gps_files <- list.files(file.path(collar_dir, "Board"), pattern = "^serialLog.*", full.names = TRUE)
    # remove the ones from 2022
    file_times <- file.mtime(gps_files)
    files_2022 <- gps_files[format(file_times, "%Y") == "2022"]
    if (length(files_2022) > 0) {
      dir_2022 <- file.path(collar_dir, "Board", "2022_Files")
      dir.create(dir_2022, showWarnings = FALSE)
      file.rename(files_2022, file.path(dir_2022, basename(files_2022)))
    }
    # otherwise stitch them togetjer
    gps_files <- setdiff(gps_files, files_2022)
    gps_data <- stitch_artemis_gps(gps_files)
    
    ## NOTE this is implaa specific
    # now remove all the gps hits from australia (select only africa)
    gps_data <- gps_data[lat %between% c(-35, 15) & lon %between% c(25, 50)]
    
    fwrite(gps_data, file.path(collar_dir, "Board_GPS.csv"))
  } else {
    gps_data <- fread(file.path(collar_dir, "Board_GPS.csv"))
  }
  
  # Join the accel and gps --------------------------------------------------
  # Combine and then interpolate the utc timestamp
  print("joining")
   
  # check if they turned on and off the same number of times
  if (length(unique(gps_data$reset_events)) == 1 & length(unique(accel_data$reset_events)) > 1){
    print("there was a mismatch between the reset events of accel and gps")
    big <- accel_data %>% count(reset_events) %>% arrange(-n) %>% slice(1) %>% pull(reset_events)
    gps_data$reset_events <- big
  }

  # Match timestamps in the accelerometer and GPS ---------------------------
  setkey(accel_data, reset_events, numeric_datetime)
  setkey(gps_data,   reset_events, numeric_internal_datetime)
  
  # check whether there are matches and print if there arent # debugging step
  bounds <- range(accel_data$numeric_datetime, na.rm = TRUE)
  any_in_range <- any(
    gps_data$numeric_internal_datetime >= bounds[1] &
      gps_data$numeric_internal_datetime <= bounds[2],
    na.rm = TRUE
  )
  
  if (any_in_range){
    # nearest match
    accel_data[, gps_flag := FALSE]
    accel_data[gps_data,
               on = .(reset_events, numeric_datetime = numeric_internal_datetime),
               roll = "nearest",
               mult = "first",
               `:=`(
                 gps_timestamp        = i.gps_timestamp,
                 lon                  = i.lon,
                 lat                  = i.lat,
                 gps_int_datetime     = i.internal_timestamp,
                 num_gps_datetime     = i.numeric_gps_datetime,
                 gps_flag             = TRUE
               )
    ]
    
    cat("Matched:", sum(accel_data$gps_flag, na.rm = TRUE), "Of total GPS:", nrow(gps_data))
    
  } else {
    print("these dont match or they dont overlap")
  }
  
  # Fill in GPS times between the sat hits ----------------------------------
  # Interpolate GPS times linearly
  accel_data[, gps_time_est_sec := na.approx(num_gps_datetime, na.rm = FALSE)]
  # backwards extrapolate from the first hit so that the gps_time_est_sec is the minus increment between the rtc_datetime
  # Get the first GPS hit
  first_hit <- accel_data[!is.na(num_gps_datetime)][1]
  # Calculate the offset between GPS time and RTC time at first hit
  offset <- first_hit$num_gps_datetime - first_hit$numeric_datetime
  # Backfill rows before the first GPS hit using RTC + offset
  accel_data[is.na(gps_time_est_sec) & numeric_datetime < first_hit$numeric_datetime,
             gps_time_est_sec := numeric_datetime + offset]
  # Convert back to POSIXct
  accel_data[, gps_time_est := as.POSIXct(gps_time_est_sec, origin = "1970-01-01", tz = "UTC")]
  
  # clean up
  names(accel_data)[names(accel_data) == "gps_time_est"] <- "utc_datetime"
  accel_data[, c("rtcDate", "rtcTime", "gps_flag","gps_int_datetime", "num_gps_datetime","numeric_datetime","gps_time_est_sec",
                 "gps_time_est", "reset_events", "rtc_datetime", "gps_timestamp") := NULL]

  # Extract date from estimated GPS time
  accel_data[, date := as.Date(utc_datetime)]
  unique(accel_data$date)
  
  #play <- accel_data[as.Date(accel_data$utc_datetime) < "2024-07-01", ]
  #accel_list <- split(play, by = "date", keep.by = TRUE)
  
  # Split by date
  accel_list <- split(accel_data, by = "date", keep.by = TRUE)
  
  if (!dir.exists(chunked_dir_path)) {
    dir.create(chunked_dir_path, recursive = TRUE)
  }
  
  # Save each day to a separate RDA file in the chunked folder
  lapply(names(accel_list), function(d) {
    accel_data <- accel_list[[d]]
    fwrite(accel_data, file = file.path(chunked_dir_path, paste0("Board_Aligned_", d, ".csv")))
  })

  rm(list = intersect(ls(), c("gps_data", "accel_data", "accel_list")))
}

# files <- list.files(chunked_dir_path, full.names = TRUE)
# for (file in files){
#   load(file)
#   name <- tools::file_path_sans_ext(basename(file))
#   fwrite(accel_data, file.path(chunked_dir_path, paste0(name, ".csv")))
#   rm("accel_data")
# }
