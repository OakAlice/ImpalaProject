# Read in and Align the Boards --------------------------------------------

# define the path to the files
# if(!file.exists(file.path(collar_dir, "Board_Aligned.RDA"))){
  
  if(!dir.exists(file.path(collar_dir, "Board"))){ # if there wasnt any board then skip
    next
  }
  
  # Read artemis accel files together ---------------------------------------
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
  
  # otherwise stitch them togetjer
  accel_files <- setdiff(accel_files, files_2022)
  accel_data <- stitch_artemis_accel(accel_files)
  
  # clean up the variables
  accel_data[, c("rtcDate", "rtcTime") := NULL]
  
  save(accel_data, file = file.path(collar_dir, "Board_Accel.RDA"), compress = FALSE)
  # load(file = file.path(collar_dir, "Board_Accel.RDA"))
  
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
  fwrite(gps_data, file.path(collar_dir, "Board_GPS.csv"))
  
  # # check whether the GPS times all increment by an expected amount --------
  # gps_data[, int_time_diff := c(NA_real_, diff(unclass(internal_timestamp))), by = reset_events]
  # gps_data[, ext_time_diff := c(NA_real_, diff(unclass(gps_timestamp))), by = reset_events]
  # gps_data[, inc_diff := int_time_diff - ext_time_diff]
  # 
  # if (any(gps_data$inc_diff > 60 | gps_data$inc_diff < -60, na.rm = TRUE)) {
  #   print("there is something doesn't increment normally... it's more than a minute out (not incl. reset events")
  # }
  
  # now remove all the gps hits from australia (select only africa)
  gps_data <- gps_data[lat %between% c(-35, 15) & lon %between% c(25, 50)]
  # good_resets <- unique(gps_data$reset_events)
  # and then select them from the accel data as well
  # accel_data <- accel_data[accel_data$reset_events %in% good_resets]
  
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
  
  # Save the matched data ---------------------------------------------------
  save(accel_data, file = file.path(collar_dir, "Board_Aligned.RDA"))
  # load(file = file.path(collar_dir, "Board_Aligned.RDA"))
  
  # Extract date from estimated GPS time
  accel_data[, date := as.Date(gps_time_est)]
  unique(accel_data$date)
  
  # Split by date
  accel_list <- split(accel_data, by = "date", keep.by = TRUE)
  
  if (!dir.exists(chunked_dir_path)) {
    dir.create(chunked_dir_path, recursive = TRUE)
  }
  
  # Save each day to a separate RDA file in the chunked folder
  lapply(names(accel_list), function(d) {
    accel_data <- accel_list[[d]]
    save(accel_data, file = file.path(chunked_dir_path, paste0("Board_Aligned_", d, ".RDA")))
  })

# }