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
    
    base <- c("RawAX", "RawAY", "RawAZ", "RawMX", "RawMY", "RawMZ")
    base_acc <- base[grep("A", base)]
    base_mag <- base[grep("M", base)]
    
    ## scale the axes ------------------------------------------------------
    # determined the following divisions from the info sheet and data exploration
    accel_data[, c(base_acc) := lapply(.SD, function(x) x / 2048), .SDcols = base_acc]
    accel_data[, c(base_mag) := lapply(.SD, function(x) x / 2048), .SDcols = base_mag]
    accel_data[, c(base_mag) := lapply(.SD, function(x) x * 0.15), .SDcols = base_mag]
    # TODO: Add in the gyroscope conversion when I have figured it out
    
    ## Now filter them... firstly the median filter (k=5) -----------------
    setDT(accel_data)
    for (col in base) set(accel_data, j = paste0(col, ".med"), value = runmed(accel_data[[col]], k = 5))
    
    ## Butterworth low-pass filter -------------------------------------------
      # determining the cutoff with the PSD
      # psd <- spectrum(day_data$RawAX, spans = c(5,5), taper = 0.1, plot = FALSE)
      # plot(psd$freq * fs, 10*log10(psd$spec), type = "l", xlab = "Frequency (Hz)", ylab = "Power (dB)", main = "Power Spectral Density — RawAX")
      # abline(v = 0.5, col = "steelblue", lty = 2)   # candidate cutoff
    bf <- butter(n = 4, W = 10 / (50 / 2), type = "low") # where the 50 is the sampling rate
    for (col in paste0(base, ".med")) set(accel_data, j = paste0(sub("\\.med$", "", col), ".butt"), value = filtfilt(bf, accel_data[[col]]))
    
    # plot it to check the differences --------------------------------------
    # p0 <- ggplot(accel_data[1:10000,], aes(x = gps_time_est)) + geom_path(aes(y = RawAX.scaled, colour = "X")) + geom_path(aes(y = RawAY.scaled, colour = "Y")) + geom_path(aes(y = RawAZ.scaled, colour = "Z"))
    # p1 <- ggplot(accel_data[1:10000,], aes(x = gps_time_est)) + geom_path(aes(y = RawAX.med, colour = "X")) + geom_path(aes(y = RawAY.med, colour = "Y")) + geom_path(aes(y = RawAZ.med, colour = "Z"))
    # p2 <- ggplot(accel_data[1:10000,], aes(x = gps_time_est)) + geom_path(aes(y = RawAX.butt, colour = "X")) + geom_path(aes(y = RawAY.butt, colour = "Y")) + geom_path(aes(y = RawAZ.butt, colour = "Z"))
    # p0/p1/p2
    
    ## VDBA -----------------------------------------------------------------
    # calculate the VDBA from the smoothed (static) acceleration
    accel_data$RawAX.sm <- rollapply(accel_data$RawAX.butt, width=50, FUN=mean, align="center", fill="extend")
    accel_data$RawAY.sm <- rollapply(accel_data$RawAY.butt, width=50, FUN=mean, align="center", fill="extend")
    accel_data$RawAZ.sm <- rollapply(accel_data$RawAZ.butt, width=50, FUN=mean, align="center", fill="extend")
    
    # calculating this here because we need it for the dead reckoning later on and easier to do while variables exist here
    butt_cols <- paste0(base_acc, ".butt")
    sm_cols <- paste0(base_acc, ".sm")
    # calculate the Vectorial Dynamic Body Acceleration (and smoothed version, as well as the sd)
    accel_data$VDBA <- sqrt((accel_data[[butt_cols[1]]] - accel_data[[sm_cols[1]]])^2 + 
                              (accel_data[[butt_cols[2]]] - accel_data[[sm_cols[1]]])^2 +
                              (accel_data[[butt_cols[3]]] - accel_data[[sm_cols[1]]])^2)                     
    
    # also smooth the mag
    accel_data$RawMX.sm <- rollapply(accel_data$RawMX.butt, width=50, FUN=mean, align="center", fill="extend")
    accel_data$RawMY.sm <- rollapply(accel_data$RawMY.butt, width=50, FUN=mean, align="center", fill="extend")
    accel_data$RawMZ.sm <- rollapply(accel_data$RawMZ.butt, width=50, FUN=mean, align="center", fill="extend")
    
    # select the columns to remove aand save the data
    accel_data[, c(base, paste0(base, ".med")) := NULL]
    setnames(accel_data, paste0(base, ".butt"), paste0(base, ".cl"))
    accel_data[, c("rtcDate", "rtcTime") := NULL] # quaternions were wrong in this iteration
    
    fwrite(accel_data, file = file.path(collar_dir, "Board_Accel.csv"))
    # accel_data <- fread(file = file.path(collar_dir, "Board_Accel.csv"))
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
  }
  
  # Join the accel and gps --------------------------------------------------
  # Combine and then interpolate the utc timestamp
  print("joining")
     
  gps_data <- fread(file.path(collar_dir, "Board_GPS.csv"))
  accel_data <- fread(file = file.path(collar_dir, "Board_Accel.csv"))
   
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
  accel_data[, c("gps_flag","gps_int_datetime", "num_gps_datetime","numeric_datetime","gps_time_est_sec") := NULL]
  setnames(accel_data, "gps_time_est", "utc_datetime")
  
  # Save the matched data ---------------------------------------------------
  # save(accel_data, file = file.path(collar_dir, "Board_Aligned.RDA"))
  # load(file = file.path(collar_dir, "Board_Aligned.RDA"))
  
  # Extract date from estimated GPS time
  accel_data[, date := as.Date(utc_datetime)]
  unique(accel_data$date)
  
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
