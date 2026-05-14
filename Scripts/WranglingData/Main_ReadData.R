# Read in and Align the Boards --------------------------------------------

if(!dir.exists(file.path(collar_dir, "Board"))){ # if there wasnt any board then skip
  next
}

# rescale and clean the accelerometer data --------------------------------
if(!file.exists(file.path(collar_dir, "Board_Accel.RDA"))){
  
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
  
  ## smooth and clean the data ------------------------------------------------------
  # determine how I have to scale the data
  scales <- fread(file.path("Data", "True_collar_settings.csv")) %>%
    dplyr::filter(CollarNum == Collar)
  # acc_cov <- if(grepl("/2048", scales$Acc_Conv)){ "2048" } else {"0"}
  # mag_cov <- if(grepl("/2048", scales$Mag_Conv)){ "2048" } else {"0"}
  
  # the columns to use 
  base <- c("RawAX", "RawAY", "RawAZ", "RawMX", "RawMY", "RawMZ")
  base_acc <- base[grep("A", base)]
  base_mag <- base[grep("M", base)]
  
  ## convert the axes -------------------------------------------------------
  # convert all of them (no logic)
  #if (acc_cov == 2048) {
    accel_data[, paste0(base_acc, ".scaled") := lapply(.SD, function(x) x / 2048), .SDcols = base_acc]
  # } else {
  #   accel_data[, paste0(base_acc, ".scaled") := .SD, .SDcols = base_acc]
  # }
  # if (mag_cov == 2048) {
    accel_data[, paste0(base_mag, ".scaled") := lapply(.SD, function(x) x / 2048), .SDcols = base_mag]
  # } else {
  #   accel_data[, paste0(base_mag, ".scaled") := .SD, .SDcols = base_mag]
  # }
  
  # and now all the mags get scaled as well
  accel_data[, paste0(base_mag, ".scaled") := lapply(.SD, function(x) x * 0.15), .SDcols = paste0(base_mag, ".scaled")]
  
  ## and now the median filter (k=5) ---------------------------------------
  setDT(accel_data)
  for (col in paste0(base, ".scaled")) set(accel_data, j = paste0(sub("\\.scaled$", "", col), ".med"), value = runmed(accel_data[[col]], k = 5))
  
  ## Butterworth low-pass filter -------------------------------------------
  # determining the cutoff with the PSD
  # psd <- spectrum(day_data$RawAX, spans = c(5,5), taper = 0.1, 
  #                 plot = FALSE)
  # plot(psd$freq * fs, 10*log10(psd$spec), type = "l",
  #      xlab = "Frequency (Hz)", ylab = "Power (dB)",
  #      main = "Power Spectral Density — RawAX")
  # abline(v = 0.5, col = "steelblue", lty = 2)   # candidate cutoff
  bf <- butter(n = 4, W = 10 / (50 / 2), type = "low") # where the 50 is the sampling rate
  for (col in paste0(base, ".med")) set(accel_data, j = paste0(sub("\\.med$", "", col), ".butt"), value = filtfilt(bf, accel_data[[col]]))
  
  # plot it to check the differences --------------------------------------
  # p0 <- ggplot(accel_data[1:10000,], aes(x = gps_time_est)) + geom_path(aes(y = RawAX.scaled, colour = "X")) + geom_path(aes(y = RawAY.scaled, colour = "Y")) + geom_path(aes(y = RawAZ.scaled, colour = "Z"))
  # p1 <- ggplot(accel_data[1:10000,], aes(x = gps_time_est)) + geom_path(aes(y = RawAX.med, colour = "X")) + geom_path(aes(y = RawAY.med, colour = "Y")) + geom_path(aes(y = RawAZ.med, colour = "Z"))
  # p2 <- ggplot(accel_data[1:10000,], aes(x = gps_time_est)) + geom_path(aes(y = RawAX.butt, colour = "X")) + geom_path(aes(y = RawAY.butt, colour = "Y")) + geom_path(aes(y = RawAZ.butt, colour = "Z"))
  # p0/p1/p2
  
  # select the columns to remove aand save the data
  accel_data[, c(base, paste0(base, ".scaled"), paste0(base, ".med")) := NULL]
  setnames(accel_data, paste0(base, ".butt"), base)
  
  # round them all to 4 digits
  accel_data[, (base) := lapply(.SD, round, 4), .SDcols = base]
  
  save(accel_data, file = file.path(collar_dir, "Board_Accel.RDA"), compress = FALSE)
  # load(file = file.path(collar_dir, "Board_Accel.RDA"))
}

# Check orientation -------------------------------------------
# figured out the orientations when I had made all the data once first
# did this manually and took notes in "Notes/Worklog.docx"
# they were all the same so I didnt have to change anything
# but here is approximately where that modification would be made

if(!file.exists(file.path(collar_dir, "Board_GPS.csv"))){
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
 if(!dir.exists(chunked_dir_path)){
   
   if(!exists("gps_data")){
     gps_data <- fread(file.path(collar_dir, "Board_GPS.csv"))
   }
   if(!exists("accel_data")){
     accel_data <- load(file = file.path(collar_dir, "Board_Accel.RDA"))
   }
   
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
    save(accel_data, file = file.path(chunked_dir_path, paste0("Board_Aligned_", d, ".RDA")), compress = TRUE)
  })

 }

rm("gps_data")
rm("accel_data")
