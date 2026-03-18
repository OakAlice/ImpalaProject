# Combing the accelerometer and GPS data ----------------------------------
# Adding the two different data sources together
# Considering that they are read from the same board, this would presumably be very easy
# But then what would be the fun in it?
# Instead use this script to calculate the time conversions and then stitch based on that

# Note that the gps and ionternal timestamp SHOULD increment at the same rate but they do not
# sometimes there is a read error where the internal time gets "stuck"
# To correct for this we find the line of best fit where we find the average relationship between the inernal and external times
# and then use this equation to estimate the external times

if(!file.exists(file.path(base_path, Collar, "Artemis_Aligned.RDA"))){
  
  # load in what we created earlier frm the gps
  gps_data <- fread(file.path(base_path, Collar, "Artemis_GPS.csv"))
  
  # calculate the gps time conversion
  # As far as I know this step isnt necessary, but just in case lol
  internal_sec <- as.numeric(gps_data$internal_timestamp)
  gps_data$gps_sec <- as.numeric(gps_data$gps_timestamp)
  best_fit <- lm(gps_data$gps_sec ~ internal_sec -1) # no intercept
  #ggplot(gps_data, aes(x = internal_sec, y = gps_sec)) + geom_point()
  
  # now predict this onto the accel data
  load(file.path(base_path, Collar, "Artemis_Accel.RDA")) # comes in as accel_data
  accel_sec <- as.numeric(accel_data$rtc_datetime)
  updated_accel_sec <- accel_sec * best_fit$coefficients
  accel_data$updated_accel_sec <- updated_accel_sec
  #ggplot(accel_data, aes(x = rtc_datetime , y = updated_accel_sec)) + geom_point()
  
  # Combing the GPS and Accel files based on timestamp ----------------------
  # saves all aligned data as a single RDA with aligned timestamps
  accel_data <- combine_accel_GPS(Collar, accel_data, gps_data)
  setcolorder(accel_data, c("updated_accel_time", "rtc_datetime", "internal_timestamp", "gps_timestamp", "reset_events",
                            "lon", "lat", 
                            "Q9_1", "Q9_2", "Q9_3", "HeadAcc",
                            "RawAX", "RawAY", "RawAZ", "RawGX", "RawGY", "RawGZ", "RawMX", "RawMY", "RawMZ"
  ))
  
  
  save(accel_data, file = file.path(base_path, Collar, "Artemis_Aligned.RDA"), compress = FALSE) # make it faster to read and write
  
} else { # else load it in
  print("just loading in all the aligned data that was made before")
  load(file.path(Collar, "Artemis_Aligned.RDA")) # this will come in as accel_data
}

# now split the data into 24 hr periods and save each of those independently
# Extract date from estimated GPS time
setDT(accel_data)
accel_data[, date := as.Date(updated_accel_time)]
unique(accel_data$date)

chunked_dir_path <- file.path(base_path, Collar, "ArtemisAlignedChunked")

if(!file.exists(file.path(chunked_dir_path, paste0("Board_Aligned_", unique(accel_data$date)[1], ".RDA")))){
  if (!dir.exists(chunked_dir_path)) {
    dir.create(chunked_dir_path, recursive = TRUE)
  }
  
  # Split by date
  data_list <- split(accel_data, by = "date", keep.by = TRUE)
  
  # Save each day to a separate RDA file in the chunked folder
  lapply(names(data_list), function(d) {
    day_data <- data_list[[d]]
    save(day_data, file = file.path(chunked_dir_path, paste0("Board_Aligned_", d, ".RDA")))
  })
} else {
  print("the data has already been split into days")
}

# clean the workspace
if (exists("accel_data")) rm(accel_data)
if (exists("data_list")) rm(data_list)
if (exists("gps_data")) rm(gps_data)
gc()
