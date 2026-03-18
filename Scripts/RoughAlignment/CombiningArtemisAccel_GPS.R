# Combing the accel and GPS -----------------------------------------------
# original version (which I know worked for the imapalas but not the kangaroo)

# Load in and format the accel data ---------------------------------------
# load in the accel data # this takes a while # saves as accel_data
load(file.path(base_path, Collar, "Artemis_Accel.RDA"))
setDT(accel_data)

# Read in the GPS data ----------------------------------------------------
gps_data <- fread(file.path(base_path, Collar, "Artemis_GPS.csv"))
setDT(gps_data)

# convert the times to POSIXct again (just in case) # set them all to UTC
gps_data[, internal_timestamp := as.POSIXct(internal_timestamp, tz = "UTC")]
gps_data[, gps_timestamp := as.POSIXct(gps_timestamp, tz = "UTC")]


# Match timestamps in the accelerometer and GPS ---------------------------
setkey(accel_data, rtc_datetime)
setkey(gps_data, internal_timestamp)

# check whether there are matches and print if there arent # debugging step
bounds <- range(accel_data$rtc_datetime, na.rm = TRUE)

any_in_range <- any(
  gps_data$internal_timestamp >= bounds[1] &
    gps_data$internal_timestamp <= bounds[2],
  na.rm = TRUE
)

if (any_in_range){
  # joined <- merge(accel_data, gps_data, by = "internal_timestamp", all = TRUE)
  # previously I was doing an exact match but Chris realised that this was discarding a lot of matches
  # therefore he moved to nearest match
  accel_data[, gps_flag := FALSE]
  
  accel_data[gps_data,
             on = .(rtc_datetime = internal_timestamp),
             roll = "nearest",
             mult = "first",
             `:=`(
               gps_timestamp = i.gps_timestamp,
               gps_lon       = i.lon,
               gps_lat       = i.lat,
               gps_flag      = TRUE
             )
  ]
} else {
  print("these dont match or they dont overlap")
}

# Should be the same as the number of GPS hits, check whether that's the case
if (sum(accel_data$gps_flag) != nrow(gps_data)){
  print("there is something funny going on... not all the GPS hits have a match")
}

# Fill in GPS times between the sat hits ----------------------------------
# Chris did an interpolation with assumption of consistent (or close enough) accel sampling and doesn't back-calculate from the first hit

method <- "interpolation"
if (method == "interpolation"){
  # Convert GPS times to numeric seconds
  accel_data[, gps_time_sec := as.numeric(gps_timestamp)]
  # Interpolate GPS times linearly
  accel_data[, gps_time_est_sec := na.approx(gps_time_sec, na.rm = FALSE)]
  
  # Convert back to POSIXct
  accel_data[, gps_time_est := as.POSIXct(gps_time_est_sec, origin = "1970-01-01", tz = "UTC")]
  # clean
  accel_data[, c("gps_time_sec", "gps_time_est_sec") := NULL]
}

# Save the matched data ---------------------------------------------------
save(accel_data, file = file.path(accel_dir, "Artemis_Aligned.RDA"))

# Extract date from estimated GPS time
accel_data[, date := as.Date(gps_time_est)]
unique(accel_data$date)

# Split by date
accel_list <- split(accel_data, by = "date", keep.by = TRUE)

chunked_dir_path <- file.path(base_path, Collar, "ArtemisAlignedChunked")
if (!dir.exists(chunked_dir_path)) {
  dir.create(chunked_dir_path, recursive = TRUE)
}

# Save each day to a separate RDA file in the chunked folder
lapply(names(accel_list), function(d) {
  accel_data <- accel_list[[d]]
  save(accel_data, file = file.path(chunked_dir_path, paste0("Board_Aligned_", d, ".RDA")))
})
