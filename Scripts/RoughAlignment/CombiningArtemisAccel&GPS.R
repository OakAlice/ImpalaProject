# Combing the accel and GPS -----------------------------------------------

# Load in and format the accel data ---------------------------------------
# load in the accel data # this takes a while # saves as accel_data
load(file.path(accel_dir, "Board_Accel.RDA"))
setDT(accel_data)

# remove the emoty column
accel_data[, V17 := NULL]

# Convert the internal timestamp
accel_data[, rtc_datetime := mdy_hms(paste(rtcDate, rtcTime), tz = "UTC")]

# and then check for duplicates
if (sum(duplicated(accel_data$rtc_datetime))>0){
  print("there are duplicates in the times recoded by the accel board")
}

# Read in the GPS data ----------------------------------------------------
gps_data <- fread(file.path(accel_dir, "Board_GPS.csv"))
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
# Chris and I did this in very different ways
# I added incremental seconds in case of inconsistent sampling rates and back-calculated from the first time
# Chris did an interpolation with assumption of cinsistent (or close enough) accel sampling and doesn't back-calculate from the first hit
# I've preserved both methods but have set the workflow to do it Chris' way for now

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
  
} else { 
  
  # oak's method
  joined <- merge(accel_data, gps_data, by = "internal_timestamp", all = TRUE)
  accel_data$internal_timestamp <- accel_data$rtc_datetime
  if (length(unique(joined$gps_timestamp)) > 1){
    setorder(joined, internal_timestamp)
    joined[, diff := as.numeric(internal_timestamp - shift(internal_timestamp), units = "secs")]
    joined[, gps_group := cumsum(!is.na(gps_timestamp))]
    joined[, anchor_time := na.locf(gps_timestamp, na.rm = FALSE)]
    joined[, anchor_id := rleid(anchor_time)]
    joined[, adjusted_time := anchor_time + cumsum(nafill(diff, fill = 0)), by = anchor_id]
    
    # and then going backwards from the first reading
    first_gps_row <- which(!is.na(joined$gps_timestamp))[1]
    first_gps_time <- joined$gps_timestamp[first_gps_row]
    joined[, adjusted_time_up := as.POSIXct(NA, tz = attr(internal_timestamp, "tzone"))]
    
    if (first_gps_row > 1) {
      rev_diffs <- rev(nafill(joined$diff[1:(first_gps_row - 1)], fill = 0))
      cum_diffs <- cumsum(rev_diffs)
      adjusted_times <- first_gps_time - cum_diffs
      joined[1:(first_gps_row - 1), adjusted_time_up := adjusted_times[rev(seq_along(adjusted_times))]]
    }
    
    # combine those two columsn into one
    joined[, adjusted_timestamp := fifelse(!is.na(adjusted_time), adjusted_time, adjusted_time_up)]
    # then just select the important columns
    # keeping more info than necessary so we dont have to repeat this
    joined <- joined[, c("internal_timestamp", "rtcDate", "rtcTime", "gps_timestamp", "adjusted_timestamp",
                         "RawAX", "RawAY", "RawAZ", "RawGX", "RawGY", "RawGZ", "RawMX", "RawMY", "RawMZ")]
  
  }
}

# Save the matched data ---------------------------------------------------
save(accel_data, file = file.path(accel_dir, "Board_Aligned.RDA"))

# Extract date from estimated GPS time
accel_data[, date := as.Date(gps_time_est)]
unique(accel_data$date)

# Split by date
accel_list <- split(accel_data, by = "date", keep.by = TRUE)

chunked_dir_path <- file.path(accel_dir, "Chunked")
if (!dir.exists(chunked_dir_path)) {
  dir.create(chunked_dir_path, recursive = TRUE)
}

# Save each day to a separate RDA file in the chunked folder
invisible(lapply(names(accel_list), function(d) {
  dt <- accel_list[[d]] 
  save(dt, file = file.path(chunked_dir_path, paste0("Board_Aligned_", d, ".RDA")))
}))

