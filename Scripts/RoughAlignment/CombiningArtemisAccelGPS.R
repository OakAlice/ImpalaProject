# Combing the accel and GPS -----------------------------------------------

# Load in and format the accel data ---------------------------------------
# load in the accel data # this takes a while # saves as accel_data
load(file.path(collar_dir, "Board_Accel.RDA"))
setDT(accel_data)
# accel_data2 <- accel_data
# accel_data <- accel_data[1:20000000,]

# count rows per reset_event and display
reset_counts <- accel_data[, .N, by = reset_events][order(-N)]
print(reset_counts)
longest_reset <- reset_counts[1, reset_events]
print(paste0("for accelerometer, keeping only reset tvent #", longest_reset))
accel_data <- accel_data[reset_events == longest_reset]

# Read in the GPS data ----------------------------------------------------
gps_data <- fread(file.path(collar_dir, "Board_GPS.csv"))
setDT(gps_data)
  
# check whether the GPS times all increment by an expected amount
gps_data[, int_time_diff := c(NA_real_, diff(unclass(internal_timestamp))), by = reset_events]
gps_data[, ext_time_diff := c(NA_real_, diff(unclass(gps_timestamp))), by = reset_events]
gps_data[, inc_diff := int_time_diff - ext_time_diff]

if (any(gps_data$inc_diff > 60 | gps_data$inc_diff < -60, na.rm = TRUE)) {
  print("there is something doesn't increment normally... it's more than a minute out (not incl. reset events")
}

# count rows per reset_event and display
reset_counts <- gps_data[, .N, by = reset_events][order(-N)]
print(reset_counts)
longest_reset <- reset_counts[1, reset_events]
print(paste0("for gps, keeping only reset tvent #", longest_reset))
gps_data <- gps_data[reset_events == longest_reset]

# Match timestamps in the accelerometer and GPS ---------------------------
setkey(accel_data, rtc_datetime, reset_events)
setkey(gps_data, internal_timestamp, reset_events)

# check whether there are matches and print if there arent # debugging step
bounds <- range(accel_data$rtc_datetime, na.rm = TRUE)

any_in_range <- any(
  gps_data$internal_timestamp >= bounds[1] &
    gps_data$internal_timestamp <= bounds[2],
  na.rm = TRUE
)

if (any_in_range){
  # nearest match
  accel_data[, gps_flag := FALSE]
  
  accel_data[gps_data,
             on = .(rtc_datetime = internal_timestamp, reset_events = reset_events),
             roll = "nearest",
             mult = "first",
             `:=`(
               gps_timestamp             = i.gps_timestamp,
               lon                       = i.lon,
               lat                       = i.lat,
               num_gps_int_datetime      = i.numeric_internal_datetime,
               num_gps_datetime          = i.numeric_gps_datetime,
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
  # Interpolate GPS times linearly
  accel_data[, gps_time_est_sec := na.approx(num_gps_datetime, na.rm = FALSE)]
  # Convert back to POSIXct
  accel_data[, gps_time_est := as.POSIXct(gps_time_est_sec, origin = "1970-01-01", tz = "UTC")]
}

# Save the matched data ---------------------------------------------------
save(accel_data, file = file.path(collar_dir, "Board_Aligned.RDA"))

# Extract date from estimated GPS time
accel_data[, date := as.Date(gps_time_est)]
unique(accel_data$date)

# Split by date
accel_list <- split(accel_data, by = "date", keep.by = TRUE)

chunked_dir_path <- file.path(collar_dir, "Chunked")
if (!dir.exists(chunked_dir_path)) {
  dir.create(chunked_dir_path, recursive = TRUE)
}

# Save each day to a separate RDA file in the chunked folder
lapply(names(accel_list), function(d) {
  accel_data <- accel_list[[d]]
  save(accel_data, file = file.path(chunked_dir_path, paste0("Board_Aligned_", d, ".RDA")))
})
