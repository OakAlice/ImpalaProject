# Converting the board accel style to standardised format -----------------

library(tidyverse)
library(data.table)

base_path <- "D:/ImpalaProject/RawData"
impalas <- basename(list.dirs(path = file.path(base_path), full.names = TRUE, recursive = FALSE))

for (ID in impalas){
  # ID <- "Collar_2"
  
  # read in all the accelerometer readings
  accel_files <- list.files(file.path(base_path, ID, "Board"),
    pattern = "^dataLog.*", full.names = TRUE)
  
  # accel_files <- accel_files[1:6] # for troubleshooting
  
  # this takes yonks to run - just btw
  board_accel <- rbindlist(lapply(accel_files, function(x) {
    tryCatch({
      accel_data <- fread(x)[, .(rtcDate, rtcTime, RawAX, RawAY, RawAZ, RawGX, RawGY, RawGZ)]
      accel_data[, internal_timestamp := as.POSIXct(
        paste(rtcDate, rtcTime),
        format = "%d/%m/%Y %H:%M:%OS",
        tz = "Africa/Johannesburg"
      )]
      return(accel_data)
    }, error = function(e) {
      message("Skipping file due to read error: ", x)
      return(NULL)
    })
  }), fill = TRUE)
  
  # quickly write this out
  # fwrite(board_accel, file.path(base_path, ID, "Board_accelerometer_data.csv"))
  # fread(file.path(base_path, ID, "Board_accelerometer_data.csv"))
  
  # read in all the GPS readings
  sat_files <- list.files(file.path(base_path, ID, "Board"),
                            pattern = "^serialLog.*", full.names = TRUE)
  
  # sat_files <- sat_files[1:5] # debugging purposes
  
  # doesn't take as long
  board_sat <- rbindlist(lapply(sat_files, function(x){
    lines <- readLines(x)
    
    # Regular expressions for the times I want
    timestamp_pattern <- "^\\^(\\d{2}/\\d{2}/\\d{4}),(\\d{2}:\\d{2}:\\d{2}\\.\\d{2})$"
    gps_pattern <- "^(\\d{2}/\\d{2}/\\d{4} \\d{2}:\\d{2}:\\d{2}) - Lon:([0-9.-]+), Lat:([0-9.-]+)$"
    
    results <- list()
    
    # Loop through the lines to find where each of these occur
    for (i in seq_along(lines)) {
      line <- lines[i]
      
      # Check for the timestamp line
      if (grepl(timestamp_pattern, line)) {
        # Extract the first timestamp
        ts_match <- regmatches(line, regexec(timestamp_pattern, line))[[1]]
        internal_ts <- paste(ts_match[2], ts_match[3], sep = " ")
        
        # Check the next line for GPS info
        if (i + 1 <= length(lines) && grepl(gps_pattern, lines[i + 1])) {
          gps_match <- regmatches(lines[i + 1], regexec(gps_pattern, lines[i + 1]))[[1]]
          gps_ts <- gps_match[2]
          lon <- as.numeric(gps_match[3])
          lat <- as.numeric(gps_match[4])
          
          # Store result
          results[[length(results) + 1]] <- list(
            internal_timestamp = internal_ts,
            gps_timestamp = gps_ts,
            lon = lon,
            lat = lat
          )
        }
      }
    }
    
    # Convert list to data.frame
    gps_data <- do.call(rbind, lapply(results, as.data.frame))
  }))
  
  # save the GPS
  fwrite(board_sat, file.path(base_path, ID, "GPS_hits.csv"))
  
  # extract and convert the times from the sat gps
  board_times <- board_sat[, c("internal_timestamp", "gps_timestamp")]
  board_times$internal_timestamp <- as.POSIXct(
    board_times$internal_timestamp, format = "%d/%m/%Y %H:%M:%OS", tz = "Africa/Johannesburg")
  
  # the use the GPS Sat hits to realign the accelerometer
  # match them up by their internal readings
  
  joined <- merge(board_accel, board_times, by = "internal_timestamp", all = TRUE)
  
  
  
  
  
  
}