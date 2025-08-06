# Converting the board accel style to standardised format -----------------
# reading together and formatting the satellite time syncs
# Reading together the accelerometer files in groups of 10
# merging together such that accel times are updated to match GPS times

# Set up ------------------------------------------------------------------
library(tidyverse)
library(data.table)
library(zoo)
setDTthreads(0L) # make the fread function faster

# base_path <- "D:/ImpalaProject/RawData"

base_path <- "C:/Users/PC/Documents/ImpalaProject/RawData"

impalas <- basename(list.dirs(path = file.path(base_path), full.names = TRUE, recursive = FALSE))
# set up system for iterating through the different collars

for (ID in impalas){
 # ID <- "Collar_2"

# make the folder I need
# save_folder <- file.path(base_path, ID, "Board", "Synced")
# if (!dir.exists(save_folder)) {
#   dir.create(save_folder, recursive = TRUE)
# }

  # Satellite data ----------------------------------------------------------
  # read in all the GPS readings into a standardised file
  sat_files <- list.files(file.path(base_path, ID, "Board"),
                          pattern = "^serialLog.*", full.names = TRUE)
  gps_output <- file.path(base_path, ID, "Sat_board_GPS.csv")
  
  # run this once and then save it
  if (!file.exists(gps_output)){
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
          
          # Check the direct next line for GPS info
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
      
      gps_data <- do.call(rbind, lapply(results, as.data.frame))
    }))
    
    board_sat$internal_timestamp <- as.POSIXct(
      board_sat$internal_timestamp, format = "%m/%d/%Y %H:%M:%OS", tz = "Africa/Johannesburg")
    board_sat$gps_timestamp <- as.POSIXct(
      board_sat$gps_timestamp, format = "%d/%m/%Y %H:%M:%OS", tz = "Africa/Johannesburg")
    
    # save it
    fwrite(board_sat, gps_output)
    
  } else {
    # just read it in next time... was having so many R crashes on my laptop lmao.
    # switched to proper desktop and had 0 issues though
    board_sat <- fread(gps_output)
  }
  
  # extract and convert the times from the sat gps
  board_times <- board_sat[, c("internal_timestamp", "gps_timestamp")]
  board_times$internal_timestamp <- as.POSIXct(
     board_times$internal_timestamp, format = "%m/%d/%Y %H:%M:%OS", tz = "Africa/Johannesburg")
  board_times$gps_timestamp <- as.POSIXct(
     board_times$gps_timestamp, format = "%d/%m/%Y %H:%M:%OS", tz = "Africa/Johannesburg")
  
  # plot the rate of satellite hits (for debugging)
  # ggplot(board_times, aes(x = internal_timestamp, y  = gps_timestamp)) + geom_point()
  
  # Accelerometer data ------------------------------------------------------
  # combine with the GPS and change the times
  # because the files are so large, do one at a time
  # working memory constraints... annoying, but also prevents lost processing if it crashes
  # this takes yonks to run - just btw
  
  accel_files <- list.files(file.path(base_path, ID, "Board"),
      pattern = "^dataLog.*", full.names = TRUE)
  
  # initially I thought I would need to do them in chunks but looks like not...
  # Split files into chunks of 10 # or whatever you want
  # accel_chunks <- split(accel_files, ceiling(seq_along(accel_files) / 20))
  # for (i in seq_along(accel_chunks)) {
  #   chunk_files <- accel_chunks[[i]]
    
  chunk_files <- accel_files # try doing all of them at once
  
  # although most of the files look good, extremely rarely there is a misread and non-UTC-8 encoding
  # these false reads corrupt the data without corrupting the files
  # they will read with fread() BUT everything after the corruption will be weird
  # therefore, I follow this convoluted proceedure to clean them

  board_accel <- rbindlist(lapply(chunk_files, function(x) {
    tryCatch({
      # Attempt normal read first
      accel_data <- tryCatch({
        fread(x, sep = ",", header = TRUE, fill = TRUE, encoding = "UTF-8")
      }, error = function(e) {
        message("Corrupted file detected, attempting to clean: ", x)
        
        # Read lines manually and remove corrupted lines
        lines <- tryCatch(readLines(x, warn = FALSE), error = function(e) return(NULL))
        if (is.null(lines)) return(NULL)
        
        # Remove lines with unreadable characters
        clean_lines <- lines[!is.na(iconv(lines, from = "", to = "UTF-8", sub = NA))]
        
        if (length(clean_lines) == 0) {
          message("All lines are corrupted or unreadable in: ", x)
          return(NULL)
        }
        
        # Read cleaned lines into data.table
        fread(text = clean_lines, fill = TRUE)
      })
      
      # Ensure data.table
      setDT(accel_data)
      
      # Parse timestamp
      accel_data[, internal_timestamp := as.POSIXct(
        paste(rtcDate, rtcTime), format = "%m/%d/%Y %H:%M:%OS", tz = "Africa/Johannesburg"
      )]
      
      accel_data
    }, error = function(e) {
      message("Skipping file due to error: ", x)
      NULL
    })
  }), fill = TRUE)
  
  
  # plot this to check its not corrupted or weird
  accel_snip <- board_accel[seq(1, min(1000000000, nrow(board_accel)), by = 10), ] # downsample
  ggplot(accel_snip) + 
    geom_line(aes(x = internal_timestamp, y = RawAX), colour = "goldenrod") # +
  
  ggplot(accel_snip, aes(x = seq_len(nrow(accel_snip)), y = RawAX)) +
    geom_line(colour = "goldenrod") 
  
  
  
  
  
  # Bind to the GPS ---------------------------------------------------------
  # chaeck whether there is an overlap in the times between the GPS and the accel
  # if there isn't, skip this whole file chunk 
  # they're meant to be every 5 minutes
  bounds <- range(board_accel$internal_timestamp, na.rm = TRUE)
  
  any_in_range <- any(
    board_times$internal_timestamp >= bounds[1] &
      board_times$internal_timestamp <= bounds[2],
    na.rm = TRUE
  )
  
  if (any_in_range){
    joined <- merge(board_accel, board_times, by = "internal_timestamp", all = TRUE)
    #### NOTE ####
    # if you get the error with the "too many joins" then its likely due to NA times
    # there will be NAs in the date times if you've converted the dates wrong
    # for example, the first time I did this, I thought it was dd/mm/YYYY (instead of american way) 
    # and so some of the later dates were impossible and it gave a NA 
  } else {
    print("these dont match or they dont overlap")
  }
  
  # if they do match, then join them together
  # and then fill in the accel samples between accel hits with the cumulitive time since GPS hit
  # this way the times are always ~5min accuracy of sat
  joined <- joined[!is.na(RawAX)]
  
  # but then look whether there are any hits for inside this speciifc file
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
  } else {
    # there are no sat hits for inside this file, so we need to do soemthing
    print("There were no sat hits")
  }
  
  fwrite(joined, file.path(base_path, ID, "Synced_Board_Accel.csv"))
}
  

# # Read them togteher into a single file -----------------------------------
# board_accels <- list.files(file.path(base_path, ID, "Board", "Synced"), full.names = TRUE)
# 
# # read them all together
# board_accel_data <- rbindlist(lapply(board_accels, function(x) {
#   data <- fread(x)
#   data <- data[, .(adjusted_timestamp, RawAX, RawAY, RawAZ)]
#   data[, ID := ID]
#   data
# }))
# 
# # save that
# fwrite(board_accel_data, file.path(base_path, ID, "Synced_Board_Accel.csv"))


# Reset the envirnment before the next one --------------------------------
rm(list = ls())
gc()
  