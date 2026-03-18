# Function to combine the artemis accelerometer files ---------------------
# adapted from Chris Clemente -> https://github.com/cclemente/Collar_data_extraction/blob/main/Step1a_Read_in_raw_accel_files.R
stitch_artemis_accels <- function(accel_files){
  
  file_nums <- as.integer(gsub("\\D", "", basename(accel_files))) # order them approporiately
  accel_files <- accel_files[order(file_nums)]
  
  bad_files <- c()  # store skipped files
  
  accel_data <- rbindlist(
    lapply(accel_files, function(f) {
      if (file.size(f) == 0) {
        message("Skipping empty file: ", f)
        bad_files <<- c(bad_files, f)
        return(NULL)
      }
      dt <- tryCatch(
        fread(f, skip = 0),
        error = function(e) {
          message("Skipping unreadable file: ", f, " (", e$message, ")")
          bad_files <<- c(bad_files, f)
          return(NULL)
        }
      )
      dt
    }),
    use.names = TRUE,
    fill = TRUE
  )
  
  return(accel_data)
}

# Combining the GPS files -------------------------------------------------
# code from Chris
clean_artemis_GPS <- function(path) {
  # read in the raw data
  raw <- readBin(path, what = "raw", n = file.info(path)$size)
  raw[raw == as.raw(0)] <- as.raw(32)                       # NUL -> space
  keep <- (raw >= as.raw(32) & raw <= as.raw(126)) | raw %in% c(as.raw(10), as.raw(13))
  txt  <- rawToChar(raw[keep])
  txt  <- gsub("\r\n?", "\n", txt, useBytes = TRUE)
  lines <- unlist(strsplit(txt, "\n", fixed = TRUE), use.names = FALSE)
  
  # grep patterns for the specific data we are trying to extract
  re_lat_only <- "^\\s*Lat:([+-]?\\d+(?:\\.\\d+)?)\\s*$"
  re_lon_line <- "^\\s*(\\d{2}/\\d{2}/\\d{4}\\s+\\d{2}:\\d{2}:\\d{2})\\s*-\\s*Lon:([+-]?\\d+(?:\\.\\d+)?),?\\s*(?:Lat:([+-]?\\d+(?:\\.\\d+)?))?\\s*$"
  re_rtc_line <- "^\\s*\\^\\s*(\\d{2}/\\d{2}/\\d{4})\\s*,\\s*(\\d{2}:\\d{2}:\\d{2}(?:\\.\\d{1,2})?)\\s*$"
  
  out <- vector("list", length(lines))
  k <- 0
  
  # extract lat from nearby lines (prev/next up to 2)
  find_neighbor_lat <- function(idx) {
    # prefer previous line, then next lines (skip blank/rtc)
    # search order: i-1, i-2, i+1, i+2
    ord <- c(idx-1L, idx-2L, idx+1L, idx+2L)
    ord <- ord[ord >= 1 & ord <= length(lines)]
    for (j in ord) {
      lj <- lines[j]
      mlat <- str_match(lj, re_lat_only)
      if (!is.na(mlat[1,1])) return(as.numeric(mlat[1,2]))
      # also tolerate a "Lat:" trailing after a comma-only line, e.g. "...,", then "Lat:..."
    }
    return(NA_real_)
  }
  
  i <- 1L
  while (i <= length(lines)) {
    li <- lines[i]
    
    # Case A: the standard lon line (may or may not include Lat)
    m <- str_match(li, re_lon_line)
    if (!is.na(m[1,1])) {
      gps_ts_str <- m[1,2]
      lon_val    <- as.numeric(m[1,3])
      lat_val    <- if (!is.na(m[1,4])) as.numeric(m[1,4]) else NA_real_
      
      # If Lat wasn't on the same line, try nearby lines (handles your Lat-then-Lon case)
      if (is.na(lat_val)) lat_val <- find_neighbor_lat(i)
      
      # Look ahead for RTC (up to 3 lines, but stop if we hit next lon record)
      rtc_str <- NA_character_
      for (j in seq.int(i+1L, min(i+3L, length(lines)))) {
        if (j > length(lines)) break
        lj <- lines[j]
        if (nzchar(lj)) {
          # Stop early if a new GPS block appears
          if (!is.na(str_match(lj, re_lon_line)[1,1])) break
          mrtc <- str_match(lj, re_rtc_line)
          if (!is.na(mrtc[1,1])) {
            rtc_str <- paste(mrtc[1,2], mrtc[1,3])
            break
          }
        }
      }
      
      # Record if we have enough fields
      if (!is.na(lat_val) && !is.na(rtc_str)) {
        k <- k + 1
        out[[k]] <- list(
          # note that the dates are in DIFFERENT formats
          # there is american AND normal formats in the same txt file!!
          internal_timestamp_raw = rtc_str,          # mm/dd/yyyy hh:mm:ss.s
          gps_timestamp_raw      = gps_ts_str,       # dd/mm/yyyy hh:mm:ss
          lon = lon_val,
          lat = lat_val
        )
      }
      
      i <- i + 1L
      next
    }
    
    # Case B: early-file pattern may start with a Lat-only line; just move on.
    # We'll bind it when we encounter the Lon line that follows.
    i <- i + 1L
  }
  
  if (k == 0) return(NULL)
  dt <- rbindlist(out[seq_len(k)])
  
  # Parse timestamps (note the different formats)
  dt[, internal_timestamp := as.POSIXct(internal_timestamp_raw, format = "%m/%d/%Y %H:%M:%OS", tz = "UTC")]
  dt[, gps_timestamp      := as.POSIXct(gps_timestamp_raw,      format = "%d/%m/%Y %H:%M:%S",  tz = "UTC")]
  
  dt[, .(internal_timestamp, gps_timestamp, lon, lat)]
}

stitch_artemis_gps <- function(gps_files){
  gps_data <- lapply(gps_files, clean_artemis_GPS)
  gps_data <- rbindlist(gps_data, use.names = TRUE, fill = TRUE)
  return(gps_data)
}

# Combine gps and accel together ------------------------------------------

combine_accel_GPS <- function(example_dir, accel_data, gps_data){
  
  setDT(accel_data)
  setDT(gps_data)
  
  # check the reset events
  if (max(accel_data$reset_events) != max(gps_data$reset_events)){
    print("stop, there's soemthing wrong - the devices have a different number of resets")
    print("removing the short resets from the accelerometer dataset")
    
    min_dur <- sample_rate * 60 * 60 # minimum duration is 1 hour
    
    # keep only the reset events that exceed the minimyum duration
    keep <- accel_data[, .I[.N >= min_dur], by = reset_events]$V1
    accel_data <- accel_data[keep]
    
    # and now reset the reset count back to 
    accel_data[, reset_events := .GRP - 1L, by = reset_events]
    
    # now check again
    if (max(accel_data$reset_events) != max(gps_data$reset_events)){
      print("stop, there's still soemthing wrong - the devices still have a different number of resets")
      print("stop and manually figure this out")
    } else {
      print("devices now have matching number of resets")
    }
    
  } else {
    print("devices were reset the same number of times")
  }
  
  # Match timestamps in the accelerometer and GPS ---------------------------
  setkey(accel_data, reset_events, updated_accel_sec)
  setkey(gps_data, reset_events, gps_sec)
  
  # check whether there are matches and print if there arent # debugging step
  bounds <- range(accel_data$updated_accel_sec, na.rm = TRUE)
  
  any_in_range <- any(
    gps_data$gps_sec >= bounds[1] &
      gps_data$gps_sec <= bounds[2],
    na.rm = TRUE
  )
  
  if (any_in_range) {
    
    gps_data[, gps_row_id := .I]
    
    # find the single closest accel row to each GPS point
    closest_idx <- accel_data[gps_data,
                              on = .(reset_events, updated_accel_sec = gps_sec),
                              roll = "nearest",
                              which = TRUE]
    
    accel_matched <- copy(accel_data)
    accel_matched[, gps_row_id := NA_integer_]
    accel_matched[closest_idx, gps_row_id := gps_data$gps_row_id]
    
    result <- gps_data[accel_matched,
                       on = .(reset_events, gps_row_id)]
    
  } else {
    print("GPS and accel data do not overlap in times")
  }
  
  # Should be the same as the number of GPS hits, check whether that's the case
  if (length(na.omit(result$lon)) != nrow(gps_data)){
    print("there is something funny going on... not all the GPS hits have a match...")
  }
  
  # Convert back to POSIXct
  result[, updated_accel_time := as.POSIXct(updated_accel_sec, tz = "UTC")]
  
  # clean thw whole thing up
  result[, c("gps_diff", "internal_diff", "gps_sec", "gps_row_id", "updated_accel_sec") := NULL]
  
  # return it. Though warning, this can be massive
  return(result)
}

# Looking for time skips in the GPS data ----------------------------------
gps_diagnostics <- function(gps_data){
  
  gps_jumps <- gps_data %>%
    arrange(gps_timestamp) %>%
    mutate(time_diff = difftime(internal_timestamp, lag(internal_timestamp), units = "min"), # had to define package or errored
           break_point = ifelse(as.numeric(time_diff) > 6 | as.numeric(time_diff) < 0 , 1, 0), # if bigger than 6 minutes or -ve... then count as break
           break_point = replace_na(break_point, 0),
           sequence = cumsum(break_point)) %>%
    select(-time_diff) %>%
    ungroup() %>%
    mutate(difference_gps = round(as.numeric(
      difftime(gps_timestamp, lag(gps_timestamp), units = "mins")
    ), 1)) %>%
    mutate(difference_internal = round(as.numeric(
      difftime(internal_timestamp, lag(internal_timestamp), units = "mins")
    ), 1)) %>%
    mutate(read_error = difference_gps - difference_internal)
  
  return(gps_jumps)
}