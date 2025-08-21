# CombiningArtemisGPSFiles ------------------------------------------------

# Function to clean the GPS data ------------------------------------------
# there are a lot of misreads and garbled data in  the files
# the inconsistency of the formatting mean that we need to be careful before accepting the input
# for example, collar 8 did not have latitude in the correct place
# function from Chris' repo - I haven't modified it
parse_sat_file <- function(path) {
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

# Read in all the files into a standardised file --------------------------
sat_files <- list.files(accel_dir, pattern = "^serialLog.*", full.names = TRUE)
board_sat <- rbindlist(lapply(sat_files, parse_sat_file), use.names = TRUE, fill = TRUE)

# clean
board_sat[order(gps_timestamp), diff_s := as.numeric(gps_timestamp - shift(gps_timestamp), "secs")]
board_sat <- board_sat[complete.cases(board_sat), ]

gps_output <- file.path(accel_dir, "Board_GPS.csv")
fwrite(board_sat, gps_output)

