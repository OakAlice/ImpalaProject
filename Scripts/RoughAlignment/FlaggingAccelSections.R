# Flagging the accelerometer sections -------------------------------------
library(data.table)
library(tidyverse)
library(lubridate)

accel_type <- "Board" # "Axivity"

base_path <- "C:/Users/PC/Documents/ImpalaProject/RawData"

for (ID in unique(videos$individual)){
  
  # ID <- "Collar_7"
  
  mediainfo <- FALSE
  if (mediainfo == TRUE){
    # load in the file created with mediainfo
    # videos <- fread(file.path(base_path, ID, "Videos", "video_metadata.csv"))
    # just definitely make sure it is formatted properly
    videos$end_times <- as.POSIXct(videos$`General Encoded_Date`, format ="%Y-%m-%d %H:%M:%OS", tz = "UTC")
    
    # the durations are all weird, so convert them into usable format
    convert_duration_to_seconds <- function(x) {
      x[x == ""] <- NA
      
      # Extract numeric parts for minutes, seconds, milliseconds
      mins <- as.numeric(sub(".*?(\\d+) min.*", "\\1", x))
      secs <- as.numeric(sub(".*?(\\d+) s.*", "\\1", x))
      ms   <- as.numeric(sub(".*?(\\d+) ms.*", "\\1", x))
      
      # Set non-matching parts to 0
      mins[is.na(mins)] <- 0
      secs[is.na(secs)] <- 0
      ms[is.na(ms)]     <- 0
      
      # Return total duration in seconds
      mins * 60 + secs + ms / 1000
    }
    
    videos$duration_sec <- convert_duration_to_seconds(videos$`General Duration/String`)
    videos$start_times <- videos$end_times - videos$duration_sec
    
  } else {
    # load in the file made with R av and file.info
    videos <- fread(file.path(base_path, "Video_info.csv"), header = TRUE)
    relevant_videos <- videos %>% filter(individual == ID)
    
    # they are in UTC time
    relevant_videos$start_time <- as.POSIXct(relevant_videos$start_time, format ="%Y-%m-%d %H:%M:%OS", tz = "UTC")
    relevant_videos$end_time <- as.POSIXct(relevant_videos$end_time, format ="%Y-%m-%d %H:%M:%OS", tz = "UTC")
  }
  
  # load in the accelerometer
  if (accel_type == "Axivity"){
    accel_name <- list.files(file.path(base_path, ID, "Axivity"), pattern = "*?.csv", full.names = TRUE)
    accel_data <- fread(accel_name)
    
    # convert the accel time according to whatever rule you figured out
    # in this case, I needed to do the standard conversion - 2 hrs
    accel_data$Time <- as.POSIXct((accel_data$V1 - 719529)*86400 - 2*3600, origin = "1970-01-01", tz = "Africa/Johannesburg")
    colnames(accel_data) <- c("X", "Y", "Z", "Time")
    
  } else {
    # read select and rename
    accel_data <- fread(file.path(base_path, ID, "Synced_Board_Accel.csv"))
    accel_data <- accel_data[, c("adjusted_timestamp", "RawAX", "RawAY", "RawAZ")]
    colnames(accel_data) <- c("Time", "X", "Y", "Z")
    # convert the time from UTC to local
    # accel_data$Time <- with_tz(accel_data$UTCTime, tzone = "Africa/Johannesburg")
    accel_data$MatTime <- as.numeric(accel_data$Time) / 86400 + 719529
  }
  
  # Do all flagging
  setDT(accel_data)
  setDT(relevant_videos)
  
  # Perform non-equi join to match V1 between start and end
  # this is the fastest way to do it I think?
  # hard because files are so massive
  setnames(relevant_videos, c("start_time", "end_time", "filename"),
           c("start", "end", "filename"))
  
  # Order relevant_videos by decreasing duration to prioritise longer duration videos
  setorder(relevant_videos, -duration_sec)
  
  # Assign Flags only if not already set
  accel_data[, Flags := NA_character_]
  
  # join
  for (i in seq_len(nrow(relevant_videos))) {
    s <- relevant_videos$start[i]
    e <- relevant_videos$end[i]
    f <- relevant_videos$filename[i]
    
    accel_data[is.na(Flags) & Time >= s & Time <= e, Flags := f]
  }
  
  # quickly check the data
  # plot_accel_data <- accel_data[1:100000000,] %>%
  #    filter(row_number() %% 50 == 1) %>%
  #    select(Time, X)
  # 
  #  ggplot(plot_accel_data) +
  #    geom_point(aes(x = Time, y = as.numeric(X))) +
  #    theme_minimal()
  # 
  
  # clip each of these flagged sections and save as their own thing
  
  # Get unique flags (excluding NA and empty strings)
  unique_flags <- unique(accel_data$Flags)
  unique_flags <- unique_flags[!is.na(unique_flags) & unique_flags != ""]
  
  # sanity check statements
  print(paste("ID:", ID, "Flagged rows:", sum(!is.na(accel_data$Flags))))
  print(unique_flags)
  
  # For each flag, extract and save corresponding data
  for (flag in unique_flags) {
    # Extract rows for this flag with buffer before and after
    flag_rows <- which(accel_data$Flags == flag)
    start_row <- max(1, min(flag_rows) - 100)  # dont go below 1 for the early videos though
    end_row <- min(nrow(accel_data), max(flag_rows) + 100)  # logic to prevent sampling too much
    flag_data <- accel_data[start_row:end_row,]
    
    flag_data <- flag_data[, c("MatTime", "X", "Y", "Z")]
    
    # Create output filename
    output_name <- gsub("\\.(MOV|DJI|MTS|MP4)$", "", flag, ignore.case = TRUE)
    output_name <- paste0(output_name, "_clip.csv")
    output_path <- file.path(base_path, ID, accel_type, "Clipped")
    
    if (!dir.exists(output_path)) {
      dir.create(output_path, recursive = TRUE)
    }
    
    # Save to CSV
    print(paste("saving: ", output_name))
    fwrite(flag_data, file.path(output_path, output_name))
  }
}
