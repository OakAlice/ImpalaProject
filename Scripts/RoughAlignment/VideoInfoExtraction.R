# Video and accel information extraction ----------------------------------

videos_list <- list.files(video_dir, pattern = "\\.(MTS|DJI|MOV|MP4)$", ignore.case = TRUE, full.names = TRUE, recursive = TRUE)

# Extract the base metadata -----------------------------------------------
video_info <- data.frame()  # Reset for each
for (video in videos_list) {
  filename <- basename(video)
  dirname <- basename(dirname(video))
  dirdirname <- basename(dirname(dirname(video)))
    
  Video_mtime <- file.info(video)$mtime
  Dur_video_sec <- av_media_info(video)$duration
    
  # Create temporary dataframe for this video
  temp_video_info <- data.frame(
    individual = Collar,
    date = dirdirname,
    camera = dirname,
    filename = filename,
    mtime = Video_mtime,
    duration_sec = Dur_video_sec,
    stringsAsFactors = FALSE
  )
    
  # Append to this cat's video info
  video_info <- rbind(video_info, temp_video_info)
}

# Conversions to the timestamps -------------------------------------------
# this will be unique to the camera you're working with
video_info$start_time <- as.POSIXct(NA, tz = "UTC")
video_info$timezone <- NA 

for (i in seq_len(nrow(video_info))) {
  row <- video_info[i, ]
  
  if (grepl("Drone", row$camera)) {
    # strip the video time out of the name
    time_string <- strsplit(row$filename, "_")[[1]][2]
    video_info$start_time[i] <- as.POSIXct(time_string, format = "%Y%m%d%H%M%S", tz = "UTC")
    video_info$timezone[i] <- "UTC"
    
  } else if (row$camera == "ChrisPhone") {
    # strip the date and time
    parts <- strsplit(row$filename, "_")[[1]]
    date_string <- parts[1]
    time_string <- parts[2]
    video_info$start_time[i] <- as.POSIXct(paste0(date_string, time_string), format = "%Y%m%d%H%M%S", tz = "Africa/Johannesburg")
    video_info$timezone[i] <- "Africa/Johannesburg"
  
  } else if (grepl("Robin", row$camera)){
    # extracting the mediainfo information
    # this extracts the time weirdly
    # av extracts the time as "AEST" even when it is UTC
    AEST_Time <- row$mtime - hours(8)
    video_info$start_time[i] <- lubridate::force_tz(AEST_Time, "UTC") 
    video_info$timezone[i] <- "UTC"
    
  } else {
    # hasn't been calculated for any other methods yet
    print("I haven't calculated this for these cameras yet")
  }
}

# save results
fwrite(video_info, file.path(video_dir, "Video_metadata.csv"))
