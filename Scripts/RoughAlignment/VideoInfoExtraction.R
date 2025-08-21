# Video and accel information extraction ----------------------------------

videos_list <- list.files(video_dir, pattern = "\\.(MTS|DJI|MOV|MP4)$", ignore.case = TRUE, full.names = TRUE, recursive = TRUE)

# Process videos
video_info <- data.frame()  # Reset for each cat
for (video in videos_list) {
  filename <- basename(video)
  dirname <- basename(dirname(video))
    
  Video_mtime <- file.info(video)$mtime
  Dur_video_sec <- av_media_info(video)$duration
    
    # apply timestamp conversion based on the camera it came from
    ##### ADD HERE
    
  # Create temporary dataframe for this video
  temp_video_info <- data.frame(
    individual = Collar,
    date = as.Date(Video_mtime),
    camera = dirname,
    filename = filename,
    mtime = Video_mtime,
    duration_sec = Dur_video_sec,
    stringsAsFactors = FALSE
  )
    
  # Append to this cat's video info
  video_info <- rbind(video_info, temp_video_info)
}
  
fwrite(video_info, file.path(video_dir, "Video_metadata.csv"))

