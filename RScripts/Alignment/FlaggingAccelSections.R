# Flagging the accelerometer sections -------------------------------------

alignment_guide <- fread(file.path(base_path, "RawData", "Alignment_guide.csv"))

for (cats_name in unique(alignment_guide$cat)){
  
  accel_name <- list.files(file.path(base_path, "RawData", cats_name), pattern = "*?.csv", full.names = TRUE)

  # Get this cat's video frame info
  cat_frames <- alignment_guide[alignment_guide$cat == cats_name,]
  
  # Create a column for flagging the possible video
  accel_data$Flags <- NA
  
  # Track if we need padding at the start
  padding_needed <- 0
  
  # First pass: do all flagging
  for(i in 1:nrow(cat_frames)) {
    video_start <- cat_frames$start_frame[i]
    video_end <- cat_frames$end_frame[i]
    video_name <- cat_frames$filename[i]
    
    # Track the maximum padding needed
    if(video_start < 1) {
      padding_needed <- max(padding_needed, abs(video_start))
      next  # Skip this video for now, we'll handle it after padding
    }

    # print(sprintf("Video %s: start=%.0f, end=%.0f, data_rows=%d", 
    #              video_name, video_start, video_end, nrow(accel_data)))
    
    # Check if video_end exceeds data length (if vid kept going after accel dead)
    if(video_end > nrow(accel_data)) {
      print(sprintf("Video %s ends at frame %.0f but data only has %.0f rows - making adjustments", 
                     video_name, video_end, nrow(accel_data)))
      video_start <- nrow(accel_data)
      video_end <- nrow(accel_data)
    }
    
    # Flag rows that fall within this video's timeframe
    accel_data$Flags[video_start:video_end] <- video_name
  }
  
  # Add padding if needed
  if(padding_needed > 0) {
    padding <- data.frame(
      Time = rep(0, padding_needed),
      X = rep(0, padding_needed),
      Y = rep(0, padding_needed),
      Z = rep(0, padding_needed),
      Flags = NA
    )
    
    # Combine padding with existing data
    accel_data <- rbind(padding, accel_data)
    
    # Second pass: flag videos that start before 1
    for(i in 1:nrow(cat_frames)) {
      video_start <- cat_frames$start_frame[i]
      video_end <- cat_frames$end_frame[i]
      video_name <- cat_frames$filename[i]
      
      if(video_start < 1) {
        # Adjust indices to account for this new padding
        new_start <- 1
        new_end <- video_end + padding_needed
        accel_data$Flags[new_start:new_end] <- video_name
      }
    }
  }
  
  # Save the flagged data back to file under a new name
  new_accel_name <- file.path(base_path, "RawData", cats_name, paste0(cats_name, "_flagged.csv")) 
  fwrite(accel_data, new_accel_name, row.names = FALSE)
}
