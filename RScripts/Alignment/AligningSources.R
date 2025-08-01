# Aligning Video and Accel ------------------------------------------------
# load in the infomration I have amassed from other sources

# this was manually generated
sync_details <- fread(file.path(base_path, "RawData", "Sync_details.csv"))
# this was generated in VideoAndAccelInfoExtraction.R
accel_info <- fread(file.path(base_path, "RawData", "Accel_info.csv"))
video_info <- fread(file.path(base_path, "RawData", "Video_info.csv"))

# combine these information types in order to get the true starting and end times
# from each video (due to the cameras probably being incorrectly set)
# in the past we have calculated this in terms of frames but the drift was bad
# now we are calculating this in terms of seconds (which we hope will be more stable)


# Add the correct time into video_info in right format --------------------
# get the accelerometer time at the sync frame
# assign to be posixct or it will rever to numeric
sync_details$sync_accel_time <- as.POSIXct(NA, tz = "Australia/Brisbane")
  
for (sync_video in unique(sync_details$sync_video)){

  cat <- sync_details$cat[sync_details$sync_video == sync_video][1]
  sync_row <- sync_details$sync_accel_frame[sync_details$sync_video == sync_video][1]
  
  if (cat %in% unique(accel_info$cat)){
    file <- accel_info$filename[accel_info$cat == cat][1]
    sync_time_frac <- read.csv(file, skip = sync_row-1, nrows = 1)[,1]
    sync_time <- as.POSIXct((sync_time_frac - 719529)*86400-36000, origin = "1970-01-01", tz = "Australia/Brisbane")
    sync_details$sync_accel_time[sync_details$sync_video == sync_video] <- sync_time
    
    # then calculate the start time of the video
    sync_time_secs <- sync_details$sync_video_time[sync_details$sync_video == sync_video]
    sync_details$sync_video_start_time[sync_details$sync_video == sync_video] <- sync_time - as.difftime(sync_time_secs, units = "secs")
    
  } else {
    print("no sync file or no accelerometer or something")
    sync_details$sync_accel_time[sync_details$sync_video == sync_video] <- as.POSIXct(NA, tz = "Australia/Brisbane")
    sync_details$sync_video_start_time[sync_details$sync_video == sync_video] <- as.POSIXct(NA, tz = "Australia/Brisbane")
  }
  
}

# Update time of sync video -----------------------------------------------
# add this new start time in
# force to posixct
video_info$adjusted_start_time <- as.POSIXct(rep(NA, nrow(video_info)), origin = "1970-01-01", tz = "Australia/Brisbane")

for (cat_name in unique(sync_details$cat)) {
  cat_rows <- sync_details[sync_details$cat == cat_name, ]
  if (nrow(cat_rows) == 0) next
  
  video_name <- cat_rows$sync_video[1]
  new_start <- cat_rows$sync_video_start_time[1]
  
  if (!is.na(new_start)) {
    # make it posixct (even though it is, be safe)
    new_start_time <- as.POSIXct(new_start, origin = "1970-01-01", tz = "Australia/Brisbane")
  } else {
    new_start_time <- as.POSIXct(NA, tz = "Australia/Brisbane")
    message(sprintf("Missing start time for cat: %s, video: %s", cat_name, video_name))
  }
  
  # Find matching video_info row
  idx <- video_info$cat == cat_name & tools::file_path_sans_ext(video_info$filename) == video_name
  if (any(idx)) {
    video_info$adjusted_start_time[idx] <- new_start_time
  } else {
    message(sprintf("No match found in video_info for cat: %s, video: %s", cat_name, video_name))
  }
}


# Update time of every video ----------------------------------------------
# now that we have the start time of the sync video
# we can update the times of the subsequent videos
video_info <- video_info %>%
  group_by(cat) %>%
  mutate(
    # Get index of first non-NA adjusted_start_time
    first_non_na_idx = which(!is.na(adjusted_start_time))[1],
    
    # ref start time
    reference_time = adjusted_start_time[first_non_na_idx],
    reference_offset = seconds_from_first[first_non_na_idx],
    
    # Adjusted video time
    adjusted_video_start_time = reference_time + as.difftime(seconds_from_first - reference_offset, units = "secs")
  ) %>%
  mutate(adjusted_video_end_time = adjusted_video_start_time + as.difftime(duration_sec, units = "secs")
) %>%
  ungroup() %>%
  select(-first_non_na_idx, -reference_time, -reference_offset)

# save
fwrite(video_info, file.path(base_path, "RawData", "Alignment_guide.csv"))
