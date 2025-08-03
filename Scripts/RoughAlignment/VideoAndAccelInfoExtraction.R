# Video and accel information extraction ----------------------------------
library(av)
library(data.table)

# base_pav# base_path <- "C:/Users/oaw001/OneDrive - University of the Sunshine Coast/BAU/Senna/Impala Data"
base_path <- "D:/ImpalaProject/RawData"

video_files <- list.files(base_path, pattern = "\\.(MTS|DJI|MOV|MP4)$", ignore.case = TRUE, full.names = TRUE, recursive = TRUE)

impalas <- list.dirs(path = file.path(base_path), full.names = TRUE, recursive = FALSE)

# initialise dataframes
all_video_info <- data.frame()
all_accel_info <- data.frame()

for (collar in unique(impalas)){
  print(collar)
  videos_list <- list.files(file.path(collar, "Videos"), pattern = "\\.(MTS|DJI|MOV|MP4)$", ignore.case = TRUE, full.names = TRUE, recursive = TRUE)
  accels_list <- list.files(file.path(collar, "Axivity"), pattern = "*?.csv", full.names = TRUE)
  
  # Process videos
  video_info <- data.frame()  # Reset for each cat
  for (video in videos_list) {
    filename <- basename(video)
    
    # extract available information
    Time_video_end <- file.info(video)$mtime
    Dur_video_sec <- av_media_info(video)$duration
    
    # Calculate start time
    Time_video_start <- Time_video_end - as.difftime(Dur_video_sec, units = "secs")
    
    # Create temporary dataframe for this video
    temp_video_info <- data.frame(
      individual = basename(collar),
      filename = filename,
      start_time = Time_video_start,
      duration_sec = Dur_video_sec,
      end_time = Time_video_end,
      stringsAsFactors = FALSE
    )
    
    # Append to this cat's video info
    video_info <- rbind(video_info, temp_video_info)
  }
  
  # Calculate seconds from first video for this cat
  first_video_start <- min(video_info$start_time)
  video_info$seconds_from_first <- as.numeric(difftime(video_info$start_time, first_video_start, units = "secs"))
  
  # Append this cat's video info to the main dataframe
  all_video_info <- rbind(all_video_info, video_info)
  
  # Process accelerometer files
  accel_info <- data.frame()  # Reset for each cat
  for (accel in accels_list) {
    filename <- basename(accel)
    
    # Read first line of CSV to get start time
    accel_data <- read.csv(accel, nrows = 1)
    Time_accel_start <- as.POSIXct((accel_data[1,1] - 719529)*86400, origin = "1970-01-01", tz = "Africa/Johannesburg")
  
    Time_accel_start_char <- as.character(format(Time_accel_start, "%Y-%m-%d %H:%M:%S", tz = "Africa/Johannesburg"))
    
    # Create temporary dataframe for this accel file
    temp_accel_info <- data.frame(
      cat = collar,
      filename = filename,
      start_time = Time_accel_start,
      start_time_char =Time_accel_start_char,
      start_time_frac = accel_data[1,1],
      stringsAsFactors = FALSE
    )
    
    # Append to this cat's accel info
    accel_info <- rbind(accel_info, temp_accel_info)
  }
  
  # Append this cat's accel info to the main dataframe
  all_accel_info <- rbind(all_accel_info, accel_info)
}

# writing these to file
fwrite(all_accel_info, file.path(base_path, "Accel_info.csv"))
fwrite(all_video_info, file.path(base_path, "Video_info.csv"))

