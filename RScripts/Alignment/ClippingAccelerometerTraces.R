# Clipping accelerometer traces to align with video -----------------------
# Manually identifying the probable start and end acceelrometer rows for each video

# Set up ------------------------------------------------------------------
options(digits = 12)
library(lubridate)
library(dplyr)
library(av)
library(data.table)
library(plotly)

# Define variables --------------------------------------------------------
base_path <- "C:/Users/oaw001/OneDrive - University of the Sunshine Coast/CatProject"
accel_Hz <- 50
buffer <- 0 * accel_Hz

# Find all the claps / sync events ----------------------------------------
# this script is interactive and graphs must be inspected manually
# source(file.path(base_path, "Scripts", "AligningVideoAndAccel", "AccelClapFinder.R"))

# save all output to csv in RawData called: Sync_details.csv
sync_details <- fread(file.path(base_path, "RawData", "Sync_details.csv"))

# Find video and accel information ----------------------------------------
source(file.path(base_path, "Scripts",  "AligningVideoAndAccel", "VideoAndAccelInfoExtraction.R"))
# saves as: all_accel_info and all_video_info

# Align the video and accel
source(file.path(base_path, "Scripts",  "AligningVideoAndAccel", "AligningSources.R"))
# returns: alignment_guide

# Identifying sections of the accelerometer that probably correspond to video
source(file.path(base_path, "Scripts",  "AligningVideoAndAccel", "FlaggingAccelSections.R"))









# Calculate relevant accelerometer rows -----------------------------------
# convert video and start time back into fractional time (to match accelerometer)
video_info$start_time_frac <- (as.numeric(as.POSIXct(video_info$start_time)) + 36000) / 86400 + 719529
video_info$end_time_frac <- (as.numeric(as.POSIXct(video_info$end_time)) + 36000) / 86400 + 719529

# read in accelerometer # will need to change if there is more than one
accel_data <- fread(accels_list[1])

video_info$accel_start_row <- sapply(video_info$start_time_frac, function(time) {
  tryCatch(
    expr = {
      which.min(abs(accel_data$V1 - time))
    },
    error = function(e) {
      print("Video begins before accelerometer, returning 0")
      return(0)
    }
  )
})

video_info$accel_end_row <- sapply(video_info$end_time_frac, function(time) {
  which.min(abs(accel_data$V1 - time))
})

# Extract relevant rows ---------------------------------------------------
video_info$clip_start_row <- video_info$accel_start_row - buffer
video_info$clip_end_row <- video_info$accel_end_row + buffer

# the buffer could cause the start to be negative and we need to account for that
for (i in 1:length(video_info$filename)) {
  if (video_info$clip_start_row[i] > 0) {
    clipped_accel <- accel_data[video_info$clip_start_row[i]:video_info$clip_end_row[i], ]
  } else {
    # Calculate the number of rows to add as 0
    num_negative_rows <- abs(video_info$clip_start_row[i]) + 1
    
    # Create a data frame of zero rows with the same column structure as accel_data to fill in the -ve rows
    zero_rows <- data.frame(matrix(0, nrow = num_negative_rows, ncol = ncol(accel_data)))
    colnames(zero_rows) <- colnames(accel_data)
    
    clipped_accel <- rbind(zero_rows, accel_data[1:video_info$clip_end_row[i], ])
  }
  
  # save it
  file_base <- sub("\\.MP4$", "", video_info$filename[i], ignore.case = TRUE)
  file_name <- file.path(base_path, "RawData", cat_name, paste0(file_base, "_clipped_accel.csv"))
  fwrite(clipped_accel, file_name, row.names = FALSE)
}







