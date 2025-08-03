# Flagging the accelerometer sections -------------------------------------
library(data.table)
library(tidyverse)

base_path <- "D:/ImpalaProject/RawData"
videos <- fread(file.path(base_path, "Video_info.csv"))

for (ID in unique(videos$individual)){
  
  relevant_videos <- videos %>% filter(individual == ID)
  
  # just definitely make sure it is formatted properly
  relevant_videos$start_times <- as.POSIXct(relevant_videos$start_time, tz = "Africa/Johannesburg")
  relevant_videos$end_times <- as.POSIXct(relevant_videos$end_time, tz = "Africa/Johannesburg")
  
  # load in the accelerometer
  accel_name <- list.files(file.path(base_path, ID, "Axivity"), pattern = "*?.csv", full.names = TRUE)
  accel_data <- fread(accel_name)
  
  # convert the accel time according to whatever rule you figured out
  # in this case, I needed to do the standard conversion - 2 hrs
  # accel_data$Time <- as.POSIXct((accel_data$V1 - 719529)*86400 + 8*3600, origin = "1970-01-01", tz = "Africa/Johannesburg")
  accel_data$Time <- as.POSIXct((accel_data$V1 - 719529)*86400 - 2*3600, origin = "1970-01-01", tz = "Africa/Johannesburg")
  
  # Do all flagging
  setDT(accel_data)
  setDT(relevant_videos)
  
  # Perform non-equi join to match V1 between start and end
  # this is the fastest way to do it I think?
  # hard because files are so massive
  setnames(relevant_videos, c("start_times", "end_times", "filename"),
           c("start", "end", "filename"))
  accel_data[, Flags := NA_character_]
  
  # Perform reverse non-equi join to stitch them together
  for (i in seq_len(nrow(relevant_videos))) {
    s <- relevant_videos$start[i]
    e <- relevant_videos$end[i]
    f <- relevant_videos$filename[i]
    
    accel_data[Time >= s & Time <= e, Flags := f]
  }
  
  # new_accel_name <- file.path(base_path, ID, "Axivity", paste0(ID, "_flagged.csv")) 
  # fwrite(accel_data, new_accel_name, row.names = FALSE)

  # clip each of these flagged sections and save as their own thing

  # Get unique flags (excluding NA and empty strings)
    unique_flags <- unique(accel_data$Flags)
    unique_flags <- unique_flags[!is.na(unique_flags) & unique_flags != ""]
    
    print(paste("ID:", ID, "- Number of flagged rows:", sum(!is.na(accel_data$Flags))))
    print(unique_flags)
    
    # For each flag, extract and save corresponding data
    for (flag in unique_flags) {
      # Extract rows for this flag with buffer before and after
      flag_rows <- which(accel_data$Flags == flag)
      start_row <- max(1, min(flag_rows) - 100)  # dont go below 1 for the early videos though
      end_row <- min(nrow(accel_data), max(flag_rows) + 500)  # logic to prevent sampling too much
      flag_data <- accel_data[start_row:end_row,]
      
      # Create output filename
      output_name <- gsub("\\.(MOV|DJI|MTS|MP4)$", "", flag, ignore.case = TRUE)
      output_name <- paste0(output_name, "_clip.csv")
      output_path <- file.path(base_path, ID, "Axivity", "Clipped")
      
      if (!dir.exists(output_path)) {
        dir.create(output_path, recursive = TRUE)
      }
      
      # Save to CSV
      fwrite(flag_data, file.path(output_path, output_name))
    }
}
