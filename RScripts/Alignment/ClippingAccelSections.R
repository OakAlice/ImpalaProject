# Clipping out the probable sections --------------------------------------

# Get list of all cat folders
cat_folders <- list.files(file.path(base_path, "RawData"), full.names = TRUE)
cat_folders <- cat_folders[file.info(cat_folders)$isdir]  # Keep only directories

# Process each cat's data
for (cat_folder in cat_folders) {
  print(basename(cat_folder))
  
  # Find the flagged CSV file
  flagged_file <- list.files(cat_folder, pattern = "_flagged.csv$", full.names = TRUE)
  
  if (length(flagged_file) > 0) {
    # Read the flagged data
    cat_data <- fread(flagged_file)
    
    # Get unique flags (excluding NA and empty strings)
    unique_flags <- unique(cat_data$Flags)
    unique_flags <- unique_flags[!is.na(unique_flags) & unique_flags != ""]
    
    # For each flag, extract and save corresponding data
    for (flag in unique_flags) {
      # Extract rows for this flag with buffer before and after
      flag_rows <- which(cat_data$Flags == flag)
      start_row <- max(1, min(flag_rows) - 100)  # dont go below 1 for the early videos though
      end_row <- min(nrow(cat_data), max(flag_rows) + 500)  # logic to prevent sampling too much
      flag_data <- cat_data[start_row:end_row,]
      
      # Create output filename
      output_name <- gsub(".MP4", "_clipped_accel.csv", flag)
      output_path <- file.path(cat_folder, output_name)
      
      # Save to CSV
      fwrite(flag_data, output_path)
    }
  } else {
    warning(sprintf("No flagged CSV file found in %s", basename(cat_folder)))
  }
}

