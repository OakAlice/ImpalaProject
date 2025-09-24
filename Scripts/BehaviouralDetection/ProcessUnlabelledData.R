# Process the unlabelled data ---------------------------------------------
# load in the unlabelled data and generate the features

collar <- "Collar_2"

unlabelled_files <- list.files(file.path(base_path, "RawData", collar, "Board", "Chunked"), full.names = TRUE, pattern = ".RDA")

processed_unlabelled <- lapply(unlabelled_files, function(x){
  load(x) # reads in as accel_data
  filname <- tools::file_path_sans_ext(basename(x))  
  
  accel_data <- accel_data[, c("RawAX", "RawAY", "RawAZ", "gps_time_est")]
  colnames(accel_data) <- c("X", "Y", "Z", "Time")
  accel_data$ID <- collar
  
  feature_data <- processDataPerID(id_raw_data = accel_data, 
                                     features_type = c("timeseries", "statistical"), 
                                     window_length = desired_window, # this is in seconds, 
                                     sample_rate = sample_rate, 
                                     overlap_percent = desired_overlap)
  
  fwrite(feature_data, file.path(base_path, "RawData", collar, "Board", "Chunked", paste0(filname, "_features.csv")))
  
  feature_data
})

processed_unlabelled <- rbindlist(processed_unlabelled)

fwrite(processed_unlabelled, file.path(base_path, "Output", collar, "Feature_data.csv"))

