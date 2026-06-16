


# Generate features from the raw data -------------------------------------
source(file = file.path(base_path, "Scripts",  "BehaviouralDetection", "GenerateTrainingData", "Functions_GenerateFeatures.R"))
if(!file.exists(file.path(base_path, "Data", "LabelledData", paste0("FeatureLabelledData.csv")))){
  
  raw_data <- fread(file.path(base_path, "Data", "LabelledData", "CleanedlLabelledData.csv")) %>%
    rename(Time = utc_datetime)
  
  # select only the raw data I want to keep
  ## NOTE: Adjust this # TODO: Fix this
  selected_data <- raw_data %>% group_by(ID, Activity) %>% arrange(Time, by.group = TRUE) %>% slice(1:30000) # only get the first 10 minutes
  
  desired_window <- 1 # in seconds
  sample_rate <- 50
  desired_overlap <- 0
  available_axes <- c("RawAX.butt", "RawAY.butt", 'RawAZ.butt') # the name of the axes
  
  generated_features <- list()
  for (id in unique(selected_data$ID)){
    data <- selected_data %>% 
      dplyr::filter(ID == id) %>% 
      as.data.table()
    
    feature_data <- processDataPerID(id_raw_data = data, 
                                     features_type = c("timeseries", "statistical"), 
                                     window_length = desired_window, # this is in seconds, 
                                     sample_rate = sample_rate, 
                                     overlap_percent = desired_overlap)
    
    generated_features[[id]] <- feature_data
  }
  features <- bind_rows(generated_features)
  as.data.table(features)
  
  # features_to_normalise <- colnames(features)[!colnames(features) %in% c("Activity", "ID", "Time")]
  # features[, (features_to_normalise) := lapply(.SD, function(x) {
  #   s <- sd(x, na.rm = TRUE)
  #   if (s == 0 || is.na(s)) return(rep(0, .N))
  #   (x - mean(x, na.rm = TRUE)) / s
  # }), .SDcols = features_to_normalise]
  
  # save this
  fwrite(features, file.path(base_path, "Data", "LabelledData", "FeatureLabelledData.csv"))
} else {
  print("features already generated")
}
