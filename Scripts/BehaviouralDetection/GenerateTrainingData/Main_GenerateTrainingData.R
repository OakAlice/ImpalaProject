#################
# Main_GenerateTrainingData

# Overview:
# This is the master script for generating the training data used for the creation
# of the machine learning behavioural classification model
# It involves synchronising the IMU with the video and then annotating the behaviours
# reading them together, relabelling/cleaning, and then generating features
# Note that this is very manual and requires step-by-step management 

# Requires:
# Per day time-corrected IMU traces
# Time-corrected videos

#################

if(!file.exists(file.path(base_path, "Data", "LabelledData", "CleanedlLabelledData.csv"))){
  print("do all of this")
  
  # Extracting video information --------------------------------------------
  # as every camera encodes its metadata slightly different, this is a quite manual process
  # therefore, update this to match the cameras
  # loops through all collars with associated videos
  source(file = file.path(base_path, "Scripts", "GeneratingTrainingData", "VideoInfoExtraction.R"))
  
  # Rough alignment of the accelerometer and videos -------------------------
  # use the manual slide bar to roughly align the videos with the accelerometer...# Instructions in the Notes/VideoAlignment_Instructions.docx file
  rstudioapi::navigateToFile(file = file.path(base_path, "Scripts", "GeneratingTrainingData", "ExtractingAccelSegments.R"))
  
  # Annotate the clipped segments of video ----------------------------------
  # Use the matlab SyncStation to apply detailed labels
  # these can be found in the Scripts/SyncStation folder
  # Instructions continued in the Notes/VideoAlignment_Instructions.docx file
  
  # combine the matlab annotations and split out into the individual behaviours --------
  # clean the data, recombine, etc.
  source(file = file.path(base_path, "Scripts",  "BehaviouralDetection", "GenerateTrainingData", "CleanLabelledData.R"))
  
  # Explore the data --------------------------------------------------------
  unique(raw_data$Activity)
  target_behaviours <- unique(raw_data$Activity)
  
  # use this script to play around with the data labels, shapes and volume
  # if there are errors or misreads go back to the start of the script and work through the matlab again
  rstudioapi::navigateToFile(file = file.path(base_path, "Scripts", "BehaviouralDetection", "GenerateTrainingData", "ExploreLabelledData.R"))
  
  # when you are finally happy with it, save here :)
  fwrite(raw_data, file.path(base_path, "Data", "LabelledData", "CleanedlLabelledData.csv"))
  
} else {
  print("annotated data already generated")
}

# Generate features from the raw data -------------------------------------
source(file = file.path(base_path, "Scripts",  "BehaviouralDetection", "GenerateTrainingData", "Functions_GenerateFeatures.R"))
if(!file.exists(file.path(base_path, "Data", "LabelledData", paste0("FeatureLabelledData.csv")))){
    
  raw_data <- fread(file.path(base_path, "Data", "LabelledData", "CleanedlLabelledData.csv")) %>%
    rename(Time = utc_datetime)
  
  # select only the raw data I want to keep
  ## NOTE: Adjust this # TODO: Fix this
  selected_data <- raw_data %>% group_by(ID, Activity) %>% arrange(Time, by.group = TRUE) %>% slice(1:15000) # only get the first 5 minutes

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
  
  features_to_normalise <- colnames(features)[!colnames(features) %in% c("Activity", "ID", "Time")]
  features[, (features_to_normalise) := lapply(.SD, function(x) {
    s <- sd(x, na.rm = TRUE)
    if (s == 0 || is.na(s)) return(rep(0, .N))
    (x - mean(x, na.rm = TRUE)) / s
  }), .SDcols = features_to_normalise]
  
  # save this
  fwrite(features, file.path(base_path, "Data", "LabelledData", paste0("FeatureLabelledData.csv")))
} else {
  print("features already generated")
}
