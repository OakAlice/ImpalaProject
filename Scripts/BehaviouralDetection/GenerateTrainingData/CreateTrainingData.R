#################
# CreateTrainingData

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
  print("do all of this manually")
  
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
  # uses logic to discriminate classes
  # if there are errors or misreads go back to the start of the script and work through the matlab again
  rstudioapi::navigateToFile(file = file.path(base_path, "Scripts", "BehaviouralDetection", "GenerateTrainingData", "ExploreLabelledData.R"))
  
  # when you are finally happy with it, save here :)
  fwrite(raw_data, file.path(base_path, "Data", "LabelledData", "CleanedlLabelledData.csv"))
  
} else {
  print("annotated data already generated")
}
