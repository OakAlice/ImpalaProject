# These are the manual steps required for making training data ------------
# Start with videos and per-day accelerometer files

if(!file.exists(file.path(base_path, "Data", "LabelledData", "CleanedlLabelledData.csv"))){
  print("do all of this")
  
  # Extracting video information --------------------------------------------
  # as every camera encodes its metadata slightly different, this is a quite manual process
  # therefore, update this to match the cameras
  # loops through all collars with associated videos
  source(file = file.path(base_path, "Scripts", "RoughAlignment", "VideoInfoExtraction.R"))
  
  # Rough alignment of the accelerometer and videos -------------------------
  # use the manual slide bar to roughly align the videos with the accelerometer...# Instructions in the Notes/VideoAlignment_Instructions.docx file
  rstudioapi::navigateToFile(file = file.path(base_path, "Scripts", "RoughAlignment", "AccelDelayFinder.R"))
  
  # Annotate the clipped segments of video ----------------------------------
  # Use the matlab SyncStation to apply detailed labels
  # these can be found in the Scripts/SyncStation folder
  # Instructions continued in the Notes/VideoAlignment_Instructions.docx file
  
  # combine the matlab annotations and split out into the individual behaviours --------
  # clean the data, recombine, etc.
  source(file = file.path(base_path, "Scripts",  "BehaviouralDetection","GenerateTrainingData", "Create_TrainingData"))
  
  # Explore the data --------------------------------------------------------
  unique(raw_data$Activity)
  target_behaviours <- unique(raw_data$Activity)
  # c("Foraging_Headup", 
  #                      "Foraging_Headdown", 
  #                      "Locomotion_Walk", 
  #                      "Locomotion_Fast",
  #                      "Stationary_Sleep", 
  #                      "Stationary_Standing", 
  #                      "Stationary_Vigilance", 
  #                      "Grooming",
  #                      "Other")
  
  # use this script to play around with the data labels, shapes and volume
  # if there are errors or misreads or you want to change the groupings
  # go back to the start of the script and work through the matlab again
  rstudioapi::navigateToFile(file = file.path(base_path, "Scripts", "BehaviouralDetection", "GenerateTrainingData", "Explore_TrainingData.R"))
  
  # when you are finally happy with it, save here :)
  fwrite(raw_data, file.path(base_path, "Data", "LabelledData", "CleanedlLabelledData.csv"))
  
} else {
  print("cleaned data already generated")
}
