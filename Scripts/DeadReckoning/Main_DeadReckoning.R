#################
# Main_DeadReckoning

# Overview:
# Main script for standardisation of Dead Reckoning workflow
# Set up, load packages, and navigate through the reamainder of the workflow
# Adapted from the work of Rich Gunner, Jojo Schultz, Luke Jessup, and Chris Clemente
# Contains a tutorial on example data from walking around a rugby field
# As well as a worked example of a single impala dataset
# This has been designed for the impala data

# Requires:
# time-corrected, scaled IMU data and gps
# predicted behaviours for each day of data

# Options:
# Before running the Gundog.Tracks dead reckoning path reconstruction method
# we have to orient the IMU in space
# due to the way the impala move, the Gundog.Compass method doesn't work
# instead, we used the onboard calculated quaternions

# Note:
# This script is in active development

#################

# Set up ------------------------------------------------------------------
# load in the package functions (from the Gundog package, developed by Rich Gunner)
source(file.path(base_path, "Scripts/DeadReckoning/Gundog.Tracks.R"))
source(file.path(base_path,"Scripts/DeadReckoning/Gundog.Compass.R"))
# and then the custom functions that I wrote
source(file.path(base_path,"Scripts/DeadReckoning/Functions_DR.R"))

for (Collar in collars){ # giant loop
  print(Collar)
  
  # define where the data is coming from and going to
  chunked_dir_path <- file.path(base_path, "Data", "RawData", Collar, "Chunked")
  predictions_dir <- file.path(base_path, "Output", "BehaviouralPredictions", Collar)
  dr_output_dir <- file.path(base_path, "Output", "DeadReckoning", Collar)
  if(!dir.exists(dr_output_dir)){dir.create(dr_output_dir)}
  
  # Select the days to dead reckon
  start_time <- fread(path_to_calinfo) %>%
    dplyr::filter(CollarNumber == Collar) %>%
    mutate(DeploymentStart = as.POSIXct(DeploymentStart, format = "%d.%m.%Y %H:%M:%S", tz = "Africa/Johannesburg")) %>%
    pull(DeploymentStart)
  start_time <- as.POSIXct(start_time, tz = "UTC")
  
  # use the unprocessed data
  # Extract dates from filenames and filter to >= start_time to only select the valid deployment days
  accel_files <- list.files(chunked_dir_path, pattern = ".csv", full.names = TRUE)
  accel_files <- accel_files[as.Date(sub(".*_(\\d{4}-\\d{2}-\\d{2})\\.csv$", "\\1", accel_files)) >= as.Date(start_time)]
  # and do the same for the predictions
  prediction_files <- list.files(predictions_dir, full.names = TRUE)
  
  # these are the dates we are working with for the dr
  dates <- as.Date(sub(".*_(\\d{4}-\\d{2}-\\d{2})\\.csv$", "\\1", accel_files))
  for (date in dates){
    
    # date <- dates[3]
    accel_day <- grep(date, accel_files, value = TRUE)
    predictions_day <- grep(date, prediction_files, value = TRUE)
    
    source("Scripts/DeadReckoning/DeadReckoningPerDay.R")
  }
  
}


