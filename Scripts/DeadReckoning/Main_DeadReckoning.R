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

# Options:
# Before running the Gundog.Tracks dead reckoning path reconstruction method
# we have to orient the IMU in space
# In the case of the impala, we need to account for locomotion occuring in 2 orientations (head up and head down)
# I have tried to do this with both Gundog compass and Magdwick quaternions
# have left everything in for WIP purposes

# Note:
# This script is in active development of new methods and is a mess at the moment

#################

# Set up ------------------------------------------------------------------
pacman::p_load(
  tidyverse,
  data.table,
  plotly,
  rgl,
  zoo,
  processx,
  patchwork,
  splines,
  signal,
  roll
)

# load in the package functions (from the Gundog package, developed by Rich Gunner)
source(file.path(base_path, "Scripts/DeadReckoning/Gundog.Tracks.R"))
source(file.path(base_path,"Scripts/DeadReckoning/Gundog.Compass.R"))
source(file.path(base_path,"Scripts/DeadReckoning/Functions_DR.R"))

# set some variables
acc_cols <- c("RawAX", "RawAY", "RawAZ")
mag_cols <- c("RawMX", "RawMY", "RawMZ")

for (Collar in collars){ # giant loop
  print(Collar)
  collar_dir <- file.path(base_path, "Data", "RawData", Collar)
  chunked_dir_path <- file.path(collar_dir, "Chunked")
  
  # Select the days to dead reckon ------------------------------------------
  start_time <- fread(path_to_calinfo) %>%
    dplyr::filter(Collar == basename(collar_dir)) %>%
    mutate(DeploymentStart = as.POSIXct(DeploymentStart, format = "%d.%m.%Y %H:%M:%S", tz = "Africa/Johannesburg")) %>%
    pull(DeploymentStart)
  start_time <- as.POSIXct(start_time, tz = "UTC")
  
  # choose the compass method
  compass_method <- "Gundog" # options: Magdwick or Gundog
  
  if(compass_method == "Gundog"){
    # Prep the calibration data -----------------------------------------------
    # (this should have already been extracted)... otherwise
    # source(file = file.path(base_path, "Scripts/DeadReckoning/ExtractingCalibrationEvents.R"))
    cal_data <- fread(file.path(collar_dir, "calibration_data.csv"))
    
    # ggplot(cal_data[4000:5000,], aes(x = gps_time_est)) + 
    #   geom_path(aes(y = RawAX.sm, colour = "AX")) + geom_path(aes(y = RawAY.sm, colour = "AY")) + geom_path(aes(y = RawAZ.sm, colour = "AZ")) 
    # ggplot(cal_data[3000:5000,], aes(x = gps_time_est)) + 
    #   geom_path(aes(y = RawMX.sm, colour = "MX")) + geom_path(aes(y = RawMY.sm, colour = "MY")) + geom_path(aes(y = RawMZ.sm, colour = "MZ")) 
    
  } else if (compass_method == "Madgwick"){
    
    print("need to go process the data in python")

    # need the following variables:
    # gyr measurements of angular velocity in rad/s (have)
    # acc measurements of acceleration in in m/s^2 (need to convert)
    # mag measurements of magnetic field in mT (have... maybe... maybe need to divide differently)
    # frequency – Sampling frequency in Herz. (have)
      
    # use the python script: "Scripts/DeadReckoning/MadgwickCompass.py"
    # to generate the Roll Pitch and Yaw... and then stitch them back in later
    # in the next script
  }
  
  # use the unprocessed data
  # Extract dates from filenames and filter to >= start_time to only select the valid deployment days
  all_days <- list.files(chunked_dir_path, pattern = ".csv", full.names = TRUE)
  all_days <- all_days[as.Date(sub(".*_(\\d{4}-\\d{2}-\\d{2})\\.csv$", "\\1", all_days)) >= as.Date(start_time)]
  
  # now execute it
  for (day in all_days){
    day <- all_days[2]
    source("Scripts/DeadReckoning/DeadReckoningPerDay.R")
  }
  
}


