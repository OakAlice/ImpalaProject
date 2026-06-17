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

for (Collar in collars){ # giant loop
  print(Collar)
  collar_dir <- file.path(base_path, "Data", "RawData", Collar)
  chunked_dir_path <- file.path(collar_dir, "Chunked")
  
  # Extract the calibration data --------------------------------------------
  cal_data <- fread(file.path(base_path, "Data", "RawData", Collar, "calibration_data.csv"))

  # Select the days to dead reckon ------------------------------------------
  start_time <- fread(path_to_calinfo) %>%
    dplyr::filter(Collar == basename(collar_dir)) %>%
    mutate(DeploymentStart = as.POSIXct(DeploymentStart, format = "%d.%m.%Y %H:%M:%S", tz = "Africa/Johannesburg")) %>%
    pull(DeploymentStart)
  start_time <- as.POSIXct(start_time, tz = "UTC")
  
  # use the unprocessed data
  # Extract dates from filenames and filter to >= start_time to only select the valid deployment days
  all_days <- list.files(chunked_dir_path, pattern = ".csv", full.names = TRUE)
  all_days <- all_days[as.Date(sub(".*_(\\d{4}-\\d{2}-\\d{2})\\.csv$", "\\1", all_days)) >= as.Date(start_time)]
  
  # now execute it
  for (day in all_days){
    day <- all_days[3]
    source("Scripts/DeadReckoning/DeadReckoningPerDay.R")
  }
  
}


