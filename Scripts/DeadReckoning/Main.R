#| Main script for standardisation of Dead Reckoning workflow
#| Set up, load packages, and navigate through the reamainder of the workflow
#| Adapted from the work of Rich Gunner, Jojo Schultz, Luke Jessup, and Chris Clemente

#| Contains a tutorial on example data from walking around a rugby field
#| As well as a worked example of a single impala dataset
#| This has been designed for the impala data

# Set up ------------------------------------------------------------------
setwd("C:/Users/PC/Documents/DeadReckoning")

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
source("Scripts/DeadReckoning/Functions/Gundog.Tracks.R")
source("Scripts/DeadReckoning/Functions/Gundog.Compass.R")
# load in custom functions (helpers)
source("Scripts/ReadingData/Custom_Functions.R")
source("Scripts/DeadReckoning/Functions/Custom_Functions.R")

# set some variables
acc_cols <- c("RawAX", "RawAY", "RawAZ")
mag_cols <- c("RawMX", "RawMY", "RawMZ")


# Part 1: Test data --------------------------------------------------------
# Tutorials for learning how to work with this data
# Also just me learning about the system too
source("Scripts/DeadReckoning/TestTrials/Rugby_TestTrial_WorkedExamples.R")
source("Scripts/DeadReckoning/TestTrials/Mblock_TestTrial_WorkedExamples.R")
# source("Scripts/DeadReckoning/TestTrials/Campus_TestTrial_WorkedExamples.R") # didnt work

# Part 2: Deployment Data -------------------------------------------------
# Apply method to the deployment data from wild animals
# More complicated because the deployment data has to be extracted from the devices
CollarNum <- "Collar8"
path_to_data <- paste0("Data/Impala/", CollarNum) # set the collar we're working on
chunked_dir_path <- file.path(path_to_data, "ArtemisChunked")

## Extracting the calibration periods -------------------------------------
# this will save the extracted and formatted calibration data into its own file
path_to_calinfo <- "Data/Impala/ImpalaCollaringInformation.csv"
source("Scripts/DeadReckoning/ExtractingCalibrationEvents.R")
  
# Smooth and prepare all data ---------------------------------------------
all_days <- list.files(chunked_dir_path, pattern = ".RDA", full.names = TRUE)
# load in the cal data
cal_data <- fread(file.path(path_to_data, "calibration_data.csv"))
start_time <- fread(path_to_calinfo) %>%
  filter(Collar == basename(path_to_data)) %>%
  mutate(DeploymentStart = as.POSIXct(DeploymentStart, format = "%d.%m.%Y %H:%M:%S", tz = "Africa/Johannesburg")) %>%
  pull(DeploymentStart)
start_time <- as.POSIXct(start_time, tz = "UTC")

# Determine the collar orientation ----------------------------------------
# Run this and then inspect the output t decide the orientation of the collar
# source("Scripts/DeadReckoning/FindingOrientationVariables.R")

# from this, enter what you feel is the orientation (refer to tutorial sheet for more info)
# orientation_info <- fread(file.path(path_to_data, "orientation_variables.csv"))
acc_orientation <- "EUS"
mag_orientation <- "EDN"
gravity_direction <- "down"
pitch <- -48

# Run the dead reckoning correction --------------------------------------
source("Scripts/DeadReckoning/DeadReckoningPerDay.R")






