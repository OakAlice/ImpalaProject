# Set up ------------------------------------------------------------------

source(file = file.path(base_path, "Scripts", "DeadReckoning", "Custom_Functions.R"))
source(file = file.path(base_path, "Scripts", "DeadReckoning", "Gundog.Compass.R"))
source(file = file.path(base_path, "Scripts", "DeadReckoning", "Gundog.Tracks.R"))

# set some variables
acc_cols <- c("RawAX", "RawAY", "RawAZ")
mag_cols <- c("RawMX", "RawMY", "RawMZ")


chunked_dir_path <- file.path(collar_dir, "Chunked")

## Extracting the calibration periods -------------------------------------
# this will save the extracted and formatted calibration data into its own file
path_to_calinfo <- file.path(base_path, "Notes/ImpalaCollaringTimes.csv")
source("Scripts/DeadReckoning/ExtractingCalibrationEvents.R")
  
# Smooth and prepare all data ---------------------------------------------
all_days <- list.files(file.path(collar_dir, "Chunked"), pattern = ".RDA", full.names = TRUE)
# load in the cal data
cal_data <- fread(file.path(collar_dir, "calibration_data.csv"))
start_time <- fread(path_to_calinfo) %>%
  filter(Collar == basename(collar_dir)) %>%
  mutate(DeploymentStart = as.POSIXct(DeploymentStart, format = "%d.%m.%Y %H:%M:%S", tz = "Africa/Johannesburg")) %>%
  pull(DeploymentStart)
start_time <- as.POSIXct(start_time, tz = "UTC")

# Determine the collar orientation ----------------------------------------
# Run this and then inspect the output to decide the orientation of the collar
source("Scripts/DeadReckoning/FindingOrientationVariables.R")

# from this, enter what you feel is the orientation (refer to tutorial sheet for more info)
orientation_info <- fread(file.path(collar_dir, "orientation_variables.csv"))
orientation_frame <- "NED" ##NOTE: This is currently custom 

# Run the dead reckoning correction --------------------------------------
source("Scripts/DeadReckoning/DeadReckoningPerDay.R")






