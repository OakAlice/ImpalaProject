#| Main script for standardisation of Dead Reckoning workflow
#| Set up, load packages, and navigate through the reamainder of the workflow
#| Adapted from the work of Rich Gunner, Jojo Schultz, Luke Jessup, and Chris Clemente

#| Contains a tutorial on example data from walking around a rugby field
#| As well as a worked example of a single impala dataset
#| This has been designed for the impala data

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
source("Scripts/DeadReckoning/Gundog.Tracks.R")
source("Scripts/DeadReckoning/Gundog.Compass.R")
source("Scripts/DeadReckoning/Custom_Functions.R")

# set some variables
acc_cols <- c("RawAX", "RawAY", "RawAZ")
mag_cols <- c("RawMX", "RawMY", "RawMZ")


# Prep the calibration data -----------------------------------------------
# (this should have already been extracted)... otherwise
# source(file = file.path(base_path, "Scripts/DeadReckoning/ExtractingCalibrationEvents.R"))
cal_data <- fread(file.path(collar_dir, "calibration_data.csv"))
# smooth it
cal_data <- smooth_and_filter(data = cal_data, k = 5, fs = 50, bw_cutoff = 5, bw_order = 4)

# ggplot(cal_data[4000:5000,], aes(x = gps_time_est)) + 
#   geom_path(aes(y = RawAX.sm, colour = "AX")) + geom_path(aes(y = RawAY.sm, colour = "AY")) + geom_path(aes(y = RawAZ.sm, colour = "AZ")) 
# ggplot(cal_data[3000:5000,], aes(x = gps_time_est)) + 
#   geom_path(aes(y = RawMX.sm, colour = "MX")) + geom_path(aes(y = RawMY.sm, colour = "MY")) + geom_path(aes(y = RawMZ.sm, colour = "MZ")) 


# Select the days to dead reckon ------------------------------------------
start_time <- fread(path_to_calinfo) %>%
  dplyr::filter(Collar == basename(collar_dir)) %>%
  mutate(DeploymentStart = as.POSIXct(DeploymentStart, format = "%d.%m.%Y %H:%M:%S", tz = "Africa/Johannesburg")) %>%
  pull(DeploymentStart)
start_time <- as.POSIXct(start_time, tz = "UTC")

# Extract dates from filenames and filter to >= start_time to only select the valid deployment days
all_days <- list.files(chunked_dir_path, pattern = ".RDA", full.names = TRUE)
all_days <- all_days[as.Date(sub(".*_(\\d{4}-\\d{2}-\\d{2})\\.RDA$", "\\1", all_days)) >= as.Date(start_time)]

# Determine the collar orientation ----------------------------------------
# See attached doc for information on how we determined these orientations...
acc_orientation <- "NWU"
mag_orientation <- "NED"
gravity_direction <- "down"

# pitch was determined from extracting known walking events and then taking the mean of axes during those times
pitch <- atan2(-(-0.253599267), sqrt(0.259529436^2 + 0.891019352^2))
pitch_deg <- pitch * 180 / pi

# now execute it
# for (day in all_days){
  day <- all_days[2]
  source("Scripts/DeadReckoning/DeadReckoningPerDay.R")
# }





