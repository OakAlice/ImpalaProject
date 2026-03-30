# Main Script for executing Rough Alignment -------------------------------
# Environment Set Up ------------------------------------------------------
base_path <- "C:/Users/PC/Documents/ImpalaProject"

pacman:: p_load(av,
       data.table, 
       lubridate,
       plotly,
       stringr,
       shiny,
       tidyverse,
       zoo)

setDTthreads(0L) # make the fread function faster

source(file = file.path(base_path, "Scripts", "RoughAlignment", "DataReadFunctions.R"))

sampling_start <- fread(file.path(base_path, "Notes/Metadata.csv")) %>%
  mutate(StartDate = as.Date(as.character(ReleaseDate), format = "%d-%b-%y")) %>%
  select(CollarNumber, StartDate)

# Read in and Align the Boards --------------------------------------------
# define the collar you want to execute the workflow for
collars <- list.dirs(file.path(base_path, "Data", "RawData"), recursive = FALSE, full.names = FALSE)

# Collar <- collars[12]

for (Collar in collars){
  # define the path to the files
  collar_dir <- file.path(base_path, "Data", "RawData", Collar)
  
  # Read artemis accel files together ---------------------------------------
  accel_files <- list.files(path = file.path(collar_dir, "Board"), pattern = "^dataLog\\d+\\.TXT$",  # matches dataLog00000.TXT etc.
    full.names = TRUE)
  accel_data <- stitch_artemis_accel(accel_files)
  save(accel_data, file = file.path(collar_dir, "Board_Accel.RDA"), compress = FALSE)
  
  # Read the GPS files together ---------------------------------------------
  gps_files <- list.files(file.path(collar_dir, "Board"), pattern = "^serialLog.*", full.names = TRUE)
  gps_data <- stitch_artemis_gps(gps_files)
  fwrite(gps_data, file.path(collar_dir, "Board_GPS.csv"))
  
  # Combing the GPS and Accel files based on timestamp ----------------------
  # saves all aligned data as a single RDA, as well each 24-hr period as its own file
  source(file = file.path(base_path, "Scripts", "RoughAlignment", "CombiningArtemisAccelGPS.R"))
  
  # clean the workspace
  if (exists("accel_data")) rm(accel_data)
  if (exists("accel_list")) rm(accel_list)
  
  # Extracting video information --------------------------------------------
  # this is a absolute pain in the behind
  # as every camera encodes its metadata slightly different, it's a highly manual process
  # I have automated it for the drone footage collected by Chris
  # but we are still working on the other cameras
  source(file = file.path(base_path, "Scripts", "RoughAlignment", "VideoInfoExtraction.R"))
}

# Manually Assessing Alignment / Determining Delay ------------------------
# the reality of working with technology is that some of the clocks drifted
# we need to determine the amount by which it drifted and it is easiest to do this manually
# use the following script to explore and play around with the different files
file <- file.path(base_path, "Scripts", "RoughAlignment", "AccelDelayFinder.R")


