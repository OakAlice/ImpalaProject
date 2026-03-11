# Main Script for executing Rough Alignment -------------------------------
# Very much a collaborative effort between Chris and I
# I worked on it for ages but was unable to get an alignment 
# Chris fixed all the bugs and wrote much better, much cleaner code
# and then I put that much better code back into the reproducible workflow
# and that's what's here now - yay

# Environment Set Up ------------------------------------------------------
rm(list = ls())
gc()

base_path <- "C:/Users/PC/Documents/ImpalaProject"

library(pacman)
p_load(av,
       data.table, 
       lubridate,
       plotly,
       stringr,
       shiny,
       tidyverse,
       zoo)

setDTthreads(0L) # make the fread function faster

# define the collar you want to execute the workflow for
Collar <- "Collar_2"

# define the path to the files
accel_dir <- file.path(base_path, "RawData", Collar, "Board")
video_dir <- file.path(base_path, "RawData", Collar, "Videos")

# Read artemis accel files together ---------------------------------------
# previously I was doing this step using cmdline and it was really fast but I was getting some NAs and issues
# so Chris figured better safe than sorry
# so can either use my way (the instructions are in scripts file) or this way
if (!file.exists(file.path(accel_dir, "Board_Accel.RDA"))){
  source(file = file.path(base_path, "Scripts", "RoughAlignment", "CombiningArtemisAccelFiles.R"))
}

# Read the GPS files together ---------------------------------------------
# I wrote some nice code that only works when the data is in the right order and consistently formatted
# Chris discovered that this is not always the case and there is a lot of issues
# therefore, the following is a combination of our codes... thank you to Chris for figuring out the issue
if (!file.exists(file.path(accel_dir, "Board_GPS.csv"))){
  source(file = file.path(base_path, "Scripts", "RoughAlignment", "CombiningArtemisGPSFiles.R"))
}

# Combing the GPS and Accel files based on timestamp ----------------------
# saves all aligned data as a single RDA, as well each 24-hr period as its own file
if (!file.exists(file.path(accel_dir, "Board_Aligned.RDA"))){
  source(file = file.path(base_path, "Scripts", "RoughAlignment", "CombiningArtemisAccel_GPS.R"))
}

# clean the workspace
if (exists("accel_data")) rm(accel_data)
if (exists("accel_list")) rm(accel_list)

# Extracting video information --------------------------------------------
# this is a absolute pain in the behind
# as every camera encodes its metadata slightly different, it's a highly manual process
# I have automated it for the drone footage collected by Chris
# but we are still working on the other cameras
if (!file.exists(file.path(video_dir, "Video_metadata.csv"))){
  source(file = file.path(base_path, "Scripts", "RoughAlignment", "VideoInfoExtraction.R"))
}

# Manually Assessing Alignment / Determining Delay ------------------------
# the reality of working with technology is that some of the clocks drifted
# we need to determine the amount by which it drifted and it is easiest to do this manually
# use the following script to explore and play around with the different files
file <- file.path(base_path, "Scripts", "RoughAlignment", "AccelDelayFinder.R")




