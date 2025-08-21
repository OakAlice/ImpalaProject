# Main Script for executing Rough Alignment -------------------------------
# Very much a collaborative effort between Chris and I
# I worked on it for ages but was unable to get an alignment 
# Chris fixed all the bugs and wrote much better, much cleaner code
# and then I put that much better code back into the reproducible workflow
# and that's what's here now - yay

# Environment Set Up ------------------------------------------------------
base_path <- "C:/Users/PC/Documents/ImpalaProject"

rm(list = ls())
gc()

library(pacman)
p_load(av,
       data.table, 
       stringr,
       tidyverse,
       zoo)

setDTthreads(0L) # make the fread function faster

# define the collar you want to execute the workflow for
Collar <- "Collar_2"

# define the path to the files
accel_dir <- file.path(base_path, "RawData", Collar, "Board")
video_dir <- file.path(base_path, "rawData", Collar, "Videos")

# Read artemis accel files together ---------------------------------------
# previously I was doing this step using cmdline and it was really fast but I was getting some NAs and issues
# so Chris figured better safe than sorry
# so can either use my way (the instructions are in scripts file) or this way
source(file = file.path(base_path, "Scripts", "RoughAlignment", "CombiningArtemisAccelFiles.R"))

# Read the GPS files together ---------------------------------------------
# I wrote some nice code that only works when the data is in the right order and consistently formatted
# Chris discovered that this is not always the case and there is a lot of issues
# therefore, the following is a combination of our codes... thank you to Chris for figuring out the issue
source(file = file.path(base_path, "Scripts", "RoughAlignment", "CombiningArtemisGPSFiles.R"))

# Combing the GPS and Accel files based on timestamp ----------------------
# saves all aligned data as a single RDA, as well each 24-hr period as its own file
source(file = file.path(base_path, "Scripts", "RoughAlignment", "CombiningArtemisAccel&GPS.R"))

# Extracting video information --------------------------------------------
# this is a absolute pain in the behind
# as every camera encodes its metadata slightly different, it's a highly manual process
# I have automated it for the drone footage collected by Chris
# but I'm still working on the other cameras
source(file = file.path(base_path, "Scripts", "RoughAlignment", "VideoInfoExtraction.R"))


# Extracting the relevant section of accel for each video -----------------
# in progress




