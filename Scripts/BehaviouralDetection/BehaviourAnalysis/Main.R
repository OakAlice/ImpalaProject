# Main script for processing the other scripts ----------------------------

base_path <- "~/ImpalaProject"

# Packages and settings ---------------------------------------------------
# if (!requireNamespace("BiocManager", quietly = TRUE))
#   install.packages("BiocManager")
# BiocManager::install("rhdf5")

library(pacman)
p_load(tidyverse,
       data.table,
       tsfeatures,
       future)

# variables
window_sec <- 5
frequency_Hz <- 50



# Processing the free-romaing data ----------------------------------------
for (collar in 2:15) {
  CollarNum <- paste0("Collar_", collar)
  
  unlabelled_files <- list.files(
    file.path(base_path, "RawData", CollarNum, "Board", "Chunked"),
    pattern = "\\.RDA$", full.names = TRUE
  )
  
  unlabelled_data <- lapply(unlabelled_files, function(x) {
    # load() returns the name of the loaded object
    obj_name <- load(x)
    accel_data <- get(obj_name)
    
    setDT(accel_data)
    accel_data <- accel_data[, .(RawAX, RawAY, RawAZ, gps_time_est)]
    accel_data[, ID := CollarNum]
    
    setnames(accel_data, c("Accel.X", "Accel.Y", "Accel.Z", "Time", "ID"))
    
    # process
    window_samples <- window_sec * frequency_Hz
    
    processed_data <- process_cont_VDBA(data = accel_data, window_length = window_samples)
    summarised_data <- summarise_cont_VDBA(data = processed_data, window_samples)
    
    summarised_data
  })
  
  # bind and save
  date_data <- rbindlist(unlabelled_data, use.names = TRUE, fill = TRUE)
  
  fwrite(date_data, file.path(base_path, "Output", paste0(CollarNum, "_VDBA.csv")))
}

  
  
  





# Finding the threshold between active and inactive for each species ------
source(file = file.path(base_path, "Scripts", "ThresholdingVDBA.R"))



# Plotting these results --------------------------------------------------
source(file = file.path(base_path, "Scripts", "ScalingVDBA.R"))

