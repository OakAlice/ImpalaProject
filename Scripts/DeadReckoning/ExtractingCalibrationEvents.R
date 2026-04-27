# In this script we extract the calibration events from the impala data ----
# In other datasets this may have been managed differently 
# (e.g., the calibration might be an already separated period of time)
# but in this case, I am trying to pull out the calibration event from the full data

# This first section is for extracting the calibration times ---------------
##NOTE: this will be unique to every project and has to be managed appropriately

source(file = file.path(base_path, "Scripts", "DeadReckoning", "Custom_Functions.R"))
acc_cols <- c("RawAX", "RawAY", "RawAZ")
mag_cols <- c("RawMX", "RawMY", "RawMZ")

for (x in c("Start", "End")){
  
  column <- ifelse(x == "Start", "MagCalStart", "MagCalEnd")
  
  cal <- fread(path_to_calinfo) 
  
  # not all of them have an end calibration too
  if (x == "End" & !column %in% colnames(cal)){
    next
  }
  
  cal <- cal %>%
    mutate(MagCalStart = as.POSIXct(.data[[column]], format = "%d.%m.%y %H:%M:%S", tz = "Africa/Johannesburg")) %>%
    filter(Collar == basename(collar_dir)) %>%
    pull(MagCalStart)
  cal <- as.POSIXct(cal, tz = "UTC")
  
  # find the day that it occured on and pull that day worth of data
  cal_date <- as.Date(cal)
  if(file.exists(file.path(chunked_dir_path, paste0("Board_Aligned_", cal_date, ".RDA")))){
   
    load(file.path(chunked_dir_path, paste0("Board_Aligned_", cal_date, ".RDA"))) # comes in as accel_data
    # extract the actual time
    cal_data <- accel_data[
      gps_time_est >= cal - 30 &
        gps_time_est <= cal + 300
    ]
    
    # plot this # does it look like a candidate calibration event?
    cal_plot <- ggplot(cal_data, aes(x = gps_time_est, y = RawAX)) + geom_path()
    cal_plot
    ggsave(file.path(collar_dir, paste0("CandidateCalibration_", x, ".png")), 
           cal_plot,
           width = 10.5, height = 3.64, units = "cm")
    
    # save the portion
    fwrite(cal_data, file.path(collar_dir, paste0("calibration_data_", x, ".csv")))
  }
}
