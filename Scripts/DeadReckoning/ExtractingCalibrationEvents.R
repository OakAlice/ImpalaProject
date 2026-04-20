# In this script we extract the calibration events from the impala data ----
# In other datasets this may have been managed differently 
# (e.g., the calibration might be an already separated period of time)
# but in this case, I am trying to pull out the calibration event from the full data

# This first section is for extracting the calibration times ---------------
##NOTE: this will be unique to every project and has to be managed appropriately
cal_start <- fread(path_to_calinfo) %>%
  mutate(MagCalStart = as.POSIXct(MagCalStart, format = "%d.%m.%y %H:%M:%S", tz = "Africa/Johannesburg")) %>%
  filter(Collar == basename(collar_dir)) %>%
  pull(MagCalStart)
cal_start <- as.POSIXct(cal_start, tz = "UTC")

# find the day that it occured on and pull that day worth of data
cal_date <- as.Date(cal_start)
load(file.path(chunked_dir_path, paste0("Board_Aligned_", cal_date, ".RDA"))) # comes in as accel_data

# extract the actual time
cal_data <- accel_data[
  gps_time_est >= cal_start - 60 &
    gps_time_est <= cal_start + 180
]

# plot this # does it look like a candidate calibration event?
cal_plot <- ggplot(cal_data, aes(x = gps_time_est, y = RawAX)) + geom_path()
ggsave(file.path(collar_dir, "CandidateCalibration.png"), cal_plot)

# save the portion
fwrite(cal_data, file.path(collar_dir, "calibration_data.csv"))
