# Figure out the orientation of the board ---------------------------------
# did this manually b y plotting each of the individuals

Collar <- "Collar_15"  
chunked_dir_path <- file.path(base_path, "Data", "RawData", Collar, "Chunked")

# load in a day of data
file <- list.files(file.path(chunked_dir_path), full.names = TRUE)[5]
load(file)

# see whether it was during the deployment period
start_time <- fread(path_to_calinfo) %>%
  dplyr::filter(Collar == basename(collar_dir)) %>%
  mutate(DeploymentStart = as.POSIXct(DeploymentStart, format = "%d.%m.%Y %H:%M:%S", tz = "Africa/Johannesburg")) %>%
  pull(DeploymentStart)
start_time <- as.POSIXct(start_time, tz = "UTC")

accel_data <- accel_data[gps_time_est >= start_time]
if(nrow(accel_data)==0){ # if this was before deployment, then just delete it
  print("this was before deployment began, skipping")
  next
}


means <- accel_data %>%
  summarise(X = mean(RawAX),
            Y = mean(RawAY),
            Z = mean(RawAZ))


play <- accel_data[920000:940000]

# determine whether this is in the standard expected orientation
ggplot(play, aes(x = rtc_datetime)) + 
  geom_path(aes(y = RawAX, colour = "X")) + 
  geom_path(aes(y = RawAY, colour = "Y")) + 
  geom_path(aes(y = RawAZ, colour = "Z")) + 
  theme_minimal()



