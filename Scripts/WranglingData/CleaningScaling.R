# R code for cleaning and scaling the implaa data -------------------------
# scaling the axes to be all the same units across devices
# removing misreads and smoothing with butterworth

# firstly need to convert the axes to the right units
# find the right converion from the information sheet produced earlier
scales <- fread(file.path("Data", "True_collar_settings.csv")) %>%
  dplyr::filter(CollarNum == Collar)
acc_cov <- if(grepl("/2048", scales$Acc_Conv)){ "2048" } else {NA}
mag_cov <- if(grepl("/2048", scales$Mag_Conv)){ "2048" } else {NA}

days <- list.files(file.path(collar_dir, "Chunked"), full.names = TRUE)
for(day in days){
  load(day) # comes in accel_data
  
  # convert the axes
  if (acc_cov == 2048) {
    accel_data[, (c("RawAX", "RawAY", "RawAZ")) := lapply(.SD, function(x) x / 4096), .SDcols = c("RawAX", "RawAY", "RawAZ")]
  }
  if (mag_cov == 2048) {
    accel_data[, (c("RawMX", "RawMY", "RawMZ")) := lapply(.SD, function(x) x / 4096), .SDcols = c("RawMX", "RawMY", "RawMZ")]
  }
  
  # and now all the mags get scaled as well
  accel_data[, (c("RawMX", "RawMY", "RawMZ")) := lapply(.SD, function(x) x * 0.15), .SDcols = c("RawMX", "RawMY", "RawMZ")]
  
  # and now the median filter
  
}