#| Worked Example
#| Just has data for the first few days of collar 8

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

setwd("C:/Users/PC/Documents/ImpalaProject/WorkedExample")

# load in the package functions (from the Gundog package, developed by Rich Gunner)
source("Gundog.Tracks.R")
source("Gundog.Compass.R")
source("Functions_WorkedExample.R")

# Making the data ---------------------------------------------------------
# I already read the txt files together and added the gps and extracted the calibrations... load that in here
accel_data <- fread("Board_Aligned_2024-06-30.csv") # thew data I sent over gmeial

# accel_data <- accel_data %>% select(1:12, "utc_datetime", "lon", "lat", "VDBA.sm", "ME", "group_id")
# fwrite(accel_data, "Board_Aligned_2024-06-30_quat.csv")

# Clean up the GPS accounting for stationary periods
gps_data <- accel_data[!is.na(accel_data$lon), ] %>%
  select(utc_datetime, group_id, lon, lat, ME)
gps <- smooth_the_gps(gps_data, movement_column = "ME", no_movement = 0, spar_setting = 0.1)
gps_data <- gps$gps_data
gps$plot
# merge back into the accelerometer
accel_data <- merge(accel_data, gps_data,
                    by = "utc_datetime", all.x = TRUE)
setorder(accel_data, utc_datetime)

# combine the calibration data and the sampling data
accel_data <- accel_data[complete.cases(accel_data[, c("RawAX.sm", "RawMX.sm")]), ]

# Compass alignment -------------------------------------------------------
# multiple methods of possible alignment have been trialled
  # using the columsn already calculated onboard the devices...
  # code adapted from ChrisB
  
  accel_data[, c("Q9_1", "Q9_2", "Q9_3") := lapply(
    .SD, function(x) suppressWarnings(as.numeric(iconv(x, from = "", to = "UTF-8", sub = NA)))),
  .SDcols = c("Q9_1", "Q9_2", "Q9_3")]
  
  # get the quaternions
  q1 <- as.numeric(accel_data$Q9_1)
  q2 <- as.numeric(accel_data$Q9_2)
  q3 <- as.numeric(accel_data$Q9_3)
  q0 = sqrt( 1.0 - ((q1 * q1) + (q2 * q2) + (q3 * q3)))
  qw = q0
  qx = q2
  qy = q1
  qz = -q3
  
  # Roll
  t0 = +2.0 * (qw * qx + qy * qz)
  t1 = +1.0 - 2.0 * (qx^2 + qy^2)
  accel_data$Roll = atan2(t0, t1) * 180.0 / pi
  
  # Pitch
  t2 <- +2.0 * (qw * qy - qx * qz)
  t2 <- ifelse(t2 > 1.0, 1.0, t2)
  t2 <- ifelse(t2 < -1.0, -1.0, t2)
  accel_data$Pitch <- asin(t2) * 180.0 / pi
  
  # # Yaw
  t3 <- +2.0 * (qw * qz + qx * qy)
  t4 <- +1.0 - 2.0 * (qy^2 + qz^2)
  accel_data$Yaw <- atan2(t3, t4) * 180.0 / pi
  
  
  if(any(is.na(accel_data$Yaw))){
    temp <- which(is.na(accel_data$Yaw))
    accel_data <- accel_data[-temp, ]
  }
  # RPY <- cbind(accel_data$Roll,accel_data$Pitch,accel_data$Yaw)
  # dev.new()
  # matplot(RPY, type = "l", lty = 1, col = c("blue", "red","green"), lwd = 2, xlab = "Index", ylab = "Rotation")
  # legend("topright", legend = c("Roll", "Pitch", "Yaw"), col = c("blue", "red", "green"), lty = 1, lwd = 2, cex = 0.75, bg = "transparent", bty = "n")

  correcteddata <- accel_data
  
# Gundog.Tracks -----------------------------------------------------------
# and then use the gps to do VPC
first_lo <- na.omit(correcteddata$lon.sm)[1]
first_lat <- na.omit(correcteddata$lat.sm)[1]

projected_path2 = with(correcteddata, Gundog.Tracks(TS = utc_datetime, h = Yaw, v = VDBA.sm, 
                                                    ME = ME,
                                                    lo = first_lo,
                                                    la = first_lat,
                                                    VP.lon = lon.sm, 
                                                    VP.lat = lat.sm,
                                                    method = "All",
                                                    plot = TRUE,
                                                    bound = FALSE))


