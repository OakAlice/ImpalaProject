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
accel_data <- fread("Board_Aligned_2024-06-30.csv") # or any of them that I included
cal_data <- fread("calibration_data.csv")

# moving / not moving (VDBA and sd of VDBA)
accel_data <- activity_scoring(accel_data, threshold = 0.005)
cal_data <- activity_scoring(cal_data, threshold = 0.005)
cal_data[["ME"]] <- NULL
cal_data$ME <- "M" # overwrite M for the calibration period

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
if (compass_method == "Gundog.Compass"){
  
  all_data <- rbind(cal_data, accel_data, fill = TRUE)
  keep_cols <- c("utc_datetime",
                 #"Q9_1", "Q9_2", "Q9_3",
                 "RawAX.sm", "RawAY.sm", "RawAZ.sm",
                 "RawMX.sm", "RawMY.sm", "RawMZ.sm",
                 "RawGX.sc", "RawGY.sc", "RawGZ.sc",
                 "VDBA.sm",
                 "lon.sm", "lat.sm",
                 "ME")
  all_data <- all_data[, ..keep_cols]
    
  # see the docx for how we determined these orientations
  acc_orientation <- "NWU"
  mag_orientation <- "NED"
  gravity_direction <- "down"
  
  # pitch was determined from extracting known walking events and then taking the mean of axes during those times
  pitch <- atan2(-(-0.253599267), sqrt(0.259529436^2 + 0.891019352^2))
  pitch_deg <- pitch * 180 / pi
  
  # Gundog.Compass
  alldata_rotated <- with(all_data, Gundog.Compass(mag.x = RawMX.sm, mag.y = RawMY.sm, mag.z = RawMZ.sm,
                                                   acc.x = RawAX.sm, acc.y = RawAY.sm, acc.z = RawAZ.sm,
                                                   ME = ME,
                                                   acc.ref.frame = acc_orientation, 
                                                   positive.g = gravity_direction, 
                                                   mag.ref.frame = mag_orientation,
                                                   pitch.offset = -pitch_deg, 
                                                   roll.offset = 0,
                                                   yaw.offset = 0,
                                                   method = 2,
                                                   algorithm = "standard",
                                                   plot = TRUE))
  
  # Remove the calibration data and now you have your corrected trial data.
  setDT(alldata_rotated)
  correcteddata <- cbind(all_data, alldata_rotated[, c("Pitch", "Roll", "Yaw")])
  correcteddata <- correcteddata %>% dplyr::filter(ME != "M")
  
  # wfrite(correcteddata, "Board_Accel_Section_Gundog.Compass.csv")
  
} else if (compass_method == "Onboard_calcs"){
  
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
  
} 

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


