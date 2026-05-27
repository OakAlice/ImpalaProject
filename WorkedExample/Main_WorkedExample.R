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
accel_data <- fread("Board_Accel_Section.csv")
cal_data <- fread("calibration_data.csv")

# scale the variables
accel_data <- scale_variables(accel_data)
cal_data <- scale_variables(cal_data)

# filter the noise with a median filter and then a low-pass butterworth filter
accel_data <- clean_noise(accel_data, med_k = 5)
cal_data <- clean_noise(cal_data, med_k = 5)

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

# Compass alignment -------------------------------------------------------
# multiple methods of possible alignment have been trialled
if (compass_method == "Gundog.Compass"){
    
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
  
  # accel_data <- accel_data[, ..keep_cols]
  # accel_data[, c("Roll", "Pitch", "Yaw") := lapply(
  #   .SD, function(x) suppressWarnings(as.numeric(iconv(x, from = "", to = "UTF-8", sub = NA)))),
  # .SDcols = c("Q9_1", "Q9_2", "Q9_3")]
  
  # and then would somehow convert them to the NED orientation from the NWU orientation??
  print("unfinished and IDK")
  
} else if (compass_method == "Magdwick"){
  print("ENTER MANUAL MODE")
  
  accel_data <- accel_data[, ..keep_cols]
  fwrite(accel_data, "Board_Accel_Section_Cleaned.csv")
  
  # now go into python and run the MagdwickCompass.py 
  # come back here and load them in...
  accel_data <- fread("Board_Accel_Section_Cleaned.csv")
  eulers <- fread("Board_Accel_Section_Cleaned_Compass.csv") %>% select(utc_datetime, Roll, Pitch, Yaw)
  
  correcteddata <- merge(accel_data, eulers, by = "utc_datetime")
}

# projected_path <- with(correcteddata, Gundog.Tracks(TS = utc_datetime, h = Yaw, v = VDBA.sm,
#                                                     ME = ME,
#                                                     method = NULL,
#                                                     plot = TRUE))

# Gundog.Tracks -----------------------------------------------------------
# and then use the gps to do VPC
correcteddata <- fread("Board_Accel_Section_Gundog.Compass.csv")

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

fwrite(projected_path2, "Board_Accel_Section_Gundog.Tracks.csv")

# Estimate of speeds ------------------------------------------------------
# projected_path2 <- fread("Board_Accel_Section_Gundog.Tracks.csv")

play <- projected_path2[1:1000000,]
setDT(play)
play[, sec_bin := floor(DR.seconds)]
result <- play[, .(per.sec.DR.distance.2D = sum(DR.distance.2D, na.rm = TRUE)),
               by = sec_bin]
play <- merge(play, result, by = "sec_bin")

ggplot(play, aes(x = per.sec.DR.distance.2D)) +
  geom_freqpoly(binwidth = 0.1)


ggplot(play, aes(x = DR.longitude, y = DR.latitude)) + geom_point()




projected_path2$DR.distance.2D

head(play)






