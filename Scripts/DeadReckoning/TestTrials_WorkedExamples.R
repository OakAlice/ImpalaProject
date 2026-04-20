## Investigating DR accuracy during a test in which the user walked around the a rugby field with the device held horizontal, and with the device pitched up 45 degress
# replicating jojos exact workflow...

#### PART ONE: FLAT TRIAL ####

## Setup -------------------------------------------------------
pacman::p_load(tidyverse, data.table, plotly, rgl, zoo, lubridate, dygraphs, xts, gridExtra)
source(file.path(base_path, "Scripts/DeadReckoning/Functions/Gundog.Tracks.R"))
source(file.path(base_path, "Scripts/DeadReckoning/Functions/Gundog.Compass.R"))
source(file.path(base_path, "Scripts/DeadReckoning/Functions/Custom_Functions.R"))
source(file.path(base_path, "Scripts/ReadingData/Custom_Functions.R"))

base_path      <- "~/DeadReckoning"
test_condition <- "flat"
example_dir    <- file.path(base_path, "Data", "Test", "rugby_field", test_condition)

## 1. READ & FORMAT ACCELEROMETER DATA -------------------------
setDTthreads(0L)

accel_files <- list.files(example_dir, pattern = "dataLog*", full.names = TRUE)[1]
df.flat     <- fread(accel_files)

df.flat[, c("RawAX", "RawAY", "RawAZ")] <- df.flat[, c("RawAX", "RawAY", "RawAZ")] / 2048
df.flat[, c("RawMX", "RawMY", "RawMZ")] <- df.flat[, c("RawMX", "RawMY", "RawMZ")] * 0.15

df.flat$rtc_datetime <- as.POSIXct(
  paste(df.flat$rtcDate, df.flat$rtcTime),
  format = "%d/%m/%Y %H:%M:%OS",
  tz = "UTC"
)

## 2. READ & FORMAT GPS DATA -----------------------------------
gps_files <- list.files(example_dir, "serialLog*", full.names = TRUE)
gps.flat  <- fread(gps_files)

gps.flat <- gps.flat[, c(1, 3, 4, 6, 7)]
colnames(gps.flat) <- c("internal_rtc_datetime", "gps_date", "gps_time", "lon", "lat")
gps.flat <- gps.flat %>%
  mutate(
    internal_rtc_datetime = gsub(",", " ", internal_rtc_datetime),
    lon = as.numeric(gsub(",", "", gsub("Lon:", "", lon))),
    lat = as.numeric(gsub("Lat:", "", lat)),
    gps_rtc_datetime = paste(gps_date, gps_time)
  ) %>%
  select(internal_rtc_datetime, gps_rtc_datetime, lon, lat) %>%
  mutate(
    internal_rtc_datetime = as.POSIXct(internal_rtc_datetime, format = "%d/%m/%Y %H:%M:%OS", tz = "UTC"),
    gps_rtc_datetime      = as.POSIXct(gps_rtc_datetime,      format = "%d/%m/%Y %H:%M:%OS", tz = "UTC")
  )

## 3. JOIN ACCEL + GPS -----------------------------------------
setkey(df.flat,  rtc_datetime)
setkey(gps.flat, internal_rtc_datetime)

df.flat[
  gps.flat,
  on      = .(rtc_datetime = internal_rtc_datetime),
  roll    = "nearest",
  mult    = "first",
  `:=`(
    gps_rtc_datetime = i.gps_rtc_datetime,
    gps_lon          = i.lon,
    gps_lat          = i.lat
  )
]

## 4. SMOOTH & CALCULATE VDBA ----------------------------------
acc_cols <- c("RawAX", "RawAY", "RawAZ")
mag_cols <- c("RawMX", "RawMY", "RawMZ")

for (col in acc_cols)
  df.flat[[paste0(col, ".sm")]] <- rollapply(df.flat[[col]], width = 50, FUN = mean, align = "center", fill = "extend")
for (col in mag_cols)
  df.flat[[paste0(col, ".sm")]] <- rollapply(df.flat[[col]], width = 20, FUN = mean, align = "center", fill = "extend")

sm_cols <- paste0(acc_cols, ".sm")
df.flat$VDBA    <- sqrt((df.flat[[acc_cols[1]]] - df.flat[[sm_cols[1]]])^2 +
                          (df.flat[[acc_cols[2]]] - df.flat[[sm_cols[2]]])^2 +
                          (df.flat[[acc_cols[3]]] - df.flat[[sm_cols[3]]])^2)
df.flat$VDBA.sm <- rollapply(df.flat$VDBA, width = 50, FUN = mean, align = "center", fill = "extend")


## 5. CROP TO TRIAL PERIOD -------------------------------------
df.sub.flat      <- df.flat[22000:69000]
df.sub.flat$ME   <- 0

## 6. LOAD & FORMAT CALIBRATION DATA --------------------------
cal.df <- fread(file.path(base_path, "Data", "Test", "rugby_field", "calibration", "calIMUdf.txt"))
cal.df <- cal.df %>% rename(RawMX = mX, RawMY = mY, RawMZ = mZ,
                            RawAX = aX, RawAY = aY, RawAZ = aZ)
cal.df$rtc_datetime <- as.POSIXct(
  paste(cal.df$rtcDate, paste0("00:", cal.df$rtcTime)),
  format = "%d/%m/%Y %H:%M:%OS",
  tz = "UTC"
)

for (col in acc_cols)
  cal.df[[paste0(col, ".sm")]] <- rollapply(cal.df[[col]], width = 50, FUN = mean, align = "center", fill = "extend")
for (col in mag_cols)
  cal.df[[paste0(col, ".sm")]] <- rollapply(cal.df[[col]], width = 20, FUN = mean, align = "center", fill = "extend")

cal.df$VDBA    <- sqrt((cal.df[[acc_cols[1]]] - cal.df[[sm_cols[1]]])^2 +
                         (cal.df[[acc_cols[2]]] - cal.df[[sm_cols[2]]])^2 +
                         (cal.df[[acc_cols[3]]] - cal.df[[sm_cols[3]]])^2)
cal.df$VDBA.sm <- rollapply(cal.df$VDBA, width = 50, FUN = mean, align = "center", fill = "extend")
cal.df$ME      <- "M"


## 7. COMBINE CALIBRATION + TEST DATA -------------------------
keep_cols <- c("RawAX.sm", "RawAY.sm", "RawAZ.sm", "RawMX.sm", "RawMY.sm", "RawMZ.sm", "ME")
foo       <- rbind(
  cal.df[,      ..keep_cols],
  df.sub.flat[, ..keep_cols]
)

## 8. RUN GUNDOG.COMPASS ---------------------------------------
# NOTE: mag.ref.frame = "NWU" not "NED" - Jojo found mag axes differ from accel axes
foo.ang <- with(foo,
                Gundog.Compass(
                  mag.x = RawMX.sm, mag.y = RawMY.sm, mag.z = RawMZ.sm,
                  acc.x = RawAX.sm, acc.y = RawAY.sm, acc.z = RawAZ.sm,
                  ME            = ME,
                  acc.ref.frame = "NED",
                  positive.g    = "up",
                  mag.ref.frame = "NWU",   # <-- key fix from Jojo's workflow
                  pitch.offset  = 0, roll.offset = 0, yaw.offset = 0,
                  method        = 3,
                  algorithm     = "SAAM",
                  plot          = TRUE
                )
)

## 9. EXTRACT CORRECTED TEST DATA ------------------------------
foo.ang.test <- foo.ang[foo.ang$ME != "M", c("Roll", "Pitch", "Yaw")]
df.sub.flat  <- cbind(df.sub.flat, foo.ang.test)

# Inspect angles
plot1 <- ggplot(df.sub.flat, aes(x = rtc_datetime)) +
  geom_line(aes(y = Pitch, color = "Pitch")) +
  geom_line(aes(y = Roll,  color = "Roll"))  +
  scale_color_manual(values = c("Pitch" = "red", "Roll" = "green")) +
  labs(y = "Angle (degrees)", color = "Body Angle") +
  ylim(-180, 180) + theme_minimal()

plot2 <- ggplot(df.sub.flat, aes(x = rtc_datetime, y = Yaw)) +
  geom_line(color = "blue") +
  labs(y = "Heading (degrees)", x = "Time") +
  ylim(0, 360) + theme_minimal()

grid.arrange(plot1, plot2, ncol = 1)

## 10. DEAD RECKONING (no GPS correction) ----------------------
df.flat.dr <- with(df.sub.flat,
                   Gundog.Tracks(
                     TS     = rtc_datetime,
                     h      = Yaw,
                     v      = VDBA.sm,
                     method = NULL,
                     plot   = TRUE
                   )
)

## 11. DEAD RECKONING (GPS-corrected) --------------------------
first_lon <- head(df.sub.flat$gps_lon[df.sub.flat$gps_lon != 0 & !is.na(df.sub.flat$gps_lon)], 1)
first_lat <- head(df.sub.flat$gps_lat[df.sub.flat$gps_lat != 0 & !is.na(df.sub.flat$gps_lat)], 1)

df.flat.dr.gps <- with(df.sub.flat,
                       Gundog.Tracks(
                         TS      = rtc_datetime,
                         h       = Yaw,
                         v       = VDBA.sm,
                         lo      = first_lon,
                         la      = first_lat,
                         VP.lon  = gps_lon,
                         VP.lat  = gps_lat,
                         method  = "All",
                         plot    = TRUE,
                         bound   = FALSE
                       )
)






#### PART TWO: 45 DEGREE ANGLE TRIAL ####

## Setup -------------------------------------------------------
test_condition <- "45"
example_dir    <- file.path(base_path, "Data", "Test", "rugby_field", test_condition)

## 1. READ & FORMAT ACCELEROMETER DATA -------------------------
accel_files <- list.files(example_dir, pattern = "dataLog*", full.names = TRUE)[1]
df.45    <- fread(accel_files)

df.45[, c("RawAX", "RawAY", "RawAZ")] <- df.45[, c("RawAX", "RawAY", "RawAZ")] / 2048
df.45[, c("RawMX", "RawMY", "RawMZ")] <- df.45[, c("RawMX", "RawMY", "RawMZ")] * 0.15

df.45$rtc_datetime <- as.POSIXct(
  paste(df.45$rtcDate, df.45$rtcTime),
  format = "%d/%m/%Y %H:%M:%OS",
  tz = "UTC"
)

## 2. READ & FORMAT GPS DATA -----------------------------------
gps_files <- list.files(example_dir, "serialLog*", full.names = TRUE)
gps.45  <- fread(gps_files)

gps.45 <- gps.45[, c(1, 3, 4, 6, 7)]
colnames(gps.45) <- c("internal_timestamp", "gps_date", "gps_time", "lon", "lat")
gps.45 <- gps.45 %>%
  mutate(
    internal_timestamp = gsub(",", " ", internal_timestamp),
    lon = as.numeric(gsub(",", "", gsub("Lon:", "", lon))),
    lat = as.numeric(gsub("Lat:", "", lat)),
    gps_timestamp = paste(gps_date, gps_time)
  ) %>%
  select(internal_timestamp, gps_timestamp, lon, lat) %>%
  mutate(
    internal_timestamp = as.POSIXct(internal_timestamp, format = "%d/%m/%Y %H:%M:%OS", tz = "UTC"),
    gps_timestamp      = as.POSIXct(gps_timestamp,      format = "%d/%m/%Y %H:%M:%OS", tz = "UTC")
  )

## 3. JOIN ACCEL + GPS -----------------------------------------
setkey(df.45,  rtc_datetime)
setkey(gps.45, internal_timestamp)

df.45[
  gps.45,
  on      = .(rtc_datetime = internal_timestamp),
  roll    = "nearest",
  mult    = "first",
  `:=`(
    gps_rtc_datetime = i.gps_timestamp,
    gps_lon          = i.lon,
    gps_lat          = i.lat
  )
]

## 4. SMOOTH & CALCULATE VDBA ----------------------------------
acc_cols <- c("RawAX", "RawAY", "RawAZ")
mag_cols <- c("RawMX", "RawMY", "RawMZ")

for (col in acc_cols)
  df.45[[paste0(col, ".sm")]] <- rollapply(df.45[[col]], width = 50, FUN = mean, align = "center", fill = "extend")
for (col in mag_cols)
  df.45[[paste0(col, ".sm")]] <- rollapply(df.45[[col]], width = 20, FUN = mean, align = "center", fill = "extend")

sm_cols <- paste0(acc_cols, ".sm")
df.45$VDBA    <- sqrt((df.45[[acc_cols[1]]] - df.45[[sm_cols[1]]])^2 +
                        (df.45[[acc_cols[2]]] - df.45[[sm_cols[2]]])^2 +
                        (df.45[[acc_cols[3]]] - df.45[[sm_cols[3]]])^2)
df.45$VDBA.sm <- rollapply(df.45$VDBA, width = 50, FUN = mean, align = "center", fill = "extend")

## 5. CROP TO TRIAL PERIOD -------------------------------------
df.sub.45     <- df.45[12500:nrow(df.45)]
df.sub.45$ME   <- 0

## 6. LOAD & FORMAT CALIBRATION DATA --------------------------
# if not still in the environment, run the cal.df section from above
cal.df$ME      <- "M"

## 7. COMBINE CALIBRATION + TEST DATA -------------------------
keep_cols <- c("RawAX.sm", "RawAY.sm", "RawAZ.sm", "RawMX.sm", "RawMY.sm", "RawMZ.sm", "ME")
foo       <- rbind(
  cal.df[,      ..keep_cols],
  df.sub.45[, ..keep_cols]
)

## NEW. CALCULATE THE ANGLES -----------------------------------
# because the device is not perfepctly flat this time, we need to account for the angles
orientation <- check_orientation(df.sub.45, acc_cols, mag_cols)
orientation$accel_graph
orientation$mag_graph
orientation$orientation_table

pitch <- compute_pitch(orientation$orientation_table[1,]$mean, 
                       orientation$orientation_table[2,]$mean, 
                       orientation$orientation_table[3,]$mean)

## 8. RUN GUNDOG.COMPASS ---------------------------------------
# NOTE: mag.ref.frame = "NWU" not "NED" - Jojo found mag axes differ from accel axes
foo.ang <- with(foo,
                Gundog.Compass(
                  mag.x = RawMX.sm, mag.y = RawMY.sm, mag.z = RawMZ.sm,
                  acc.x = RawAX.sm, acc.y = RawAY.sm, acc.z = RawAZ.sm,
                  ME            = ME,
                  acc.ref.frame = "NED",
                  positive.g    = "up",
                  mag.ref.frame = "NWU",   # again, jojo figured this out
                  pitch.offset  = pitch, roll.offset = 0, yaw.offset = 0,
                  method        = 3,
                  algorithm     = "SAAM",
                  plot          = TRUE
                )
)

## 9. EXTRACT CORRECTED TEST DATA ------------------------------
foo.ang.test <- foo.ang[foo.ang$ME != "M", c("Roll", "Pitch", "Yaw")]
df.sub.45  <- cbind(df.sub.45, foo.ang.test)

# Inspect angles
plot1 <- ggplot(df.sub.45, aes(x = rtc_datetime)) +
  geom_line(aes(y = Pitch, color = "Pitch")) +
  geom_line(aes(y = Roll,  color = "Roll"))  +
  scale_color_manual(values = c("Pitch" = "red", "Roll" = "green")) +
  labs(y = "Angle (degrees)", color = "Body Angle") +
  ylim(-180, 180) + theme_minimal()

plot2 <- ggplot(df.sub.45, aes(x = rtc_datetime, y = Yaw)) +
  geom_line(color = "blue") +
  labs(y = "Heading (degrees)", x = "Time") +
  ylim(0, 360) + theme_minimal()

grid.arrange(plot1, plot2, ncol = 1)

## 10. DEAD RECKONING (no GPS correction) ----------------------
df.45.dr <- with(df.sub.45,
                 Gundog.Tracks(
                   TS     = rtc_datetime,
                   h      = Yaw,
                   v      = VDBA.sm,
                   method = NULL,
                   plot   = TRUE
                 )
)

## 11. DEAD RECKONING (GPS-corrected) --------------------------
first_lon <- head(df.sub.45$gps_lon[df.sub.45$gps_lon != 0 & !is.na(df.sub.45$gps_lon)], 1)
first_lat <- head(df.sub.45$gps_lat[df.sub.45$gps_lat != 0 & !is.na(df.sub.45$gps_lat)], 1)

df.45.dr.gps <- with(df.sub.45,
                     Gundog.Tracks(
                       TS      = rtc_datetime,
                       h       = Yaw,
                       v       = VDBA.sm,
                       lo      = first_lon,
                       la      = first_lat,
                       VP.lon  = gps_lon,
                       VP.lat  = gps_lat,
                       method  = "All",
                       plot    = TRUE,
                       bound   = FALSE
                     )
)


