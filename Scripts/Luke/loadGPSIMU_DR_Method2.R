loadGPSIMU_DR_Method2 <- function(filesdir){
  setwd(filesdir)
  GPSfiles <- list.files(pattern = "seriallogclean", ignore.case = TRUE)
  IMUfiles <- list.files(pattern = "datalog", ignore.case = TRUE)
  ## Load GPS data into data frame
  for(i in 1:length(GPSfiles)){
    tempdf <- read.delim(GPSfiles[i])
    # Go through GPS text file and make so that it can be separated by a delimiter,
    # in this case by ",". If the formatting of the serialLog files change, this
    # code may need to be adjusted for changes in character positions
    for(j in 1:nrow(tempdf)){
      tempstr <- as.character(tempdf[j,1])
      substr(tempstr,11,11) <- ","; substr(tempstr,23,25) <- ",  "; substr(tempstr,36,36) <- ","; 
      substr(tempstr,45,47) <- ",  "; tempstr <- sub("(.{1,62})( *)", "\\1 \\2", tempstr); 
      tempstr <- gsub('[  ]', '', tempstr); tempstr <- gsub('lon:','',tempstr,ignore.case = TRUE); 
      tempstr <- gsub('lat:','',tempstr,ignore.case = TRUE); tempdf[j,1] <- tempstr
    }
    tempdf[c("ArtemisDate","ArtemisTime","SatelliteDate","SatelliteTime","Longitude","Latitude")] <- do.call(rbind, strsplit(tempdf$Artemis.Time...Satellite.Time...Coordinates, ","))
    tempdf <- tempdf[, -c(1)]
    if(sum(nchar(tempdf[,1])) != 10*nrow(tempdf) || sum(nchar(tempdf[,2])) != 11*nrow(tempdf)){
      stop(paste("one or more lines in", basename(GPSfiles[i]), "contain error/s", sep=" "))
    }
    if (i==1){
      GPSdata <- tempdf
    } else {
      GPSdata <- rbind(GPSdata,tempdf)
    }
  }
  ## Load IMU data into data frame
  for(i in 1:length(IMUfiles)){
    tempdf <- fread(IMUfiles[i])
    # Remove unused data columns
    tempdf <- subset(tempdf, select = -c(RawGX,RawGY,RawGZ,output_Hz))
    column_to_remove <- "V17"
    if (column_to_remove %in% names(tempdf)) {
      tempdf <- subset(tempdf, select = -c(V17))
    }
    if (i==1){
      IMUdata <- tempdf
    } else {
      IMUdata <- rbind(IMUdata,tempdf)
    }
  }
  rm(tempdf)
  ## Create idexable timestamps that can be used to determine when GPS fixes 
  ## occur relative to IMU data
  GPSdata <- DateTimeStrGPS(GPSdata) # Function creates an indexable GPS timestamp
  IMUdata <- DateTimeStrIMU(IMUdata) # Function creates an indexable IMU timestamp
  IMUdata$GPSfix <- rbind(IMUdata$GPSfix, 0)
  IMUdata$SatelliteTime <- rbind(IMUdata$SatelliteTime, 0)
  IMUdata$SatelliteDate <- rbind(IMUdata$SatelliteDate, 0)
  ## divide raw accelerometer and magnetometer data by the appropriate scale factors
  IMUdata$aX <- (IMUdata$RawAX/2048)
  IMUdata$aY <- (IMUdata$RawAY/2048)
  IMUdata$aZ <- (IMUdata$RawAZ/2048)
  IMUdata$mX <- (IMUdata$RawMX*0.15)
  IMUdata$mY <- (IMUdata$RawMY*0.15)
  IMUdata$mZ <- (IMUdata$RawMZ*0.15)
  ## Remove outliers and compute VeDBA
  response <- askYesNo("Have you removed outliers from the IMU data?")
  if (response) {

  } else {
    outliers <- OutlierSelection(IMUdata)
    IMUdata <- OutlierRemoval(IMUdata,outliers)
  }
  IMUdata <- subset(IMUdata, select = -c(RawAX,RawAY,RawAZ,RawMX,RawMY,RawMZ))
  IMUdata$aX.sm <- rollapply(IMUdata$aX, width=100, FUN=mean, align="center", fill="extend")
  IMUdata$aY.sm <- rollapply(IMUdata$aY, width=100, FUN=mean, align="center", fill="extend")
  IMUdata$aZ.sm <- rollapply(IMUdata$aZ, width=100, FUN=mean, align="center", fill="extend")
  IMUdata$VeDBA = sqrt((IMUdata$aX - IMUdata$aX.sm)^2 + (IMUdata$aY - IMUdata$aY.sm)^2 + (IMUdata$aZ - IMUdata$aZ.sm)^2)                     
  IMUdata$VeDBA.sm = rollapply(IMUdata$VeDBA, width=100, FUN=mean, align="center", fill="extend")
  ## Compute pitch, roll, yaw
  IMUdata$Q9_1 <- iconv(IMUdata$Q9_1, from = "ISO-8859-1", to = "UTF-8") # Without this, errors are thrown when converting the quaternion outputs to numeric
  q1 <- as.numeric(IMUdata$Q9_1)
  IMUdata$Q9_2 <- iconv(IMUdata$Q9_2, from = "ISO-8859-1", to = "UTF-8")
  q2 <- as.numeric(IMUdata$Q9_2)
  IMUdata$Q9_3 <- iconv(IMUdata$Q9_3, from = "ISO-8859-1", to = "UTF-8")
  q3 <- as.numeric(IMUdata$Q9_3)
  q0 = sqrt( 1.0 - ((q1 * q1) + (q2 * q2) + (q3 * q3)))
  qw = q0
  qx = q2
  qy = q1
  qz = -q3
  # Roll 
  t0 = +2.0 * (qw * qx + qy * qz);
  t1 = +1.0 - 2.0 * (qx^2 + qy^2);
  IMUdata$Roll = atan2(t0, t1) * 180.0 / pi;
  # Pitch
  t2 <- +2.0 * (qw * qy - qx * qz)
  t2 <- ifelse(t2 > 1.0, 1.0, t2)
  t2 <- ifelse(t2 < -1.0, -1.0, t2)
  IMUdata$Pitch <- asin(t2) * 180.0 / pi
  # Yaw
  t3 <- +2.0 * (qw * qz + qx * qy)
  t4 <- +1.0 - 2.0 * (qy^2 + qz^2)
  IMUdata$Yaw <- atan2(t3, t4) * 180.0 / pi
  if(any(is.na(IMUdata$Yaw))){
    temp <- which(is.na(IMUdata$Yaw))
    IMUdata <- IMUdata[-temp, ]
  }
  IMUdata$Yaw = ifelse(IMUdata$Yaw < 0, IMUdata$Yaw + 360, IMUdata$Yaw)
  ## Determine when GPS fixes occur relative to IMU data
  tempIMU <- rep(0,nrow(GPSdata))
  tempGPS <- rep(0,nrow(GPSdata))
  x <- rep(0,nrow(GPSdata))
  for(i in 1:nrow(GPSdata)){
    matching_indices <- which(IMUdata$DateTimeIdx == GPSdata$DateTimeIdx[i])
    close_indices <- which(abs(IMUdata$DateTimeIdx - GPSdata$DateTimeIdx[i]) == 1)
    if(length(matching_indices) > 0){
      tempIMU[i] <- matching_indices
      tempGPS[i] <- i
    } else if(length(close_indices) > 0){
      tempIMU[i] <- min(close_indices)
      tempGPS[i] <- i
    } else {
      
    }
  }
  IMUdata$GPSfix[tempIMU] = 1
  ## Compile remaining variables of interest into one df
  IMUdata$SatelliteDate[tempIMU] <- GPSdata$SatelliteDate[tempGPS]
  IMUdata$SatelliteTime[tempIMU] <- GPSdata$SatelliteTime[tempGPS]
  df <- IMUdata; rm(IMUdata)
  df$Longitude <- rep(0, nrow(df))
  df$Latitude <- rep(0, nrow(df))
  df$Longitude[tempIMU] <- GPSdata$Longitude[tempGPS]
  df$Latitude[tempIMU] <- GPSdata$Latitude[tempGPS]
  df <- df[-c((tempIMU[length(tempIMU)]+1):nrow(df)), ]
  df <- df[-c(1:tempIMU[1]-1), ]
  df$SatelliteTime <- format(as.POSIXlt(df$SatelliteTime, format="%H:%M:%OS"),"%H:%M:%OS3")
  df$SatelliteTime <- as.character(df$SatelliteTime)
  options(digits.secs = 3)
  df$timestamp <- as.POSIXct(na.approx(strptime(paste(df$SatelliteDate, df$SatelliteTime), format = "%d/%m/%Y %H:%M:%OS", tz = "GMT")))
  df$EventID <- 1:nrow(df)
  df <- subset(df, select = -c(rtcDate,rtcTime,DateTimeIdx,SatelliteTime,SatelliteDate))
  return(df)
}