#################
# Functions_DR

# Overview:
# Custom functions to assist with the dead reckoning 

#################

# Smooth and filter the data for use in the Gundog system -----------------
smooth_and_filter <- function(data, k , fs, bw_cutoff = 5, bw_order = 4){
  
  setDT(data)
  # Median smoothing (k=5)
  med_cols <- c("RawAX", "RawAY", "RawAZ", "RawMX", "RawMY", "RawMZ")
  for (col in med_cols) set(data, j = paste0(col, ".med"), value = runmed(data[[col]], k = 5))
  
  # Butterworth low-pass filter
  # determining the cutoff with the PSD
  # psd <- spectrum(day_data$RawAX, spans = c(5,5), taper = 0.1, 
  #                 plot = FALSE)
  # plot(psd$freq * fs, 10*log10(psd$spec), type = "l",
  #      xlab = "Frequency (Hz)", ylab = "Power (dB)",
  #      main = "Power Spectral Density — RawAX")
  # abline(v = 0.5, col = "steelblue", lty = 2)   # candidate cutoff
  bf <- butter(bw_order, bw_cutoff / (fs / 2), type = "low")
  for (col in med_cols) set(data, j = paste0(col, ".butt"), value = filtfilt(bf, data[[paste0(col, ".med")]]))
  
  # Rolling mean smoothing
  for (col in paste0(med_cols[1:3], ".butt")) {
    base <- sub("\\.butt$", "", col)
    data[[paste0(base, ".sm")]] <- rollapply(data[[col]], width = 50, FUN = mean, align = "center", fill = "extend")
  }
  
  for (col in paste0(med_cols[4:6], ".butt")) {
    base <- sub("\\.butt$", "", col)
    data[[paste0(base, ".sm")]] <- rollapply(data[[col]], width = 20, FUN = mean, align = "center", fill = "extend")
  }
  
  # plot it to check the differences
  # p0 <- ggplot(accel_data[1:10000,], aes(x = gps_time_est)) + geom_path(aes(y = RawAX, colour = "X")) + geom_path(aes(y = RawAY, colour = "Y")) + geom_path(aes(y = RawAZ, colour = "Z"))
  # p1 <- ggplot(accel_data[1:10000,], aes(x = gps_time_est)) + geom_path(aes(y = RawAX.med, colour = "X")) + geom_path(aes(y = RawAY.med, colour = "Y")) + geom_path(aes(y = RawAZ.med, colour = "Z"))
  # p2 <- ggplot(accel_data[1:10000,], aes(x = gps_time_est)) + geom_path(aes(y = RawAX.butt, colour = "X")) + geom_path(aes(y = RawAY.butt, colour = "Y")) + geom_path(aes(y = RawAZ.butt, colour = "Z"))
  # p0/p1/p2
  
  # and just select the columns you want
  # data[, c(paste0(med_cols, ".med"), paste0(med_cols, ".butt")) := NULL]
  
  return(data)
}

