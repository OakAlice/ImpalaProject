# Preprocessing -----------------------------------------------------------

preprocessing <- function(data, sample_rate){
  
  # smooth
  smoothed <- filter_noise(data, sample_rate)
  
  # add here anything else I'd like to do...
  # considered rotation but ultimately didn't do it...
  
  return(smoothed)
}

filter_noise <- function(data, sample_rate, cutoff = 5, order = 4) {

  # Low pass filter to remove noise from the raw accelerometer traces
  bf <- butter(
    n = order,
    W = cutoff / (sample_rate / 2),
    type = "low"
  )
  
  # Apply filtering to each axis
  data$X <- filtfilt(bf, data$X)
  data$Y <- filtfilt(bf, data$Y)
  data$Z <- filtfilt(bf, data$Z)
  
  return(data)
}







# Play around with some plotting here -------------------------------------
# beh <- "Chewing"
# compare_visual(beh)
# 
# 
# compare_visual <- function(beh){
#   normal <- plotTraceExamples(behaviours = beh, #c("Vigilant", "Chewing", "Grazing") # unique(data$Activity)
#                     data = data, 
#                     individuals = length(unique(data$ID)), 
#                     n_samples = 1000, n_col = 1)
#   
#   processed <- filter_noise(data, sample_rate = 50, cutoff = 5, order = 4)
#   smoothed <- plotTraceExamples(behaviours = beh, #c("Vigilant", "Chewing", "Grazing") # unique(data$Activity)
#                     data = processed, 
#                     individuals = length(unique(processed$ID)), 
#                     n_samples = 1000, n_col = 1)
#   
#   return(normal / smoothed)
# }

