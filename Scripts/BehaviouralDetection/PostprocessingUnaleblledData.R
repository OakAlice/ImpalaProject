# Postprocessing Unaleblled Data ------------------------------------------
# probably will add in quite complicated logic about this eventually
# or like a naive bayes or something model
# but for now just do some basic mode shiiii

behaviour_files <- list.files(file.path(base_path, "Output", collar), pattern = "_Unlabelled_Predictions.csv", full.names = TRUE)
behaviour_data <- lapply(behaviour_files, function(x){
  accel <- fread(x)
  
  accel[, Minute := format(Time, "%Y-%m-%d %H:%M")]

  # get the mode
  get_mode <- function(x) {
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
  }
  
  accel_minute <- accel[, .(
    Activity = get_mode(Activity)
  ), by = .(ID, Minute)]
  
  accel_minute[, Time := as.POSIXct(Minute, format="%Y-%m-%d %H:%M", tz="UTC")]
  accel_minute <- accel_minute[, .SD, .SDcols = c("ID", "Time", "Activity")]
  
  accel_minute
    
})
behaviour_data <- rbindlist(behaviour_data) 

fwrite(behaviour_data, file.path(base_path, "Output", collar, paste0("Behaviours_smoothed.csv")))
