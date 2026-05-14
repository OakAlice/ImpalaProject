# Creating Training Data ----------------------------------------------
# Normally this would be a simple matter of combining the matlab files into a 
# single csv however, due to learning and growing and trouble shooting
# I originally extracted the training data without the gyroscope 
# so I have to go back into the original data and get the right sections of data... but with gyro this time lol

# Extracting the right segments of data -----------------------------------
files <- list.files(file.path(base_path, "Data", "LabelledData", "MatLab(NoGyro)"), recursive = TRUE, full.names = TRUE)
# find which indiviudual and day this data came from
for (file in files){
  Collar <- basename(dirname(file))
  
  dat <- fread(file) %>%
    rename_with(tolower) %>%  # normalize all column names to lowercase
    mutate(utc_datetime = as.POSIXct((time - 719529)*86400, origin = "1970-01-01", tz = "UTC")) %>%
    select(utc_datetime, eco_behaviour, mech_behaviour)
  date <- as.Date(dat$utc_datetime[1])
  
  # extract that date from that impala
  target_files <- list.files(file.path(base_path, "Data", "RawData", Collar, "Chunked"), full.names = TRUE)
  matched <- target_files[grep(date, target_files)]
  if (length(matched) == 0) next
  load(matched)
  
  segment <- accel_data[accel_data$utc_datetime > dat$utc_datetime[1] & 
                          accel_data$utc_datetime <  dat$utc_datetime[nrow(dat)], ]
  segment[, c("gps_timestamp", "lon", "lat", "rtc_datetime", "reset_events", "date") := NULL]
  
  # add the behaviours in
  len_diff <- nrow(segment) - length(dat$eco_behaviour)
  print(len_diff)
  segment$eco_behaviour <- if (len_diff > 0) {
    c(dat$eco_behaviour, rep(0, len_diff))
  } else {
    dat$eco_behaviour[1:nrow(segment)]
  }
  segment$mech_behaviour <- if (len_diff > 0) {
    c(dat$mech_behaviour, rep(0, len_diff))
  } else {
    dat$mech_behaviour[1:nrow(segment)]
  }
  
  # save it
  fwrite(segment, file.path(base_path, "Data", "LabelledData", "IncludingGyroscope", paste0(Collar, "_", basename(file))))
}

# Now stitch the data together ------------------------------------------
files <- list.files(file.path(base_path, "Data", "LabelledData", "IncludingGyroscope"), recursive = TRUE, full.names = TRUE)
raw_data <- lapply(files, function(file) {
  df <- fread(file)
  individual <- paste0("Collar_", str_split(basename(file), "_", simplify = TRUE)[2])
  df$ID <- individual
  df[, (1:3) := lapply(.SD, as.character), .SDcols = 1:3]
})
raw_data <- bind_rows(raw_data)

# fix the behvaioours -------------------------------------------------
mech_labels <- fread(file.path(base_path, "Data/Functional Behaviours.csv")) %>%
  rename(mech_behaviour = Num)
raw_data <- left_join(raw_data, mech_labels, by = "mech_behaviour") %>%
  rename(Number = "mech_behaviour")

# read in the behaviour conversion key
if(file.exists(file.path(base_path, "Notes", "BehaviourConversionKey.csv"))){
  key <- fread(file.path(base_path, "Notes", "BehaviourConversionKey.csv"))
  
  raw_data <- left_join(raw_data, key, by = "Activity") %>%
    select(-c(Why))
}

# combine the individual behaviours into groups (based on the Explore_TrainingData.R file)
# use this script to play around with the labels 
rstudioapi::navigateToFile(file = file.path(base_path, "Scripts", "BehaviouralDetection", "GenerateTrainingData", "Explore_TrainingData.R"))

fwrite(raw_data, file.path(base_path, "Data", "LabelledData", "OriginalLabelledData.csv"))

# Now split it out into Activities and save each independently to clean them
save_split_dir <- file.path(base_path, "Data", "LabelledData", "Split")
if(!dir.exists(save_split_dir)){dir.create(save_split_dir)}
data_list <- split(raw_data, raw_data$GroupedActivity)
lapply(names(data_list), function(nm) {
  fwrite(data_list[[nm]], file.path(save_split_dir, paste0(nm, ".csv")), row.names = FALSE)
})

# Now go back into Matlab, clean and check the data. Remove anything that looks like an error.
