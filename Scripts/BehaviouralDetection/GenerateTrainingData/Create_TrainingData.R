# Creating Training Data ----------------------------------------------
# Iterate between these steps above to clean more and more ------------
# Load in the annotations, split them out, clean them, recombine them, rename them, etc.
# This should take a while. This is an important step.

# Load in the annotated matlab data ---------------------------------------
matlab_files <- list.files(file.path(base_path, "Data", "LabelledData", "Matlab(NoGyro)"), recursive = TRUE, full.names = TRUE)
matlab_data <- lapply(matlab_files, function(file) {
  df <- fread(file)
  colnames(df) <- tolower(colnames(df))
  individual <- str_split(basename(dirname(file)), "_", simplify = TRUE)[2]
  df$id <- individual
  df <- df %>% select("time", "x", "y", "z", "id", "eco_behaviour", "mech_behaviour")
  df
})
matlab_data <- bind_rows(matlab_data)
# save this
fwrite(matlab_data, file.path(base_path, "Data", "LabelledData", "OriginalLabelledData.csv"))

# split by Activity and save back into their own files ---------------------
# Clean each of them independently in matlab to remove the inconsistencies
save_split_dir <- file.path(base_path, "Data", "LabelledData", "Split")
if(!dir.exists(save_split_dir)){dir.create(save_split_dir)}
data_list <- split(matlab_data, matlab_data$mech_behaviour)
lapply(names(data_list), function(nm) {
  fwrite(data_list[[nm]], file.path(save_split_dir, paste0(nm, ".csv")), row.names = FALSE)
})

# When they have been cleaned, add them back together ----------------------
# For each untagged file, replace with tagged version if it exists
matlab_files <- list.files(file.path(base_path, "Data", "LabelledData", "Split"), recursive = TRUE, full.names = TRUE)
tagged <- matlab_files[grepl("_tagged\\.csv$", matlab_files)]
untagged <- matlab_files[!grepl("_tagged\\.csv$", matlab_files)]
matlab_files <- sapply(untagged, function(f) {
  tagged_version <- sub("\\.csv$", "_tagged.csv", f)
  if (tagged_version %in% tagged) tagged_version else f
})
matlab_data <- lapply(matlab_files, function(file) {
  df <- fread(file)
  colnames(df) <- tolower(colnames(df))
  df
})
matlab_data <- bind_rows(matlab_data)
# fix the Id... don't know why its wrong, can't be bothered to fix error, just account for it
matlab_data <- matlab_data %>% mutate(ID = coalesce(id...5, id...8, id)) %>% select(!c(id...5, id...8, id))

# Get the gyroscope data ---------------------------------------------------
# Normally making training data would be a simple matter of combining the matlab files into a 
# single csv however, due to learning and growing and trouble shooting
# I originally extracted the training data without the gyroscope 
# so I have to go back into the original data and get the right sections of data... but with gyro this time lol
# this is completely unique to my one specific scenario and would not normally be included.




# Something went wrong in this step ---------------------------------------




matlab_sorted <- matlab_data %>%
  group_by(ID) %>%
  arrange(time, .by_group = TRUE) %>%
  mutate(time_diff = time - data.table::shift(time), # had to define package or errored
         break_point = ifelse(time_diff > 2.33e-7*500, 1, 0), # break of more than a minute
         break_point = replace_na(break_point, 0),
         sequence = cumsum(break_point)) %>%
  mutate(event_id = paste0(ID, "_", sequence)) %>%
  select(-break_point, -time_diff, -sequence) %>%
  ungroup() %>%
  dplyr::filter(!mech_behaviour == 0)
matlab_chunks <- split(matlab_sorted, matlab_sorted$event_id)

# Extracting the right segments of data -----------------------------------
# find the time and ID this came from, then go get it from the properly processed data
for (event in unique(matlab_sorted$event_id)){
  
  dat <- matlab_chunks[[event]] 
  Collar <- paste0("Collar_", dat$ID[1]) # get the ID
  dat <- dat %>%
    mutate(utc_datetime = as.POSIXct((time - 719529)*86400, origin = "1970-01-01", tz = "UTC")) %>%
    select(utc_datetime, eco_behaviour, mech_behaviour)
  date <- as.Date(dat$utc_datetime[1]) # get the date
  
  # extract that date from that impala
  target_files <- list.files(file.path(base_path, "Data", "RawData", Collar, "Chunked"), full.names = TRUE)
  matched <- target_files[grep(date, target_files)]
  if (length(matched) == 0) next
  load(matched)
  
  segment <- accel_data[accel_data$utc_datetime > dat$utc_datetime[1] & 
                          accel_data$utc_datetime <  dat$utc_datetime[nrow(dat)], ]
  segment[, c("gps_timestamp", "lon", "lat", "rtc_datetime", "reset_events", "date") := NULL]
  
  # add the behaviours in
  setDT(segment)
  setDT(dat)
  
  # Add row number within each timestamp group
  segment[, row_in_group := seq_len(.N), by = utc_datetime]
  dat[, row_in_group := seq_len(.N), by = utc_datetime]
  
  # Join on both time and position within group
  result <- dat[segment, on = c("utc_datetime", "row_in_group")]
  
  # save it
  fwrite(result, file.path(base_path, "Data", "LabelledData", "IncludingGyroscope", paste0(Collar, "_", basename(file))))
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
raw_data$mech_behaviour <- as.numeric(raw_data$mech_behaviour)
raw_data <- left_join(raw_data, mech_labels, by = "mech_behaviour") %>%
  rename(Number = "mech_behaviour")

# read in the behaviour conversion key
if(file.exists(file.path(base_path, "Notes", "BehaviourConversionKey.csv"))){
  key <- fread(file.path(base_path, "Notes", "BehaviourConversionKey.csv"))
  
  raw_data <- left_join(raw_data, key, by = "Activity") %>%
    select(-c(Why))
  
  raw_data$Activity <- raw_data$GroupedActivity
  raw_data[,GroupedActivity:=NULL]
}

raw_data <- raw_data %>%
  select(-c(Number, row_in_group, eco_behaviour)) %>%
  na.omit()


# check
unique(raw_data$Activity)
target_behaviours <- c("Foraging_Headup", 
                       "Foraging_Headdown", 
                       "Locomotion_Walk", 
                       "Locomotion_Fast",
                       "Stationary_Sleep", 
                       "Stationary_Standing", 
                       "Stationary_Vigilance", 
                       "Grooming",
                       "Other")

# use this script to play around with the labels and change them if need be 
rstudioapi::navigateToFile(file = file.path(base_path, "Scripts", "BehaviouralDetection", "GenerateTrainingData", "Explore_TrainingData.R"))

fwrite(raw_data, file.path(base_path, "Data", "LabelledData", "CleanedlLabelledData.csv"))

# if anything looks wrong, go back to the cleaning phase and repeat entire process