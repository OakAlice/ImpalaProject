################
# CleanLabelledData

# Overview:
# Iterate between these steps above to clean more and more
# Load in the annotations, rename them, split them out, clean them, recombine them, rename them, etc.
# This should take a while and possible multiple repetitions. 
# This is an important step and will save a lot of mucking around with the model later

# code is really long and messy because this was an iterative process for me 
# with new steps added as needed... and the tech debt has not yet been cleaned up

# Requires:
# Matlab txt files with the annotations

#################


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
# matlab_data <- fread(file.path(base_path, "Data", "LabelledData", "OriginalLabelledData.csv"))

# Regroup the behaviours --------------------------------------------------
# convert to the groups
## NOTE: This has to be decided by guess and check on discretion of the researcher
## This may take multiple stages of iteration... could possibly group multiple ways to check later on
key <- fread(file.path(base_path, "Notes", "BehaviourConversionKey.csv"))
matlab_data <- left_join(matlab_data, key, by = "mech_behaviour") %>% select(-c(Why, OriginalActivity))

# split by Activity and save back into their own files ---------------------
save_split_dir <- file.path(base_path, "Data", "LabelledData", "Split")
if(!dir.exists(save_split_dir)){dir.create(save_split_dir)}
data_list <- split(matlab_data, matlab_data$Activity)
lapply(names(data_list), function(nm) {
  out <- data_list[[nm]] %>% select(-Activity)
  fwrite(out, file.path(save_split_dir, paste0(nm, ".csv")))
})

# Now go back into the matlab and clean these up so they have the right labels given the new groupings.
# note that this converts things back to numeric which has to be corrected again... but this time it will be simplified at least

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
# update the labels again back to words again
matlab_data <- left_join(matlab_data, key, by = "mech_behaviour") %>% select(-c(Why, OriginalActivity))


# Get the gyroscope data ---------------------------------------------------
# Normally making training data would be a simple matter of combining the matlab files into a 
# single csv however, due to learning and growing and trouble shooting
# I originally extracted the training data without the gyroscope 
# so I have to go back into the original data and get the right sections of data... but with gyro this time lol
# this is completely unique to my one specific scenario and would not normally be included.
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
    select(utc_datetime, Activity)
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
  setkey(segment, utc_datetime)
  setkey(dat, utc_datetime)
  
  # join dat onto segment, rolling forward to fill behaviour labels
  result <- dat[segment, on = "utc_datetime", roll = TRUE]
  
  # save it
  fwrite(result, file.path(base_path, "Data", "LabelledData", "IncludingGyroscope", paste0(Collar, "_", event, ".csv")))
}

# When that is complete, I now have all of the labelled training data, with all columns, correctly scaled
# I don't want to admit how long it took me to get to this stage but 2 months would be conservative
# My goodness what a nightmare.

# Now stitch the data together ------------------------------------------
files <- list.files(file.path(base_path, "Data", "LabelledData", "IncludingGyroscope"), recursive = TRUE, full.names = TRUE)
raw_data <- lapply(files, function(file) {
  df <- fread(file) %>% select(-HeadAcc)
  individual <- paste0("Collar_", str_split(basename(file), "_", simplify = TRUE)[2])
  df$ID <- individual
  df[, (3:5) := lapply(.SD, as.character), .SDcols = 3:5]
})
raw_data <- bind_rows(raw_data)





# WARNING -----------------------------------------------------------------
# BELOW THIS LINE IS MY PLAYING AROUND AND CHANGING
# HIGHLY MANUAL
# I wrote a lot of this code out of order, changing things and renaming them
# therefore, while I was figuring it out, needed to iterate and update frequently
# TODO: Simplify after I've finished development phase


# Reprocessing the data to the new way ------------------------------------
source(file = file.path(base_path, "Scripts",  "DeadReckoning", "Functions_DR.R"))
source(file = file.path(base_path, "Scripts",  "WranglingData", "DataReadFunctions.R"))

raw_data <- fread(file.path(base_path, "Data", "LabelledData", "CleanedlLabelledData.csv"))

raw_data <- clean_noise(raw_data, med_k = 5)
raw_data <- activity_scoring(data = raw_data, threshold = 0.05, smooth_width = 100)

# figuring out the threshold
headup <- raw_data %>% dplyr::filter(Activity == "Stationary_Vigilance")
ggplot(headup, aes(x = seq(1:nrow(headup)))) +
  geom_path(aes(y = RawAX.cl, colour = VDBA.sd)) +
  geom_path(aes(y = RawAY.cl, colour = VDBA.sd)) +
  geom_path(aes(y = RawAZ.cl, colour = VDBA.sd)) +
  geom_path(aes(y = ME, colour = ME)) +
  facet_wrap(~ID, scales = "free_x")

raw_data <- raw_data %>%
  mutate(Activity2 = case_when(
    # Stationary: head up = vigilance, head down = other
    ME == 0 & RawAY.sm > (RawAZ.sm - 0.5) ~ "Stationary_Vigilance",
    ME == 0                               ~ "Stationary_Other",
    # Moving: head down = foraging
    ME == 1 & RawAY.sm < RawAX.sm         ~ "Foraging_Headdown",
    # Moving: head up = foraging
    ME == 1 & RawAY.sm > (RawAZ.sm - 0.25) ~ "Foraging_Headup",
    # Locomotion from original labels
    Activity == "Locomotion_Walk"         ~ "Locomotion_Walk",
    Activity == "Locomotion_Fast"         ~ "Locomotion_Fast",
    Activity == "Grooming"                ~ "Grooming",
    # Otherwise retain the original labels
    TRUE                                  ~ "Other" 
  ))

# change over the columns
raw_data <- raw_data %>%
  select(-Activity) %>%
  rename(Activity = Activity2)


# Rename some of the columns and select just the ones for making features
raw_data <- raw_data %>%
  rename(RawAX.cl = RawAX.butt,
         RawAY.cl = RawAY.butt,
         RawAZ.cl = RawAZ.butt) %>%
  select(utc_datetime, ID, Activity, RawAX.cl, RawAY.cl, RawAZ.cl)

fwrite(raw_data, file.path(base_path, "Data", "LabelledData", "CleanedlLabelledData.csv"))


# checking
# plotdat <- raw_data %>%
#   slice(300000:320000) %>%
#   select(Activity, Activity2, RawAX.butt, RawAY.butt, RawAZ.butt) %>%
#   mutate(idx = row_number()) %>%
#   pivot_longer(cols = c(RawAX.butt, RawAY.butt, RawAZ.butt),
#                names_to = "axis", values_to = "value") %>%
#   pivot_longer(cols = c(Activity, Activity2),
#                names_to = "label_type", values_to = "label")
# 
# ggplot(plotdat, aes(x = idx, y = value, colour = label, group = axis)) +
#   geom_path(alpha = 0.7) +
#   facet_wrap(~label_type, ncol = 1) +
#   theme_minimal()
