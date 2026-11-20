# This is really complicated because we annotated the data before we processed any of it
# so putting the annotations together doesn't actually return usable data
# so we have to put it together, clean it, and then FURTHER go and extract it from the new data
# this isn't the best way to do it, but this is how I did it at the
# what a nightmare

reannotation_necessary <- FALSE

# Load in the annotated matlab data ---------------------------------------
matlab_files <- list.files(file.path("Data", "LabelledData", "Matlab(NoGyro)"), recursive = TRUE, full.names = TRUE)
matlab_data <- lapply(matlab_files, function(file) {
  df <- fread(file)
  colnames(df) <- tolower(colnames(df))
  individual <- str_split(basename(dirname(file)), "_", simplify = TRUE)[2]
  df$id <- individual
  df <- df %>% select("time", "x", "y", "z", "id", "eco_behaviour", "mech_behaviour")
  colnames(df) <- c("Time", "X", "Y", "Z", "ID", "eco_behaviour", "mech_behaviour")
  df
})
matlab_data <- bind_rows(matlab_data)
# simplify some of the behavioural classes --------------------------------
key <- fread(file.path("Notes", "BehaviourConversionKey.csv"))
matlab_data <- left_join(matlab_data, key, by = "mech_behaviour") %>% select(-c(Why, OriginalActivity))
matlab_data <- matlab_data %>% select(-eco_behaviour, mech_behaviour) %>% na.omit()

# Get the processed data ---------------------------------------------------
# Normally making training data would be a simple matter of combining the matlab files into a 
# single csv however, due to learning and growing and trouble shooting
# I originally extracted the training data without the gyroscope 
# so I have to go back into the original data and get the right sections of data... but with gyro this time lol
# this is completely unique to my one specific scenario and would not normally be included.
matlab_sorted <- matlab_data %>%
  group_by(ID) %>%
  arrange(Time, .by_group = TRUE) %>%
  mutate(time_diff = Time - data.table::shift(Time), # had to define package or errored
         break_point = ifelse(time_diff > 2.33e-7*500, 1, 0), # break of more than a minute
         break_point = replace_na(break_point, 0),
         sequence = cumsum(break_point)) %>%
  mutate(event_id = paste0(ID, "_", sequence)) %>%
  select(-break_point, -time_diff, -sequence) %>%
  ungroup() %>%
  dplyr::filter(!mech_behaviour == 0) %>%
  select(-mech_behaviour )
matlab_chunks <- split(matlab_sorted, matlab_sorted$event_id)

# find the time and ID this came from, then go get it from the properly processed data
for (event in unique(matlab_sorted$event_id)){
  
  dat <- matlab_chunks[[event]] 
  Collar <- paste0("Collar_", dat$ID[1]) # get the ID
  dat <- dat %>%
    mutate(utc_datetime = as.POSIXct((Time - 719529)*86400, origin = "1970-01-01", tz = "UTC")) %>%
    select(utc_datetime, Activity)
  date <- as.Date(dat$utc_datetime[1]) # get the date
  
  # extract that date from that impala
  target_files <- list.files(file.path("Data", "RawData", Collar, "Chunked"), full.names = TRUE)
  matched <- target_files[grep(date, target_files)]
  if (length(matched) == 0) next
  accel_data <- fread(matched)
  
  segment <- accel_data[accel_data$utc_datetime > dat$utc_datetime[1] & 
                          accel_data$utc_datetime <  dat$utc_datetime[nrow(dat)], ]
  segment[, c("lon", "lat", "date") := NULL]
  
  # add the behaviours in
  setDT(segment)
  setDT(dat)
  setkey(segment, utc_datetime)
  setkey(dat, utc_datetime)
  
  # join dat onto segment, rolling forward to fill behaviour labels
  result <- dat[segment, on = "utc_datetime", roll = TRUE]
  
  # save it
  fwrite(result, file.path("Data", "LabelledData", "IncludingGyroscope", paste0(Collar, "_", event, ".csv")))
}

# Now stitch the data together ------------------------------------------
files <- list.files(file.path("Data", "LabelledData", "IncludingGyroscope"), recursive = TRUE, full.names = TRUE)
raw_data <- lapply(files, function(file) {
  df <- fread(file)
  individual <- paste0("Collar_", str_split(basename(file), "_", simplify = TRUE)[2])
  df$ID <- individual
  df[, (3:5) := lapply(.SD, as.character), .SDcols = 3:5]
})
raw_data <- rbindlist(raw_data, fill= TRUE)

fwrite(raw_data, file.path("Data", "LabelledData", "OriginalLabelledData.csv"))


# Make a cleaned version as well ------------------------------------------
# this is an aggressive step I originally did but later questioned myself on
# am including here for posterity's sake
if (reannotation_necessary){
  save_split_dir <- file.path("Data", "LabelledData", "Split")
  if(!dir.exists(save_split_dir)){dir.create(save_split_dir)}
  data_list <- split(matlab_data, matlab_data$Activity)
  lapply(names(data_list), function(nm) {
    out <- data_list[[nm]] %>% select(-Activity)
    fwrite(out, file.path(save_split_dir, paste0(nm, ".csv")))
  })
}
# now use the matlab to reannotate them..
# then load them back together
if (reannotation_necessary){
  cleaned_files <- list.files("Data/LabelledData/Split", pattern = "_tagged.csv", full.names = TRUE)
  cleaned <- rbindlist(lapply(cleaned_files, fread))
  cleaned <- left_join(cleaned, key, by = "mech_behaviour")
  cleaned <- cleaned[, c("Time", "ID")]
  
  # and select the right data for these
  raw_data[, utc_datetime := as.POSIXct(round(as.numeric(utc_datetime)), origin = "1970-01-01", tz = "UTC")]
  raw_data$ID <- gsub("Collar_", "", raw_data$ID)
  
  setDT(cleaned); setDT(raw_data)
  raw_data[, ID := trimws(as.character(ID))]
  cleaned[,  ID := trimws(as.character(ID))]
  
  cleaned[, Time := as.POSIXct((Time - 719529) * 86400, origin = "1970-01-01", tz = "UTC")]
  raw_data[, Time := as.POSIXct(as.numeric(utc_datetime), origin = "1970-01-01", tz = "UTC")]
  
  # because the raw data is measured only to the whole second rather than fractionally, I can't
  # join these two data sources together
  # therefore, I need to artifically add this fractionalisation UUGHGHGH
  hz <- 50
  setorder(raw_data, ID, utc_datetime)   # make sure order is the original recording order
  # sub-second time from sample index within each (ID, second)
  raw_data[, Tfull := utc_datetime + (seq_len(.N) - 1) / .N, by = .(ID, utc_datetime)]
  
  # and now I can join them together with a tolerance
  raw_data[, tr := as.numeric(Tfull)]
  cleaned[,  tc := as.numeric(Time)]
  setkey(raw_data, ID, tr)
  
  out <- raw_data[cleaned, on = .(ID, tr = tc), roll = "nearest"]
  # out[, dt := abs(tr - tc)]
  
  out <- out[, 1:39]
  
  fwrite(out, file.path("Data", "LabelledData", "CleanedLabelledData.csv"))
}


# Relabel even more -------------------------------------------------------
# we can logically define what we mean by each of the behaviours and hardcode that
out <- out %>% rename(X = RawAX.cl, Y  = RawAY.cl, Z = RawAZ.cl)
unique(out$Activity)

plotTraceExamples(behaviours = unique(out$Activity),
                  out, 
                  individuals = length(unique(out$ID)), 
                  n_samples = 5000, n_col = 1)


out <- out %>%
  mutate(Activity = ifelse(Activity %in% c("Stationary_Vigilance", "Stationary_Standing", "Stationary_Sleep"), "Stationary", Activity))

# just the columns I want
out <- out[, c("Time", "ID", "Activity", "X", "Y", "Z")]

# and save it again
fwrite(out, file.path("Data", "LabelledData", "PooledCleanedLabelledData.csv"))

