# Generate features from the raw data -------------------------------------
raw_data <- fread(file.path(base_path, "Data", "LabelledData", "CleanedlLabelledData.csv")) %>%
  rename(Time = utc_datetime)
available_axes <- c("RawAX", "RawAY", 'RawAZ', "RawMX", "RawMY", 'RawMZ') # the name of the axes
generated_features <- list()
for (id in unique(raw_data$ID)){
  data <- raw_data %>% 
    dplyr::filter(ID == id) %>% 
    as.data.table()
  
  feature_data <- processDataPerID(id_raw_data = data, 
                                   features_type = c("timeseries", "statistical"), 
                                   window_length = desired_window, # this is in seconds, 
                                   sample_rate = sample_rate, 
                                   overlap_percent = desired_overlap)
  
  generated_features[[id]] <- feature_data
}
features <- bind_rows(generated_features)
as.data.table(features)

features_to_normalise <- colnames(features)[!colnames(features) %in% c("Activity", "ID", "Time")]
features[, (features_to_normalise) := lapply(.SD, function(x) {
  s <- sd(x, na.rm = TRUE)
  if (s == 0 || is.na(s)) return(rep(0, .N))
  (x - mean(x, na.rm = TRUE)) / s
}), .SDcols = features_to_normalise]

# save this
fwrite(features, file.path(base_path, "Data", "LabelledData", paste0("FeatureLabelledData.csv")))


