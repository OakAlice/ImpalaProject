# Generate features from the raw data -------------------------------------
raw_data <- fread(file.path(base_path, "Data", "LabelledData", "OriginalLabelledData.csv"))
available_axes <- c("X", "Y", 'Z') # the name of the accelerometer axes
generated_features <- list()
for (id in unique(raw_data$ID)){
  data <- raw_data %>% 
    filter(ID == id) %>% 
    as.data.table()
  
  feature_data <- processDataPerID(data, 
                                   features_type = c("timeseries", "statistical"), 
                                   window_length = desired_window, # this is in seconds, 
                                   sample_rate = sample_rate, 
                                   overlap_percent = desired_overlap)
  
  generated_features[[id]] <- feature_data
}
features <- bind_rows(generated_features)
setDT(features)

features_to_normalise <- colnames(features)[!colnames(features) %in% c("MechActivity", "EcoActivity", "ID", "Time")]
features[, (features_to_normalise) := lapply(.SD, function(x) {
  s <- sd(x, na.rm = TRUE)
  if (s == 0 || is.na(s)) return(rep(0, .N))
  (x - mean(x, na.rm = TRUE)) / s
}), .SDcols = features_to_normalise]

# remove the features with no variance and high correlation
clean_cols <- removeBadFeatures(features, var_threshold = 0.3, corr_threshold = 0.8)
clean_feature_data <- features %>%
  select(c(!!!syms(clean_cols), "MechActivity", "EcoActivity")) %>% 
  na.omit()

fwrite(features, file.path(base_path, "Data", "LabelledData", paste0("FeatureLabelledData.csv")))
