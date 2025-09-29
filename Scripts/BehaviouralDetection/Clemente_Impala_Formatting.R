# Clemente_Imapala ------------------------------------------------------------

## Basic formatting -------------------------------------------------------
  # i just stitch the data together
  files <- list.files(file.path(base_path, "LabelledData"), recursive = TRUE, full.names = TRUE)
  raw_data <- lapply(files, function(file) {
    df <- fread(file)
    filename <- tools::file_path_sans_ext(basename(file))
    num <- sub("^([0-9]{1,2}).*", "\\1", filename)
    df$ID <- num
    
    df <- df %>%
      select(-func_behaviour)
    
    return(df)
  }) 
  raw_data <- bind_rows(raw_data)
  
  # and thenI select the columns I want and give them the names that match my workflow
  raw_data <- raw_data %>%
    rename(X = x,
           Y = y,
           Z = z,
           Time = time
           )

# Cleaning up this data ---------------------------------------------------
raw_data <- raw_data %>%
  filter(!mech_behaviour == 81)

# stitch the behaviours to their names:
mech_labels <- fread("C:/Users/PC/Documents/ImpalaProject/RawData/Functional Behaviours.csv") %>%
  rename(mech_behaviour = Num,
         mech_activity = Activity)
eco_labels <- fread("C:/Users/PC/Documents/ImpalaProject/RawData/Ecological Behaviours.csv") %>%
  rename(eco_behaviour = Num,
         eco_activity = Actifity)
raw_data <- left_join(raw_data, mech_labels, by = "mech_behaviour")
raw_data <- left_join(raw_data, eco_labels, by = "eco_behaviour")

# clean it up
raw_data <- raw_data[!is.na(raw_data$mech_activity), ]

# remove the categories we've decided not to use 
keep_categories <- c("Walking", "Sprinting/Bounding", "Trotting", "Scratching", "sleeping",
                     "Head Down Grazing", "Grazing and Walking", "Grazing Head Up (Browsing)"
                     )

raw_data$mech_activity <- ifelse(raw_data$mech_activity %in% keep_categories, raw_data$mech_activity, "Other")
raw_data$mech_activity <- ifelse(raw_data$mech_activity %in% c("Head Down Grazing", "Grazing and Walking", "Grazing Head Up (Browsing)"), "Grazing", raw_data$mech_activity)
raw_data$mech_activity <- ifelse(raw_data$mech_activity == "Sprinting/Bounding", "Sprinting_Bounding", raw_data$mech_activity)

# downsampling to only retain the useful data
sub_raw_data <- raw_data %>%
  group_by(ID, mech_activity) %>%
  slice(1:15000)

# count_data <- sub_raw_data %>%
#   group_by(ID, mech_activity) %>%
#   count()
# ggplot(count_data, aes(x = mech_activity, y = n, fill = ID)) +
#   geom_col(position = "dodge") +
#   theme_minimal()

# how much data is this really?
# minutes <- sub_raw_data %>%
#   group_by(mech_activity) %>%
#   count() %>%
#   summarise(minutes = (n/50)/60)

# format and organise
sub_raw_data <- sub_raw_data %>%
  select(Time, X, Y, Z, ID, mech_activity) %>%
  rename(Activity = mech_activity)

# save this 
fwrite(sub_raw_data, file.path(base_path, "ModelBuilding", "Formatted_raw_data.csv"))
  
## Generate features ------------------------------------------------------
if (file.exists(file.path(base_path, "Data", species, "Feature_data.csv"))){
  print("training features already generated")
} else {
    
  data1 <- fread(file.path(base_path, "ModelBuilding", "Formatted_raw_data.csv"))
    
  generated_features <- list()
  for (id in unique(data1$ID)){
    data <- data1 %>% 
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
  
  features_to_normalise <- colnames(features)[!colnames(features) %in% c("Activity", "ID", "Time")]
  features[, (features_to_normalise) := lapply(.SD, function(x) {
    s <- sd(x, na.rm = TRUE)
    if (s == 0 || is.na(s)) return(rep(0, .N))
    (x - mean(x, na.rm = TRUE)) / s
  }), .SDcols = features_to_normalise]
    
  fwrite(features, file.path(base_path, "ModelBuilding", paste0("Feature_data.csv")))
}
