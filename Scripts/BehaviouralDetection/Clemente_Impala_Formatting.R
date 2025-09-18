# Clemente_Imapala ------------------------------------------------------------

## Basic formatting -------------------------------------------------------
# this will be different for every dataset

if(!file.exists(file.path(base_path, "Data", species, "Formatted_raw_data.csv"))){
  
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
  
  # save this 
  fwrite(raw_data, file.path(base_path, "ModelBuilding", "Formatted_raw_data.csv"))
  
} else {
  print("training data already formatted")
}
  
## Generate features ------------------------------------------------------
if (file.exists(file.path(base_path, "Data", species, "Feature_data.csv"))){
  print("training features already generated")
} else {
    
  data1 <- fread(file.path(base_path, "Data", species, "Formatted_raw_data.csv"))
    
  generated_features <- list()
  for (id in unique(data1$ID)){
    data <- data1 %>% 
      filter(ID == id) %>% 
      filter(!Activity == "") %>% 
      as.data.table()
      
    feature_data <- processDataPerID(data, 
                                     features_type = c("timeseries", "statistical"), 
                                     window_length = desired_window, # this is in seconds, 
                                     sample_rate = sample_rate, 
                                     overlap_percent = desired_overlap)
      
    generated_features[[id]] <- feature_data
    fwrite(feature_data, file.path(base_path, "Data", species, paste0(id, "_feature_data.csv")))
  }
  generated_features_df <- bind_rows(generated_features)
  fwrite(generated_features_df, file.path(base_path, "Data", species, "Feature_data.csv"))
}
  