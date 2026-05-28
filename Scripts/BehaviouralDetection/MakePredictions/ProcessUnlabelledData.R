# Process the unlabelled data ---------------------------------------------
# load in the unlabelled data and generate the features
source(file = file.path(base_path, "Scripts", "BehaviouralDetection", "GenerateTrainingData", "Functions_GenerateFeatures.R"))
unlabelled_files <- list.files(file.path(base_path, "Data", "RawData", Collar, "Chunked"), full.names = TRUE, pattern = ".csv")

# create the save repo
save_dir <- file.path(base_path, "Data", "RawData", Collar, "BoardFeatures")
if(!dir.exists(save_dir)){dir.create(save_dir)}

# was doing this as an lapply but it wasn't saving the way I wanted # or rather, I wasn't sure
# so have defaulted to an easier and safer for loop
for (i in seq_along(unlabelled_files)) {
  x <- unlabelled_files[i]
  
  filname <- tools::file_path_sans_ext(basename(x))
  filname_date <- as.POSIXct(gsub("Board_Aligned_", "", filname), format = "%Y-%m-%d")
  
  # check whether the data is valid
  start_date <- sampling_start$StartDate[sampling_start$CollarNumber == str_split(Collar, "_", simplify = TRUE)[2]]
  if (filname_date < start_date) {
    print("this file is from before the device was deployed... skipping...")
    next
  }
  
  if (!file.exists(file.path(save_dir, paste0(filname, "_features.csv")))){
    accel_data <- fread(x, select = c("utc_datetime", "RawAX.cl", "RawAY.cl", "RawAZ.cl")) # only select the important columns
    colnames(accel_data) <- c("Time", "X", "Y", "Z") # rename them nicely
    available_axes <- c("X", "Y", "Z") # make sure these match
    accel_data$ID <- Collar
    
    # because the files are so massive, split them into segments by each hour
    accel_data$Hour <- hour(accel_data$Time)
    accel_list <- split(accel_data, by = "Hour", keep.by = TRUE)
    
    # now proccesss each of them
    lapply(names(accel_list), function(d) {
      accel_data <- accel_list[[d]] %>% select(-Hour)
      
      features <- processDataPerID(
        id_raw_data    = accel_data, 
        features_type  = c("timeseries", "statistical"), 
        window_length  = desired_window,  # this is in seconds
        sample_rate    = sample_rate, 
        overlap_percent = desired_overlap
      )
      
      # save each chunk
      fwrite(features, file.path(save_dir, paste0(filname, "_", d, "_features.csv")))
    })
  
  } else {
    print("features already calculated")
    # feature_data <- fread(file.path(base_path, "Output", collar, paste0(filname, "_features.csv")))
  }
}

  # Apply the predictions
  # Start with unique Time + ID
#   tags <- feature_data %>% select(Time, ID) %>% distinct()
#   
#   all_preds <- list()  # store predictions per activity
#   for (activity in target_activities) {
#     # Load model
#     SVM_model <- readRDS(file.path(base_path, "ModelBuilding", paste0(activity, "_SVM.RDS")))
#     
#     # Features this model expects
#     good_features <- names(SVM_model$x.scale$`scaled:center`)
#     
#     # Subset features
#     num_unlabelled <- feature_data %>% 
#       select(Time, all_of(good_features)) %>% 
#       drop_na()  # only rows with complete data for this model
#     
#     # Predict
#     predictions <- predict(SVM_model, newdata = num_unlabelled %>% select(-Time))
#     
#     # Store as dataframe with Time + predictions
#     all_preds[[activity]] <- tibble(
#       Time = num_unlabelled$Time,
#       !!activity := predictions
#     )
#   }
#   
#   # Merge all predictions back into tags by Time
#   for (activity in names(all_preds)) {
#     tags <- tags %>% left_join(all_preds[[activity]], by = "Time")
#   }
#   
#   # read in the performance of the models and rank by best F1 performance
#   performance <- fread(file.path(base_path, "ModelBuilding", "Average_CrossValidated_Performance.csv")) %>%
#     arrange(desc(F1)) %>%
#     pull(activity)
#   
#   # whenever there is a conflict, choose the highest ranked behaviour
#   tags$Activity <- apply(tags[, names(all_preds), with = FALSE], 1, function(row_preds) {
#     # find all activities predicted for this row (non-Other)
#     non_other <- names(row_preds)[row_preds != "Other"]
#     
#     if (length(non_other) == 0) {
#       return("Other")  # no activity detected
#     } else if (length(non_other) == 1) {
#       return(non_other)  # only one activity → keep it
#     } else {
#       # conflict: choose the highest-ranked one from performance
#       best <- performance[performance %in% non_other][1]
#       return(best)
#     }
#   })
#   
#   tags <- tags %>% select(Time, ID, Activity)
#     
#   # Save
#   fwrite(tags, file.path(base_path, "Output", collar, paste0(filname, "_Unlabelled_Predictions.csv")))
# }


# Normalising the features ------------------------------------------------
# realised I'd forgotten this step way into the process
# have to go back and do it now
# unlabelled_files <- list.files(file.path(base_path, "Output", collar), full.names = TRUE, pattern = "_features.csv")
# normalised_data <- lapply(unlabelled_files, function(x){
#   features <- fread(x)
#   filname <- tools::file_path_sans_ext(basename(x))
#   features_to_normalise <- colnames(features)[!colnames(features) %in% c("Activity", "ID", "Time")]
#   features[, (features_to_normalise) := lapply(.SD, function(x) {
#     s <- sd(x, na.rm = TRUE)
#     if (s == 0 || is.na(s)) return(rep(0, .N))
#     (x - mean(x, na.rm = TRUE)) / s
#   }), .SDcols = features_to_normalise]
#   fwrite(feature_data, file.path(base_path, "Output", collar, paste0(filname, "_features.csv")))
# })
