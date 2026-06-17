#################
# Main_Unlabelled

# Overview:
# Generate features for the unlabelled deployment data, then make predictions for each window
# Outputs the predictions per second in 1 day chunks

# Requires:
# Raw unlabelled data
# Finalised behavioural classification model

#################

# Set up ------------------------------------------------------------------
source(file = file.path(base_path, "Scripts", "BehaviouralDetection", "GenerateTrainingData", "Functions_GenerateFeatures.R"))

# create the save repo for the features
save_dir <- file.path(base_path, "Data", "RawData", Collar, "BoardFeatures")
if(!dir.exists(save_dir)){dir.create(save_dir)}
# and for the predictions
output_dir <- file.path(base_path, "Output", "BehaviouralPredictions", Collar) 
if(!dir.exists(output_dir)){dir.create(output_dir)}

# Make the features -------------------------------------------------------
# was doing this as an lapply but it wasn't saving the way I wanted # or rather, I wasn't sure
# so have defaulted to an easier and safer for loop
unlabelled_files <- list.files(file.path(base_path, "Data", "RawData", Collar, "Chunked"), full.names = TRUE, pattern = ".csv")
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
  
  # check whether these have already been generated
  already_made_files <- list.files(save_dir)
  if (length(grep(filname, already_made_files))<24){
    
    # if not, then make the features
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
    
    ## TODO: Add some logic that checks whether its the last day and doewsn't re-generate if is
  
  } else {
    print("features already calculated")
  }
}

# Apply the predictions ----------------------------------------------------
# process for each unique day
dates <- unique(str_split(list.files(save_dir), "_", simplify = T)[,3])

for (date in dates){
  chunks <- grep(date, list.files(save_dir, full.names = TRUE), value = TRUE)
  
  day_data <- list()
  for (chunk in chunks){
    # load in the data and the model
    dat <- fread(chunk)
    RF_model <- readRDS(file.path(base_path, "Output", "ClassificationModel", "RandomForest_final_model.rds"))
    
    # prepare the data
    clean_cols <- RF_model$forest$independent.variable.names # extract the features that were used
    
    dat <- as.data.table(dat)
    complete_cases <- dat %>%
      select(all_of(c(clean_cols, "ID", "Time"))) %>%
      na.omit()
    
    numeric_testing_data <- complete_cases %>%
      select(all_of(clean_cols)) %>%
      as.matrix()
    if (anyNA(numeric_testing_data)) message("Validation data contains missing values!")
    
    testing_metadata <- complete_cases %>%
      select(Activity, ID, Time)
    ground_truth_labels <- factor(testing_metadata$Activity)
    
    # Make predictions
    output <- predict(RF_model, data = numeric_testing_data, probability = TRUE)
    predictions <- output$predictions
    predicted_class <- colnames(predictions)[max.col(predictions, ties.method = "first")]
    predictions_df <- cbind(testing_metadata, predictions, predicted_class)
    predictions_df <- predictions_df %>% rename(true_class = Activity)
    
    
    
    # apply the model
    
    
    # simplify the output
    
    
    # store
    day_data[chunk] <- dat
    
  }
  # save the day as a single file
  day_data <- rbindlist(day_data)
  fwrite(day_data, file.path(output_dir, paste0(date, "_predictions.csv")))
}
