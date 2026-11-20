
# Function to make the predictions ----------------------------------------
generate_predictions <- function(model, model_type = 'RF', data, metadata_cols){
  
  dt <- copy(data)
  
  clean_cols <- model$forest$independent.variable.names
  
  complete_cases <- dt %>%
    dplyr::select(all_of(c(clean_cols, metadata_cols))) %>%
    tidyr::drop_na(all_of(clean_cols))
  features <- complete_cases %>% dplyr::select(all_of(clean_cols)) %>% as.matrix()
  metadata <- complete_cases %>% dplyr::select(all_of(metadata_cols))
  
  # Make predictions
  output <- predict(model, data = features)
  predictions <- output$predictions
  predicted_class <- colnames(predictions)[max.col(predictions, ties.method = "first")]
  predictions_df <- cbind(metadata, predictions, predicted_class)
  
  return(predictions_df)
}

make_predictions_wrapper <- function(f, final_model, sampling_dates, sanity_check = TRUE){
  
  # define the folders
  input_subfolder <- file.path("Data", "RawData", f, "Chunked")
  feature_subfolder <- file.path("Data", "RawData", f, "Features")
  output_subfolder <- file.path("Output", "BehaviouralPredictions", f)
  
  # for each of the days that occur within the sampling window
  chunks <- list.files(input_subfolder, full.names = TRUE)
  samps <- sampling_dates[CollarNumber == f]
  
  chunk_dates <- as.Date(sub(".*Board_Aligned_(.*)\\.csv$", "\\1", chunks))   # "NA" -> NA
  chunks <- chunks[!is.na(chunk_dates) &
                     chunk_dates >  samps$Start &
                     chunk_dates <= samps$End]
  
  lapply(chunks, function(x){
    
    # generate and save the features
    unlabelled_data <- fread(x) %>% mutate(ID = f) %>%
      rename(X = RawAX.cl, Y  = RawAY.cl, Z = RawAZ.cl, Time = utc_datetime) %>%
      select(c(ID, Time, X, Y, Z, lon, lat))
    unlabelled_features <- generate_features(unlabelled_data, sample_rate, metadata_cols,
                                             overlap_percent, window_length)
    
    # save the features 
    if(!dir.exists(feature_subfolder)){dir.create(feature_subfolder, recursive = TRUE)}
    name <- basename(tools::file_path_sans_ext(x))
    fwrite(unlabelled_features, file.path(feature_subfolder, paste0(name, "_features.csv")))
    
    # generate the predictions
    final_predictions <- generate_predictions(final_model, model_type = 'RF', 
                                              unlabelled_features, metadata_cols)
    
    # make a postprocessor while we're here
    smoothed_predictions <- postprocessing(labelled_data = features, 
                                           original_predictions = final_predictions, 
                                           method = "Bayesian",
                                           sample_rate)
    
    # extract the mean VDBA for each window and append that to the predictions...
    VDBA <- unlabelled_features$meanVDBA
    smoothed_predictions$VDBA <- VDBA
    
    if(!dir.exists(output_subfolder)){dir.create(output_subfolder, recursive = TRUE)}
    fwrite(smoothed_predictions, file.path(output_subfolder, paste0(name, "_predictions.csv")))
    
    if (sanity_check){
      # SANITY CHECKING 
      # pull out sections of the raw trace and confirm visually whether they 
      # have been predicted as the right class
      sanity_plots <- sanity_check_plots(unlabelled_data, smoothed_predictions)
      ggsave(file.path("Output/BehaviouralPredictions", f, paste0(name, "_all_behs.png")), plot = sanity_plots$all_behs)
      ggsave(file.path("Output/BehaviouralPredictions", f, paste0(name, "_random_grid.png")), plot = sanity_plots$random_grid)
    }
  })
}
  