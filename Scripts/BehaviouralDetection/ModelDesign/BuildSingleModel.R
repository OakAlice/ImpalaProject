#################
# HyperparameterOptimisation

# Overview:
# Create an accelerometer-based animal behaviour classification machine learning model
# Optimises hyperparmaeters

# Requires:
# Labelled feature data

#################

# Split out test data -----------------------------------------------------
test_data <- data %>% dplyr::filter(ID %in% test_IDs)
other_data <- data %>% dplyr::filter(!ID %in% test_IDs)  

# remove bad features
clean_cols <- removeBadFeatures(other_data, var_threshold = 0.3, corr_threshold = 0.9)
clean_feature_data <- other_data %>%
  select(c(!!!syms(clean_cols), "Activity", "ID", "Time")) %>% 
  na.omit()

if (i == 1){
  # Hyperparameter tuning ---------------------------------------------------
  # Bayesian optimisation, optimising for macro F1 score where no predict = 0
  if (model_choice == "RandomForest"){
    bounds <- list(
      mtry = c(2, 20),
      max_depth = c(5, 30),
      number_trees = c(300, 1000)
    )
    
    results <- BayesianOptimization(
      FUN = function(number_trees, mtry, max_depth) {
        RFModelOptimisation(
          feature_data = clean_feature_data,
          number_trees = number_trees,
          mtry = mtry,
          max_depth = max_depth
        )
      },
      bounds = bounds,
      init_points = 5,
      n_iter = 20,
      acq = "ucb",
      kappa = 2.576 
    )
    
    
  } else if (model_choice == "XGBoost"){
    bounds <- list(
      eta       = c(0.01, 0.3),
      max_depth = c(3L, 10L),
      nrounds   = c(100L, 1000L)
    )
    
    results <- BayesianOptimization(
      FUN = function(eta, nrounds, max_depth) {
        XGBoostModelOptimisation(
          feature_data = clean_feature_data,
          eta = eta,
          nrounds = nrounds,
          max_depth = max_depth
        )
      },
      bounds = bounds,
      init_points = 2,
      n_iter = 5,
      acq = "ucb",
      kappa = 2.576 
    )
    
  } else if (model_choice == "NN"){
    bounds <- list(
      size  = c(8L, 64L),
      decay = c(0.0001, 0.1)
    )
    
    results <- BayesianOptimization(
      FUN = function(size, decay) {
        NNModelOptimisation(
          feature_data = clean_feature_data,
          size = size,
          decay = decay
        )
      },
      bounds = bounds,
      init_points = 2,
      n_iter = 5,
      acq = "ucb",
      kappa = 2.576 
    )
  }
  
  history <- results$History
  fwrite(history, file.path(base_path, "Output", "ClassificationModel", paste0(model_choice, "_hpo_", i, ".csv")))
}


# Build a single model ----------------------------------------------------
## NOTE all of this end stuff is only for the random forest model_choice so far...
if (model_choice == "RandomForest"){
  print("if randfom forest, now making the final model")
  
  # extract the best ones
  best_mtry <- round(results$Best_Par[["mtry"]],0)
  best_number_trees <- round(results$Best_Par[["number_trees"]],0)
  best_max_depth <- round(results$Best_Par[["max_depth"]],0)
  best_performance <- results$Best_Value # just for interest
  
  # Train an optimal model --------------------------------------------------
  other_feature_data <- as.data.table(other_data)
  clean_cols <- removeBadFeatures(other_feature_data, var_threshold = 0.3, corr_threshold = 0.9)
  training_data <- other_feature_data %>%
    select(c(!!!syms(clean_cols), "Activity")) %>%
    na.omit() %>%
    mutate(Activity = as.factor(Activity))
  
  # weight by class frequency
  class_freq <- table(training_data$Activity)
  class_weights <- 1 / class_freq
  class_weights <- class_weights / sum(class_weights)
  weight <- class_weights[training_data$Activity]
  
  RF_model <- ranger(
    dependent.variable.name = "Activity",
    data = training_data,
    num.trees = best_number_trees,
    mtry = best_mtry,
    max.depth = best_max_depth,
    classification = TRUE,
    probability = TRUE,
    importance = "impurity",
    case.weights = weight
  )
  
  # save this mode
  saveRDS(RF_model, file.path(base_path, "Output", "ClassificationModel", paste0(model_choice, "_model_", i, ".rds")))
  
  # Make predictions --------------------------------------------------------
  test_feature_data <- as.data.table(test_data)
  complete_cases <- test_feature_data %>%
    select(all_of(c(clean_cols, "Activity", "ID", "Time"))) %>%
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
  
  # make a confusion matrix
  performance <- calculate_performance(predictions_df$predicted_class, predictions_df$true_class)
  
  # Write to CSV
  write.csv(performance$confusion_mtx$byClass, file = file.path(base_path, "Output", "ClassificationModel", paste0(model_choice, "_performance_metrics_", i, ".csv")), row.names = TRUE)
  # write.csv(predictions_df, file = file.path(base_path, "Output", "ClassificationModel", paste0(model_choice, "_test_predictions_", i, ".csv")), row.names = FALSE)
  write.csv(performance$confusion_mtx$table, file = file.path(base_path, "Output", "ClassificationModel", paste0(model_choice, "_conf_matrix_", i, ".csv")), row.names = TRUE)
  
  # Make predictions back on the training data ------------------------------
  # due to limitations of data availability for most of the datasets
  # to get more model trainign data, I have to predict back onto the training data
  training_feature_data <- as.data.table(other_data)
  complete_cases <- training_feature_data %>%
    select(all_of(c(clean_cols, "Activity", "ID", "Time"))) %>%
    na.omit()
  numeric_training_data <- complete_cases %>%
    select(all_of(clean_cols)) %>%
    as.matrix()
  
  training_metadata <- complete_cases %>%
    select(Activity, ID, Time)
  ground_truth_labels <- factor(training_metadata$Activity)
  
  # Make predictions
  output <- predict(RF_model, data = numeric_training_data, probability = TRUE)
  predictions <- output$predictions
  predicted_class <- colnames(predictions)[max.col(predictions, ties.method = "first")]
  predictions_df <- cbind(training_metadata, predictions, predicted_class)
  predictions_df <- predictions_df %>% rename(true_class = Activity)
  
  # Write to CSV
  write.csv(predictions_df, file = file.path(base_path, "Output", "ClassificationModel", paste0(model_choice, "_training_predictions_", i, ".csv")), row.names = FALSE)
  
} else {
  print("I haven't written the code for these other methods yet, but will could be a variant of the RF method above")
}
