# Master script for building the models for any given species etc ---------
other_data <- fread(file.path(base_path, "Data", species, "other_data.csv"))

other_data <- other_data %>% as.data.table() %>%
  group_by(ID, Activity) # %>%
  # slice(1:100) ## REMOVE THIS WHEN YOU're SERIOUES
  
# this is optimised for weighted F1 score
# note that you might have to go into function and change var_threshold to 0.01 
# if it's removing all features
results <- BayesianOptimization(
  FUN = function(number_trees, mtry, max_depth) {
    RFModelOptimisation(
      feature_data = other_data,
      data_split = split_data_method,
      number_trees = number_trees,
      mtry = mtry,
      max_depth = max_depth
    )
    },
    bounds = bounds,
    init_points = 2,
    n_iter = 5,
    acq = "ucb",
    kappa = 2.576 
)
  
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
  
# save this model
saveRDS(RF_model, file.path(base_path, "Output", species, "Activity_model.rds"))

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

metrics <- compute_metrics(predicted_classes = as.factor(predictions_df$predicted_class), 
                           ground_truth_labels = as.factor(predictions_df$true_class))

# Write to CSV
write.csv(metrics$conf_matrix_padded, file = file.path(base_path, "Output", species, paste0("Confusion_matrix.csv")), row.names = FALSE)
write.csv(metrics$metrics, file = file.path(base_path, "Output", species, paste0("Performance_metrics.csv")), row.names = FALSE)
write.csv(predictions_df, file = file.path(base_path, "Output", species, paste0("Predictions.csv")), row.names = FALSE)

# saves a little file of the parameters you used
params <- as.data.table(cbind(best_number_trees, best_mtry, best_max_depth, "Macro-f1" = metrics$metrics$F1[metrics$metrics$Behaviour == "Macro-Average"]))
fwrite(params, file.path(base_path, "Output", species, "selected_hyperparameters.csv"))
