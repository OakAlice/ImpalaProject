# Test the model ----------------------------------------------------------
test_data <- fread(file.path(base_path, "Data", species, "test_data.csv")) 
RF_model <- readRDS(file.path(base_path, "Output", species, "Activity_model.rds"))
clean_cols <- 
  
  
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
