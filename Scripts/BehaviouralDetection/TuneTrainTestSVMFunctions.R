# Anomaly Detection Model Tuning

## Function to split data into k chronological folds - each fold has all 3 activities and all individuals ####
make_folds <- function(df, k = 5) {
  n <- nrow(df)
  # Split indices into k chronological chunks
  cuts <- ceiling(seq(1, n, length.out = k + 1))
  
  fold_ids <- integer(n)
  for (i in seq_len(k)) {
    fold_ids[cuts[i]:cuts[i + 1]] <- i
  }
  
  df$fold <- fold_ids
  df
}

get_train_test <- function(data_folds, fold_i) {
  list(
    train = data_folds %>% filter(fold != fold_i),
    test  = data_folds %>% filter(fold == fold_i)
  )
}

select_features<- function(data, num_features){
  
  top_predictors <- tryCatch({
    
    setDT(data)
    
    # remove bad features
    clean_cols <- removeBadFeatures(data, var_threshold = 0.3, corr_threshold = 0.8)
    clean_feature_data <- data %>%
      select(c(!!!syms(clean_cols), "Activity")) %>% 
      na.omit()
    clean_feature_data$Activity <- as.factor(clean_feature_data$Activity)
    
    # Fit RF on the training data to fidn the best predictor variables
    RF_model <- ranger(
      formula = Activity ~ .,
      data = clean_feature_data,
      num.trees = 500,
      mtry = floor(sqrt(ncol(clean_feature_data) - 1)),
      importance = "impurity",
      classification = TRUE
    )
    
    # Extract variable importance and select top 30
    imp <- importance(RF_model)
    top_predictors <- names(sort(imp, decreasing = TRUE))[1:30]
    
  }, error = function(e) {
    message("Random forest failed: ", e$message)
    NULL  # return NULL if it fails
  })
  
  return(top_predictors)
}

# remove redundant and NA columns
removeBadFeatures <- function(feature_data, var_threshold, corr_threshold) {
  
  # Step 1: Calculate variance for numeric columns
  numeric_columns <- feature_data[, .SD, .SDcols = setdiff(names(feature_data), c("Activity", "ID", "Time", "fold"))]
  variances <- numeric_columns[, lapply(.SD, var, na.rm = TRUE)]
  selected_columns <- names(variances)[!is.na(variances) & variances > var_threshold]
  
  # Step 2: Remove highly correlated features
  numeric_columns <- numeric_columns[, ..selected_columns]
  corr_matrix <- cor(numeric_columns, use = "pairwise.complete.obs")
  high_corr <- caret::findCorrelation(corr_matrix, cutoff = corr_threshold)
  remaining_features <- setdiff(names(numeric_columns), names(numeric_columns)[high_corr])
  
  return(remaining_features)
}

run_cv_iteration <- function(train, validate, activity, kernel_option, cost_option, gamma_option) {
  message("Kernel: ", kernel_option, " | Cost: ", cost_option, " | Gamma: ", gamma_option)
  
  # check whether the target class in the training and validation data
  if (!activity %in% unique(train$Activity)){
    print("class not in the training data")
    next
  }
  if (!activity %in% unique(validate$Activity)){
    print("class not in the validation data")
    next
  }
  
  # Feature selection
  best_features <- select_features(train)
  
  # Subset to best predictors
  training_data <- train %>%
    select(c(!!!syms(best_features), "Activity")) %>%
    na.omit()
  
  # Class weights
  class_freq <- table(training_data$Activity)
  
  class_weights <- 1 / class_freq
  class_weights <- class_weights / sum(class_weights)
  
  training_data$Activity <- as.factor(training_data$Activity)
  
  # Train SVM
  SVM_model <- svm(
    Activity ~ ., 
    data = training_data,
    type = "C-classification",
    kernel = kernel_option,
    cost   = cost_option,
    gamma  = gamma_option,
    class.weights = class_weights
  )
  
  # Validation data
  validation_data <- validate %>%
    select(c(!!!syms(best_features), "Activity")) %>%
    na.omit()
  
  num_validate <- validation_data %>% select(c(!!!syms(best_features)))
  ground_truth <- validation_data %>% select(Activity)
  
  # Predictions
  predictions <- predict(SVM_model, newdata = num_validate)
  
  # Confusion matrix (padded to match dimensions)
  all_classes <- sort(union(unique(predictions), unique(ground_truth$Activity)))
  predicted_classes <- factor(unlist(predictions), levels = all_classes)
  ground_truth_labels <- factor(ground_truth$Activity, levels = all_classes)
  
  # F1 score (macro)
  cm <- confusionMatrix(predicted_classes, ground_truth_labels, positive = activity)
  
  metrics_table <- data.frame(
    fold       = fold,
    activity   = activity,
    Accuracy   = cm$overall["Accuracy"],
    Kappa      = cm$overall["Kappa"],
    Sensitivity= cm$byClass["Sensitivity"],
    Specificity= cm$byClass["Specificity"],
    Precision  = cm$byClass["Pos Pred Value"],
    Recall     = cm$byClass["Sensitivity"],
    F1         = cm$byClass["F1"],
    BalancedAcc= cm$byClass["Balanced Accuracy"]
  )
  
  return(metrics_table)
}

# Main optimisation function
ModelOptimisation <- function(activity, feature_data, kernel_option, cost_option, gamma_option, k = 3) {
  # Split into folds
  data_folds <- feature_data %>%
    arrange(ID, Activity, Time) %>%
    group_by(ID, Activity) %>%
    group_modify(~ make_folds(.x, k = k)) %>%
    ungroup()
  
  # Run CV across k folds
  results <- lapply(1:k, function(i) {
    cv <- get_train_test(data_folds, i) # new split of data for each fold
    
    tryCatch({
      run_cv_iteration(
        train = cv$train,
        validate = cv$test,
        activity = activity,
        kernel_option = kernel_option,
        cost_option = cost_option,
        gamma_option = gamma_option
      )
    }, error = function(e) {
      message("Error in CV iteration ", i, ": ", e$message)
      return(NA)
    })
  })
  
  results_table <- rbindlist(results)
  
  f1s <- results_table[,F1]
  
  # Extract F1 scores
  f1 <- mean(f1s, na.rm = TRUE)
  
  # making this not a list with Score and Pred will trigger 
  # Error in This_Score_Pred$Score : $ operator is invalid for atomic vectors
  # because BayesianOptimisation is trying to call the Score object
  return(list(Score = f1, Pred = NA))
}

# Final performance of the models averaged --------------------------------
performance_files <- list.files(file.path(base_path, "ModelBuilding"), pattern = "_CrossValidation.csv", full.names = TRUE)
performance <- lapply(performance_files, function(x){
  dat <- fread(x)
  dat %>% select(!fold) %>% group_by(activity) %>% summarise(across(everything(), mean, na.rm = TRUE))
})
performance <- rbindlist(performance)
fwrite(performance, file.path(base_path, "ModelBuilding", "Average_CrossValidated_Performance.csv"))
