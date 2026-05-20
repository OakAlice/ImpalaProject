#################
# Functions_TuneTrainTestXGBoost

# Overview:
# Functions to validate and train and test the behaviour classification models
# Have currently got options for several different model types

# Currently has options for Random Forest, XGBoost, and a basic CNN

#################


# Basic helpers -----------------------------------------------------------
# remove redundant and NA columns
removeBadFeatures <- function(feature_data, var_threshold, corr_threshold) {
  
  # Step 1: Calculate variance for numeric columns
  numeric_columns <- feature_data[, .SD, .SDcols = !names(feature_data) %in% c("Activity", "ID", "Time")]
  variances <- numeric_columns[, lapply(.SD, var, na.rm = TRUE)]
  selected_columns <- names(variances)[!is.na(variances) & variances > var_threshold]
  
  if (!length(selected_columns) > 2){
    selected_columns <- names(variances)[!is.na(variances)] # include all of them then
  }
  
  # Step 2: Remove highly correlated features
  numeric_columns <- numeric_columns[, ..selected_columns]
  corr_matrix <- cor(numeric_columns, use = "pairwise.complete.obs")
  high_corr <- findCorrelation(corr_matrix, cutoff = corr_threshold)
  remaining_features <- setdiff(names(numeric_columns), names(numeric_columns)[high_corr])
  
  return(remaining_features)
}

calculate_performance <- function(predicted_classes, ground_truth_labels){
  # ensure all levels are present
  all_classes <- sort(union(unique(predicted_classes), unique(ground_truth_labels)))
  predicted_classes <- factor(unlist(predicted_classes), levels = all_classes)
  ground_truth_labels <- factor(ground_truth_labels, levels = all_classes)

  # make a confusion matrix
  confusion_matrix <- table(predicted_classes, ground_truth_labels)
  
  # Handling mismatched dimensions
  all_classes <- sort(union(colnames(confusion_matrix), rownames(confusion_matrix)))
  conf_matrix_padded <- matrix(0, 
                               nrow = length(all_classes), 
                               ncol = length(all_classes),
                               dimnames = list(all_classes, all_classes))
  conf_matrix_padded[rownames(confusion_matrix), colnames(confusion_matrix)] <- confusion_matrix
  
  # Calculate F1 scores
  confusion_mtx <- confusionMatrix(conf_matrix_padded)
  byClass <- confusion_mtx$byClass
  
  f1 <- byClass[, "F1"]
  support <- rowSums(confusion_mtx$table)  # True instances per class
    
  f1[is.nan(f1)] <- 0 # if you leave NAs in, it fails. and if you na.rm() it gets too easy because classes are ommitted
  f1[is.na(f1)] <- 0
  weighted_f1 <- weighted.mean(f1, w = support)
  
  return(list(conf_matrix_padded = conf_matrix_padded, 
              confusion_mtx = confusion_mtx,
              weighted_f1 = weighted_f1))
}

split_training_val <- function(feature_data){
  test_IDs <- sample(unique(feature_data$ID), ceiling(0.3 * length(unique(feature_data$ID))))
  
  validation_data <- feature_data %>% dplyr::filter(ID %in% test_IDs)
  training_data   <- feature_data %>% dplyr::filter(!ID %in% test_IDs)
  
  # remove ID and Time
  training_data <- training_data %>%
    select(-c(ID, Time)) %>%
    mutate(Activity = as.factor(Activity))
  
  validation_data <- validation_data %>%
    select(-c(ID, Time)) %>%
    mutate(Activity = as.factor(Activity))
  
  return(list(training_data = training_data,
              validation_data = validation_data))
}


# Full things -------------------------------------------------------------
# main call that splits data, generates function, and validates
XGBoostModelOptimisation <- function(feature_data, eta, nrounds, max_depth){
  
  f1_scores <- list()  # List to store F1-scores
  
  # Repeat the process 3 times
  for (i in 1:3) {
    
    tryCatch({
      dat <- split_training_val(feature_data)
      training_data <- dat$training_data
      validation_data <- dat$validation_data
      
    }, error = function(e) {
      message("Error in data splitting: ", e$message)
    })
    
    # Train RF model
    tryCatch({
      
      # weight by class frequency
      class_weights <- 1 / table(training_data$Activity)
      class_weights <- class_weights / sum(class_weights)
      weight <- as.numeric(class_weights[as.character(training_data$Activity)])
      
      # prepare matrix for xgboost
      feature_cols <- colnames(training_data)[colnames(training_data) != "Activity"]
      label <- as.numeric(as.factor(training_data$Activity)) - 1  # 0-indexed classes
      dtrain <- xgb.DMatrix(
        data   = as.matrix(training_data[, ..feature_cols]),
        label  = label,
        weight = weight
      )

      XGB_model <- xgb.train(
        params = list(
          booster     = "gbtree",
          objective   = "multi:softprob",
          num_class   = length(unique(label)),
          eta         = as.integer(eta),
          max_depth   = as.integer(max_depth),
          eval_metric = "mlogloss"
        ),
        data    = dtrain,
        nrounds = as.integer(nrounds),
        verbose = 0
      )
      
    }, error = function(e) {
      message("Error in model training: ", e$message)
      stop()
    })
    
    #### Validate the model
    tryCatch({
      numeric_validation_data <- as.matrix(validation_data[, !names(validation_data) %in% c("Activity"), with = FALSE])
      ground_truth_labels <- validation_data$Activity
      
      if (anyNA(numeric_validation_data)) {
        message("Validation data contains missing values!")
        flush.console()
      }
      
      # now do the validation
      dtest <- xgb.DMatrix(data = as.matrix(numeric_validation_data[, feature_cols]))
      pred_probs <- predict(XGB_model, newdata = dtest)
      
      # reshape from flat vector to matrix (nrows x num_classes)
      num_classes <- length(unique(label))
      pred_matrix <- matrix(pred_probs, ncol = num_classes, byrow = TRUE)
      
      # get predicted class index (0-indexed), convert back to original labels
      predicted_class_idx <- max.col(pred_matrix) - 1  # back to 0-indexed
      activity_levels <- levels(as.factor(training_data$Activity))
      predicted_classes <- activity_levels[max.col(pred_matrix)]  # 1-indexed directly
      
    }, error = function(e) {
      message("Error in making predictions: ", e$message)
      flush.console()
      stop()
    })
    
    # calculate the performance
    weighted_f1 <- calculate_performance(predicted_classes, ground_truth_labels)$weighted_f1
    f1_scores[[i]] <- weighted_f1
  }
  
  #### Calculate average F1-scors
  # same NA problem
  f1s <- unlist(f1_scores)
  f1s[is.na(f1s)] <- 0
  average_macro_f1 <- mean(f1s)
  
  # no preds for this one
  return(list(Score = average_macro_f1, Pred = NA))
}



# Random forest optimisation ----------------------------------------------
RFModelOptimisation <- function(feature_data, number_trees, mtry, max_depth){
  
  # remove bad features
  feature_data <- as.data.table(feature_data)
  
  if (mtry > length(clean_cols)-1){
    message("mtry too big, making max clean cols")
    flush.console()
    mtry <- length(clean_cols)
  }
  
  f1_scores <- list()  # List to store F1-scores
  
  # Repeat the process 3 times
  for (i in 1:3) {
    
    tryCatch({
      dat <- split_training_val(feature_data)
      training_data <- dat$training_data
      validation_data <- dat$validation_data
      
    }, error = function(e) {
      message("Error in data splitting: ", e$message)
    })
    
    # Train RF model
    tryCatch({
      
      # weight by class frequency
      class_weights <- 1 / table(training_data$Activity)
      class_weights <- class_weights / sum(class_weights)
      weight <- class_weights[training_data$Activity]
      
      RF_model <- ranger(
        dependent.variable.name = "Activity",
        data = training_data,
        num.trees = number_trees,
        mtry = mtry,
        max.depth = max_depth,
        sample.fraction = 1, # select all the data (small datasets will fail otherwise)
        classification = TRUE,
        importance = "impurity",
        case.weights = weight
      )
      
    }, error = function(e) {
      message("Error in RF training: ", e$message)
      stop()
    })
    
    #### Validate the model
    tryCatch({
      numeric_validation_data <- as.matrix(validation_data[, !names(validation_data) %in% c("Activity"), with = FALSE])
      ground_truth_labels <- validation_data$Activity
      
      if (anyNA(numeric_validation_data)) {
        message("Validation data contains missing values!")
        flush.console()
      }
      
      numeric_validation_data <- as.data.frame(numeric_validation_data)
      predictions <- predict(RF_model, data = numeric_validation_data)
      predicted_classes <- predictions$predictions
      
    }, error = function(e) {
      message("Error in making predictions: ", e$message)
      flush.console()
      stop()
    })
    
    # calculate the performance
    weighted_f1 <- calculate_performance(predicted_classes, ground_truth_labels)$weighted_f1
    f1_scores[[i]] <- weighted_f1
  }
  
  #### Calculate average F1-scors
  # same NA problem
  f1s <- unlist(f1_scores)
  f1s[is.na(f1s)] <- 0
  average_weighted_f1 <- mean(f1s)
  
  # no preds for this one
  return(list(Score = average_weighted_f1, Pred = NA))
}




# NN version -------------------------------------------------------------
NNModelOptimisation <- function(feature_data, size, decay) {
  
  feature_data <- as.data.table(feature_data)
  feature_data <- feature_data %>% mutate_if(is.character, as.factor)
  
  f1_scores <- list()
  
  for (i in 1:3) {
    
    tryCatch({
      dat <- split_training_val(feature_data)
      training_data <- dat$training_data
      validation_data <- dat$validation_data
    }, error = function(e) {
      message("Error in data splitting: ", e$message)
    })
    
    tryCatch({
      class_weights <- 1 / table(training_data$Activity)
      class_weights <- class_weights / sum(class_weights)
      weight <- as.numeric(class_weights[as.character(training_data$Activity)])
      
      NN_model <- nnet(
        Activity ~ .,
        data    = training_data,
        size    = size,
        decay   = decay,
        maxit   = 500,
        weights = weight,
        trace   = FALSE,
        MaxNWts  = 10000
      )
      
    }, error = function(e) {
      message("Error in NN training: ", e$message)
      stop()
    })
    
    tryCatch({
      predicted_classes <- predict(NN_model, newdata = validation_data, type = "class")
      predicted_classes <- factor(predicted_classes, levels = levels(training_data$Activity))
      ground_truth_labels <- validation_data$Activity
      
    }, error = function(e) {
      message("Error in making predictions: ", e$message)
      flush.console()
      stop()
    })
    
    weighted_f1  <- calculate_performance(predicted_classes, ground_truth_labels)$weighted_f1
    f1_scores[[i]] <- weighted_f1
  }
  
  f1s <- unlist(f1_scores)
  f1s[is.na(f1s)] <- 0
  average_weighted_f1 <- mean(f1s)
  
  return(list(Score = average_weighted_f1, Pred = NA))
}
