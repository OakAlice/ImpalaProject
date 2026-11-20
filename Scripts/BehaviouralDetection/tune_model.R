# Master script for building the models for any given species etc ---------

tune_model <- function(features, k_folds = 3, inits = 5, rounds = 10, model = 'RF', bounds = 'default', 
                       target_classes,
                       optimisation_macro = "Macro-Average", optimisation_metric = 'weighted_F1'){
  #' optimisation_metric (string | options: weighted_F1, F1, ROC_AUC): what the HPO is optimising for
  #' target_classes (string, list): the classes which are most important to the ecologist
  
  dt <- copy(features)
  
  ids <- unique(dt$ID)
  ID_groups <- data.frame(
    ID = ids,
    group = sample(rep(1:k_folds, length.out = length(ids)))
  )
  
  metrics <- vector("list", k_folds) # performance
  plots <- vector("list", k_folds) # confusion plots
  parameters <- vector("list", k_folds) # the hyperparameters
  
  for (i in seq_len(k_folds)){
    
    test_IDs <- ID_groups$ID[ID_groups$group == i]
    test_data <- dt %>% dplyr::filter(ID %in% test_IDs)
    other_data <- dt %>% dplyr::filter(!ID %in% test_IDs)  
    
    # these are the default bounds to search if they haven't been explicitly defined
    if (model == 'RF' & bounds == 'default'){
      hpo_bounds <- list(
        mtry = c(2, 20),
        max_depth = c(5, 30),
        number_trees = c(100, 1000)
      )
    }
    
    results <- BayesianOptimization(
      FUN = function(number_trees, mtry, max_depth) {
        model_optimisation(
          feature_data = other_data,
          target_classes = target_classes,
          optimisation_metric = optimisation_metric,
          optimisation_macro = optimisation_macro,
          k_folds = 3, # hardcoding this
          number_trees = number_trees,
          mtry = mtry,
          max_depth = max_depth
        )
      },
      bounds = hpo_bounds,
      init_points = inits,
      n_iter = rounds,
      acq = "ucb",
      kappa = 2.576
    )
    
    # extract the best ones
    if (model == 'RF'){
      best_mtry <- round(results$Best_Par[["mtry"]],0)
      best_number_trees <- round(results$Best_Par[["number_trees"]],0)
      best_max_depth <- round(results$Best_Par[["max_depth"]],0)
      best_performance <- results$Best_Value # just for interest
    }
    
    # prepare all of the training data
    clean_cols <- clean_features(other_data, var_threshold = 0.3, corr_threshold = 0.9)
    training_data <- other_data %>%
      select(c(!!!syms(clean_cols), "Activity")) %>%
      na.omit() %>%
      mutate(Activity = as.factor(Activity))
    
    # weight by class frequency
    class_freq <- table(training_data$Activity)
    class_weights <- 1 / class_freq
    class_weights <- class_weights / sum(class_weights)
    weight <- as.numeric(class_weights[as.character(training_data$Activity)])
    
    # just check the mtry
    best_mtry <- max(1, min(round(best_mtry), length(clean_cols)))
    
    # make a model
    if (model == 'RF'){
      trained_model <- ranger(
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
    }
    
    # prepare the test data (features and metadata)
    complete_cases <- test_data %>% select(all_of(c(clean_cols, "Activity", "ID", "Time"))) %>% na.omit()
    test_features <- complete_cases %>% select(all_of(clean_cols)) %>% as.matrix()
    testing_metadata <- complete_cases %>% select(Activity, ID, Time)
    ground_truth_labels <- factor(testing_metadata$Activity)
    
    # Make predictions
    output <- predict(trained_model, data = test_features)
    predictions <- output$predictions
    lv <- colnames(predictions)
    predicted_class <- factor(lv[max.col(predictions, ties.method = "first")], levels = lv)
    predictions_df <- cbind(testing_metadata, predictions, predicted_class)
    
    fold_metrics <- compute_metrics(
      predicted_classes   = predictions_df$predicted_class,
      ground_truth_labels = factor(predictions_df$Activity, levels = lv),
      target_classes = target_classes
    )
    
    fold_confusion <- generate_confusion_plot(fold_metrics$conf_matrix_padded)
    
    fold_parameters <- data.frame(fold = i, 
                                  mtry = best_mtry,
                                  num_trees = best_number_trees,
                                  max_depth = best_max_depth)
    
    # return everything from the fold
    metrics[[i]] <- fold_metrics$metrics
    parameters[[i]] <- fold_parameters
    plots[[i]] <- fold_confusion
  }
  
  return(list(metrics = metrics, parameters = parameters, confusion_plots = plots))
}

# remove redundant and NA columns
clean_features <- function(feature_data, var_threshold, corr_threshold) {
  
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


get_score <- function(fold_metrics, optimisation_macro = "Macro-Average", optimisation_metric = "F1") {
  m <- fold_metrics$metrics
  stopifnot(optimisation_metric %in% names(m))
  m[[optimisation_metric]][m$Behaviour == optimisation_macro]
}

model_optimisation <- function(feature_data, target_classes, optimisation_metric, optimisation_macro, k_folds = 3, number_trees, mtry, max_depth) {
  
  # BO proposes continuous values so just round them to whatever
  number_trees <- round(number_trees)
  max_depth <- round(max_depth)
  
  # remove bad features
  feature_data <- as.data.table(feature_data)
  clean_cols <- clean_features(feature_data, var_threshold = 0.3, corr_threshold = 0.9)
  clean_feature_data <- feature_data %>%
    select(c(!!!syms(clean_cols), "Activity", "ID", "Time")) %>%
    na.omit() %>%
    as.data.frame()
  
  mtry <- max(1, min(round(mtry), length(clean_cols)))
  
  ids <- unique(clean_feature_data$ID)
  scores <- rep(NA_real_, k_folds)
  
  for (j in seq_len(k_folds)) {
    
    scores[j] <- tryCatch({
      
      # split by ID: random ~30% of individuals held out
      test_IDs <- sample(ids, ceiling(0.3 * length(ids)))
      
      training_data <- clean_feature_data %>%
        dplyr::filter(!ID %in% test_IDs) %>%
        select(-c(ID, Time)) %>%
        mutate(Activity = as.factor(Activity))
      
      validation_data <- clean_feature_data %>%
        dplyr::filter(ID %in% test_IDs) %>%
        select(-c(ID, Time)) %>%
        mutate(Activity = factor(Activity, levels = levels(training_data$Activity)))
      
      predictors <- setdiff(names(training_data), "Activity") 
      mtry_i <- max(1, min(round(mtry), length(predictors)))
      test_features <- validation_data[, predictors, drop = FALSE] 
      
      # class-frequency weights
      class_freq <- table(training_data$Activity)
      class_weights <- 1 / class_freq
      class_weights <- class_weights / sum(class_weights)
      weight <- as.numeric(class_weights[as.character(training_data$Activity)])
      
      trained_model <- ranger(
        dependent.variable.name = "Activity",
        data            = training_data,
        num.trees       = number_trees,
        mtry            = mtry,
        max.depth       = max_depth,
        sample.fraction = 1,        # small datasets fail otherwise
        classification  = TRUE,
        probability     = TRUE,     # gives the probability matrix
        importance      = "impurity",
        case.weights    = weight
      )
      
      test_features <- validation_data[, clean_cols, drop = FALSE]
      predictions <- predict(trained_model, data = test_features)$predictions
      lv <- colnames(predictions)
      predicted_class <- factor(lv[max.col(predictions, ties.method = "first")], levels = lv)
      
      fold_metrics <- compute_metrics(
        predicted_classes = predicted_class,
        ground_truth_labels = factor(validation_data$Activity, levels = lv),
        probabilities = predictions,
        target_classes = target_classes
      )
      
      get_score(fold_metrics, optimisation_macro, optimisation_metric)
      
    }, error = function(e) {
      message("Error in inner fold ", j, ": ", conditionMessage(e))
      flush.console()
      NA_real_
    })
  }
  
  scores[is.na(scores)] <- 0 # penalty for failed or undefined folds
  list(Score = mean(scores), Pred = 0)
}

