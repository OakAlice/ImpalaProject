# Generate the final model, to be used on the unlabelled data -------------
generate_final_model <- function(data, model = 'RF', parameters){
  
  dt <- copy(data)
  
  clean_cols <- clean_features(data, var_threshold = 0.3, corr_threshold = 0.9)
  dt <- dt %>%
    select(c(!!!syms(clean_cols), "Activity")) %>%
    na.omit() %>%
    mutate(Activity = as.factor(Activity))
  
  # weight by class frequency
  class_freq <- table(dt$Activity)
  class_weights <- 1 / class_freq
  class_weights <- class_weights / sum(class_weights)
  weight <- class_weights[dt$Activity]
  
  final_model <- ranger(
    dependent.variable.name = "Activity",
    data = dt,
    num.trees = parameters$num_trees,
    mtry = parameters$mtry,
    max.depth = parameters$max_depth,
    classification = TRUE,
    probability = TRUE,
    importance = "impurity",
    case.weights = weight
  )
  
  return(final_model)
}
