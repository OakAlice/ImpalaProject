# Code to generate the final models ---------------------------------------

all_data <- fread(file.path(base_path, "ModelBuilding", paste0("Feature_data.csv")))

for (activity in target_activities){
  
  # load in the optimal hyperparameters
  parameters <- fread(file.path(base_path, "ModelBuilding", paste0(activity, "_HP_Optimisation.csv")))
  
  # set to binary
  other2 <- all_data %>%
    mutate(Activity = ifelse(Activity == activity, activity, "Other"))
  
  # Feature selection
  best_features <- select_features(other2)
  
  # Subset to best predictors
  training_data <- other2 %>%
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
    kernel = parameters$best_kernel,
    cost   = parameters$best_cost,
    gamma  = parameters$best_gamma,
    class.weights = class_weights
  )
  
  # save this model as an RDS object
  saveRDS(SVM_model, file = file.path(base_path, "ModelBuilding", paste0(activity, "_SVM.RDS")))
}
