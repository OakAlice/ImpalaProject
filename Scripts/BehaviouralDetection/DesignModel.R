
# Design Models -----------------------------------------------------------
source(file = file.path(base_path, "Scripts", "BehaviouralDetection", "TuneTrainTestSVMFunctions.R"))

# Design the models for behaviour detection -------------------------------

# this system will use multiple binary SVMs (each specialised to a specific behaviour)
# to detect the target behaviours (i.e., Locomotion and Inactive, excluding "other")
# as opposed to building one model that generalises between individuals
# each model will be specific to the individual 

all_data <- fread(file.path(base_path, "ModelBuilding", paste0("Feature_data.csv")))

# Code --------------------------------------------------------------------
# define the bounds for hyperpatameter tuning
kernel_map <- c("radial", "linear", "polynomial")
bounds <- list(
  gamma  = c(0.001, 1),   # range for gamma
  cost   = c(0.1, 10),    # range for cost
  kernel = c(1L, 3L)      # encoded kernel index
)

# assign data into test folds
data_folds <- all_data %>%
  arrange(ID, Activity, Time) %>%
  group_by(ID, Activity) %>%
  group_modify(~ make_folds(.x, k = 5)) %>%
  ungroup()

for (activity in target_activities) {
  results <- data.frame()  # initialise
  
  for (fold in 1:5) {
    cv <- get_train_test(data_folds, fold) # extract the data for this one fold
    other <- cv$train
    test  <- cv$test
    
    # make binary labels
    other2 <- other %>%
      mutate(Activity = ifelse(Activity == activity, activity, "Other"))
    test2 <- test %>%
      mutate(Activity = ifelse(Activity == activity, activity, "Other"))
    
    if (fold == 1) { 
      # tune hyperparameters only once in the first loop
      # figured that it was overkill to do a full nested cross-validation
      bo_results <- BayesianOptimization(
        FUN = function(kernel, cost, gamma) {
          kernel_choice <- kernel_map[round(kernel)]
          
          score <- tryCatch({
            ModelOptimisation(
              activity       = activity,
              feature_data   = other2,
              kernel_option  = kernel_choice,
              cost_option    = cost,
              gamma_option   = gamma
            )
          }, error = function(e) {
            # return a very bad score if it fails
            list(Score = -Inf, Pred = 0)
          })
          
          score
        },
        bounds = bounds,
        init_points = 5,
        n_iter = 10,
        acq = "ucb",
        kappa = 2.576
      )
      
      # save best
      tuning <- data.frame(
        activity       = activity,
        best_kernel    = kernel_map[round(bo_results$Best_Par[["kernel"]],0)],
        best_gamma     = bo_results$Best_Par[["gamma"]],
        best_cost      = bo_results$Best_Par[["cost"]],
        best_performance = bo_results$Best_Value
      )
      fwrite(tuning, file.path(base_path, "ModelBuilding", paste0(activity, "_HP_Optimisation.csv")))
    }
    
    # use tuned params
    kernel_option <- tuning$best_kernel
    cost_option   <- tuning$best_cost
    gamma_option  <- tuning$best_gamma
    
    output <- run_cv_iteration(train = other2, 
                               validate = test2, 
                               activity, kernel_option, cost_option, gamma_option)
    
    output <- as.data.frame(output)
    
    results <- rbind(results, output)
    
  }
  
  # save that
  fwrite(results, file.path(base_path, "ModelBuilding", paste0(activity, "_CrossValidation.csv")))
}


