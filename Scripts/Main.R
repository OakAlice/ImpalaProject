#################
# This is the master script for the entire Impala analysis workflow
# Directs and instructs each stage of analysis

# Written by Oakleigh Wilson with help from Chris Bird and Christofer Clemente.
# Some stages are automated, some needed direct manual handling
#################

## Set up ------------------------------------------------------------------
setwd("C:/Users/PC/Documents/ImpalaProject")
source(file = file.path("Scripts", "config.R"))

## READING/ALIGNING DATA --------------------------------------------------
# Will loop through all collars, read in, align, scale, clean, rotate, and save into 24hr chunks
# highly custom and non-transferable
lapply(collars, function(x){
  collar_dir <- file.path("Data", "RawData", x)
  print(collar_dir)
  reading_artemis(collar_dir) # saves Data/RawData/Chunked as aligned files combing accel and gps
})

## CREATING BEHAVIOURAL CLASSIFIER ----------------------------------------
# This information and all steps are covered in break out file
# use this to generate the cleaned training data # fully manual # it also takes several days~weeks
  # rstudioapi::navigateToFile(file = file.path("Scripts", "AnnotateTrainingData", "CreateTrainingData.R"))

# we created 2 versions of the data - a nromla and a cleaned version
# how I formatted them can be found in formatting_clementeimpala.R

# when data created...
labelled_data <- fread("Data/LabelledData/PooledCleanedLabelledData.csv")

# generate features 
if (!file.exists("Data/LabelledData/FeatureLabelledData.csv")){
  features <- generate_features(labelled_data, sample_rate, metadata_cols = c("ID", "Time"), overlap_percent, window_length)
  fwrite(features, "Data/LabelledData/FeatureLabelledData.csv")
} else {
  features <- fread("Data/LabelledData/FeatureLabelledData.csv")
}

# plots to check everything looks good.
behavioural_classes_plot <- plotTraceExamples(behaviours = unique(labelled_data$Activity),
                                              labelled_data, 
                                              individuals = length(unique(labelled_data$ID)), 
                                              n_samples = 500, n_col = 1)
behavioural_frequency_plot <- plotVolume(labelled_data, frequency = sample_rate)
basic_umap_plot <- plotUMAP(features, label_column = 'Activity')
# if classes look good, progress...

if (!file.exists("Output/ClassificationModel/Final_model.rds")){
  tuning_metrics <- tune_model(features, 
                               k_folds = 3, inits = 5, rounds = 5, model = 'RF', bounds = 'default', 
                               target_classes = NULL, # no target classes for this run
                               optimisation_macro = "Macro-Average", # or Weighted-Average
                               optimisation_metric = 'F1')
  
  # and save each of these things for later
  tuning_performance_all <- rbindlist(tuning_metrics$metrics , idcol = "fold")
  tuning_performance_all$Behaviour <- gsub("Class: ", "", tuning_performance_all$Behaviour)
  fwrite(tuning_performance_all, "Output/ClassificationModel/TuningPerformance.csv")
  
  # I should figure out how to automate this... if it's even necessary
  parameters_all <- rbindlist(tuning_metrics$parameters)
  fwrite(parameters_all, "Output/ClassificationModel/TuningParameters.csv")
  parameters <- list(mtry = 15, num_trees = 500, max_depth = 25)
  
  final_model <- generate_final_model(features, model = 'RF', parameters)
  saveRDS(final_model, "Output/ClassificationModel/Final_model.rds")
} else{
  final_model <- readRDS("Output/ClassificationModel/Final_model.rds")
}

# MAKE PREDICTIONS ---------------------------------------------------------
# generate features, predict, and then throw in a postprocessor while we're here
# this is a super custom wrapper function just to get the messy code out of the main script
# I do not stand by or defend this function
lapply(collars, function(f){
  if (!file.exists(file.path("Output/BehaviouralPredictions", paste0(f, "_budget.png")))){
    make_predictions_wrapper(f, final_model, sampling_dates, sanity_check = TRUE,
                             labelled_features = features) # this is for the postprocessor
  }
})

# DEAD RECKONING ----------------------------------------------------------
# Waiting on Chris Bird to finalise the GPS cleaning cost function
# This is very hard and he is under a lot of pressure
# skip for now, come back to in 2027

# for now I'm just using the GPS already in there

# ECOLOGICAL ANALYSIS -----------------------------------------------------
completed_collars <- basename(list.dirs("Output/BehaviouralPredictions", recursive = FALSE))
lapply(collars, function(f){
  pred_files <- list.files(file.path("Output", "BehaviouralPredictions", f), full.names = TRUE, pattern = ".csv")
  preds <- lapply(pred_files, fread)
  preds <- rbindlist(preds)
  
  eco_plots <- generate_ecological_plots(preds)
  ggsave(file.path("Output/BehaviouralPredictions", paste0(f, "_budget.png")), plot = eco_plots$budget)
})


