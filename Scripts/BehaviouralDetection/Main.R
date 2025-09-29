# Minimal script for very basic Random Forest model building --------------
# Written by Oakleigh Wilson, ~2024
# for help, please don't hesistate to contact me at:
# oakleigh.wilson@research.usc.edu.au

# this is the path where I have my main folder
base_path <- "~/ImpalaProject"

#install.packaged("pacman")
library(pacman)

p_load(tidyverse, 
       data.table,
       caret,
       ggplot2,
       HMM,
       torch,
       tictoc,
       zoo,
       lubridate,
       rlang,
       tsfeatures,
       lubridate,
       future,
       future.apply,
       rBayesianOptimization,
       e1071
)

# Define variables for this run -------------------------------------------
species <- "Clemente_Imapala" # dataset name # just give it a name to keep track of it
sample_rate <- 50 # sampling frequency of the raw data # in Hz
desired_window <- 1 # length of each window from which features are generated # in seconds
desired_overlap <- 0 # overlap between the feature windows # as a percentage
split_data_method <- "individual" # way to separate the test vs validate vs train data
  # individual just means based on the ID
available_axes <- c("X", "Y", "Z") # this is the names of your accelerometer axes
  # the others columns this code expects are 'ID', 'Time', and 'Activity'

# Define target behaviours ------------------------------------------------
# define the behaviours I want to detect
target_activities <- c("Walking", "Sprinting_Bounding", "Trotting", "Scratching", "sleeping", "Grazing")

# Format Data -------------------------------------------------------------
# data has to be read in anf dormatted manually
source(file = file.path(base_path, "Scripts", "BehaviouralDetection", "Clemente_Impala_Formatting.R"))

# Making the Model --------------------------------------------------------
# have to manually mess around in the formatting file btw
source(file = file.path(base_path, "Scripts", "BehaviouralDetection", "GenerateFeatures_Functions.R"))
source(file = file.path(base_path, "Scripts", "BehaviourDetection", "Clemente_Impala_Formatting.R"))

# makes a separate binary SVM for each of the target behaviours
# functions for tune, train, and test a model and generate predictions on the test data (cross-validated)
# hyperparameter optimisation calculated on the first fold only and carried over btw
source(file = file.path(base_path, "Scripts", "BehaviouralDetection", "DesignModel.R"))
source(file = file.path(base_path, "Scripts", "BehaviouralDetection", "GenerateModels.R"))

# this should save all the output into the folder Output and there will be:
  # selected_hyperparameters.csv <- the parameters that were chosen by the tuning process
  # Activity_model.rds <- the actual model it makes
  # Performance_metrics.csv <- how it performed per class and on average

# Process unlabelled data -------------------------------------------------
source(file = file.path(base_path, "Scripts", "ModelBuilding", "ProcessUnlabelledData.R"))








 