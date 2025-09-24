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
       ranger
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

# Format Data -------------------------------------------------------------
# data has to be read in anf dormatted manually
source(file = file.path(base_path, "Scripts", "BehaviouralDetection", "Clemente_Impala_Formatting.R"))

# Data Visualisation ------------------------------------------------------


# these are all the functions associated with generating features
source(file = file.path(base_path, "Scripts", "BehaviouralDetection", "GenerateFeatures_Functions.R"))

# this is the script for generating the features
# create a file that matches the functionality of the example in Vehkaoja_Dog_Formatting.R
# must format the data to match column names I use in the function (listed above)
# then generate features within each ID - saves incrementally (per ID) and then overall
source(file = file.path(base_path, "Scripts", "DataFormatting", paste0(species, "_Formatting.R")))

# Extract test data -------------------------------------------------------
# Split out test data -----------------------------------------------------
data <- fread(file.path(base_path, "Data", species, "Feature_data.csv"))

# logic for selecting the hold-out test set
test_IDs <- sample(unique(data$ID), 0.4*length(unique(data$ID)))
print(paste0("number of individuals in the test set: ", length(test_IDs)))

test_data <- data %>% filter(ID %in% test_IDs)
other_data <- data %>% filter(!ID %in% test_IDs) 

fwrite(test_data, file.path(base_path, "Data", species, "test_data.csv"))
fwrite(other_data, file.path(base_path, "Data", species, "other_data.csv"))

# Model design: hyperparameter tuning -------------------------------------
# Define the bounds in which you want to search for hyperparameters
bounds <- list(
  mtry = c(2, 50),
  max_depth = c(5, 30),
  number_trees = c(100, 1000)
)

# Make the Model ----------------------------------------------------------
# functions for tune, train, and test a model and generate predictions on the test data
source(file = file.path(base_path, "Scripts", "ModelBuilding", "HPOFunctions.R"))
source(file = file.path(base_path, "Scripts", "ModelBuilding", "TestFunctions.R"))

# script that makes it happen
source(file = file.path(base_path, "Scripts", "ModelBuilding", "TuneTrainTestModel.R"))

# this should save all the output into the folder Output/species and there will be:
  # selected_hyperparameters.csv <- the parameters that were chosen by the tuning process
  # Activity_model.rds <- the actual model it makes
  # Performance_metrics.csv <- how it performed per class and on average
  # Confusion_matrix.csv
  # Predictions.csv <- Per class confidence and selected class



# Process unlabelled data -------------------------------------------------
source(file = file.path(base_path, "Scripts", "ModelBuilding", "ProcessUnlabelledData.R"))








 