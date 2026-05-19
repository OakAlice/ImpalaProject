#################
# Main

# Overview:
# This is the master script for the entire Impala analysis workflow
# Directs and instructs each stage of analysis
# The DR and Behavioural Classification parts of the code are transferable
# but the data reading, cleaning, and debugging, is very customised due to complex bugs 
# For more instructions, see the associated README.md and publication

#################

## Set up ------------------------------------------------------------------
base_path <- "C:/Users/PC/Documents/ImpalaProject"

pacman:: p_load(#general function
                data.table, tidyverse, lubridate, stringr, plotly, patchwork,
                # parallel processing
                future,
                # time series and signal analysis
                zoo, tsfeatures, signal,
                # machine learning
                caret, xgboost, ranger, rBayesianOptimization,
                # other
                av, shiny, processx, roll
                )

sampling_start <- fread(file.path(base_path, "Notes/Metadata.csv")) %>%
  mutate(StartDate = as.Date(as.character(ReleaseDate), format = "%d-%b-%y")) %>%
  select(CollarNumber, StartDate)
path_to_calinfo <- file.path(base_path, "Notes/ImpalaCollaringTimes.csv")

## PART ONE: READING/ALIGNING DATA -----------------------------------------
source(file = file.path(base_path, "Scripts", "WranglingData", "DataReadFunctions.R"))
# figured this out after generating and exploring some of the data
# only need to run this once
source(file = file.path(base_path, "Scripts", "WranglingData", "UnitsScales.R"))

# list all the colalrs
collars <- c(#"Collar_11", "Collar_12", "Collar_14", "Collar_2", 
             "Collar_15", "Collar_3", "Collar_5", "Collar_6" , "Collar_7", "Collar_8")
# Read in and Align the Boards --------------------------------------------
# Will loop through all collars
for (Collar in collars){
  print(Collar)
  collar_dir <- file.path(base_path, "Data", "RawData", Collar)
  chunked_dir_path <- file.path(collar_dir, "Chunked")
  
  # read raw txt data together, clean, and combine accel & GPS sources
  # account for different device orientations
  source(file = file.path(base_path, "Scripts", "WranglingData", "Main_ReadData.R"))
}

## PART TWO: CREATING LABELLED DATA -----------------------------------------
# This information and all steps are covered in break out file
rstudioapi::navigateToFile(file = file.path(base_path, "Scripts", "BehaviouralDetection", "GenerateTrainingData", "Main_GenerateTrainingData.R"))
# this generates the cleaned_training data and is pretty much fully manual # it also takes several days - weeks

# now generate features across the labelled data
source(file = file.path(base_path, "Scripts", "BehaviouralDetection", "GenerateTrainingData", "Functions_GenerateFeatures.R"))
# define the feature settings
desired_window <- 1 # in seconds
sample_rate <- 50
desired_overlap <- 0
available_axes <- c("RawAX", "RawAY", 'RawAZ') # the name of the axes
source(file = file.path(base_path, "Scripts", "BehaviouralDetection", "GenerateTrainingData", "Features_TrainingData.R"))
# # and do feature selection / cluster analysis # hasn't really been written yet but can be expanded
# source(file = file.path(base_path, "Scripts", "BehaviouralDetection", "GenerateTrainingData", "CleanFeatures_TrainingData.R"))

## PART FOUR: MAKE THE BEHAVIOURAL MODEL -----------------------------------
source(file = file.path(base_path, "Scripts", "BehaviouralDetection", "ModelDesign", "Main_DesignModel.R"))



## PART FIVE: DEAD RECKONING -----------------------------------------------
# source(file = file.path(base_path, "Scripts/DeadReckoning/ExtractingCalibrationEvents.R"))
# source(file = file.path(base_path, "Scripts", "DeadReckoning", "Main_DeadReckoning.R"))



