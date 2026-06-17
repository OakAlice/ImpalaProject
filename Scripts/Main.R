#################
# Main

# Overview:
# This is the master script for the entire Impala analysis workflow
# Directs and instructs each stage of analysis
# The DR and Behavioural Classification parts of the code are transferable
# but the data reading, cleaning, and debugging, is very customised due to complex bugs 
# For more instructions, see the associated README.md and publication

# Written by Oakleigh Wilson with help from Chris Bird and Christofer Clemente.
# Some stages are automatable, some needed direct manual handling

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
                caret, xgboost, ranger, rBayesianOptimization, nnet,
                # postprocessing
                HMM,
                # other
                av, shiny, processx, roll
                )

source(file = file.path(base_path, "Scripts", "PlotFunctions.R"))


## Some variables ----------------------------------------------------------
sampling_start <- fread(file.path(base_path, "Notes/Metadata.csv")) %>%
  mutate(StartDate = as.Date(as.character(ReleaseDate), format = "%d-%b-%y")) %>%
  select(CollarNumber, StartDate)
path_to_calinfo <- file.path(base_path, "Notes/ImpalaCollaringTimes.csv")


## PART ONE: READING/ALIGNING DATA -----------------------------------------
# list all the colalrs
collars <- c(#"Collar_11", "Collar_12", "Collar_14", 
  "Collar_2", "Collar_15", "Collar_3", "Collar_5", "Collar_6" , "Collar_7", "Collar_8")
# Collar <- "Collar_8"
# Will loop through all collars, read in, align, scale, clean, and save into 24hr chunks
source(file = file.path(base_path, "Scripts", "WranglingData", "Main_ReadData.R"))

## PART TWO: CREATING TRAINING DATA ----------------------------------------
# This information and all steps are covered in break out file
rstudioapi::navigateToFile(file = file.path(base_path, "Scripts", "BehaviouralDetection", "GenerateTrainingData", "CreateTrainingData.R"))
# use this to generate the cleaned training data # pretty much fully manual # it also takes several days~weeks

# now generate features across the labelled data
source(file = file.path(base_path, "Scripts", "BehaviouralDetection", "GenerateTrainingData", "Functions_GenerateFeatures.R"))
# define the feature settings
desired_window <- 1 # in seconds
sample_rate <- 50
desired_overlap <- 0
available_axes <- c("RawAX.butt", "RawAY.butt", 'RawAZ.butt') # the name of the axes
source(file = file.path(base_path, "Scripts", "BehaviouralDetection", "GenerateTrainingData", "GenerateFeaturesTrainData.R"))
# double check whether these classes are decent... if not, go back into cleaning and relabelling data
# rstudioapi::navigateToFile(file = file.path(base_path, "Scripts", "BehaviouralDetection", "GenerateTrainingData", "DecideClasses_TrainingData.R"))

## PART THREE: MAKE THE BEHAVIOURAL MODEL -----------------------------------
# make a model to find those classes # look at how post-processing improves it
source(file = file.path(base_path, "Scripts", "BehaviouralDetection", "ModelDesign", "Main_DesignModel.R"))

## PART FOUR: MAKE PREDICTIONS WITH THE BEHAVIOURAL MODEL -------------------
# loops through all collars
# Will take a VERY long time to generate all features. Predictions dont take long
source(file = file.path(base_path, "Scripts", "BehaviouralDetection", "MakePredictions", "Main_Unlabelled.R"))

# this now concludes the behavioural prediction section of the script. 
# Use these behaviours for the next section.

## PART FIVE: DEAD RECKONING -----------------------------------------------
source(file = file.path(base_path, "Scripts/DeadReckoning/ExtractingCalibrationEvents.R"))
# do this manually until very sure that it's working
rstudioapi::navigateToFile(file = file.path(base_path, "Scripts", "DeadReckoning", "Main_DeadReckoning.R"))

# This now concludes the tracking.
# Use the behaviours and the location maps for the ecological analysis.

## PART SIX: ECOLOGICAL ANALYSIS -------------------------------------------


