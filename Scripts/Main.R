# The overall main file for the whole impala project ----------------------

## Set up ------------------------------------------------------------------
base_path <- "C:/Users/PC/Documents/ImpalaProject"

pacman:: p_load(av,
                data.table, 
                lubridate,
                plotly,
                stringr,
                shiny,
                tidyverse,
                zoo,
                future,
                tsfeatures,
                processx,
                patchwork,
                signal,
                roll)

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
             "Collar_15", 
             "Collar_3", "Collar_5", "Collar_6" , "Collar_7", "Collar_8")
# Read in and Align the Boards --------------------------------------------
# Will loop through all collars
for (Collar in collars){
  print(Collar)
  collar_dir <- file.path(base_path, "Data", "RawData", Collar)
  chunked_dir_path <- file.path(collar_dir, "Chunked")
  
  # read raw txt data together, clean, and combine accel & GPS sources
  # account for different device orientations
  source(file = file.path(base_path, "Scripts", "WranglingData", "Main_ReadData.R"))
  
  ## PART FIVE: DEAD RECKONING -----------------------------------------------
  # source(file = file.path(base_path, "Scripts/DeadReckoning/ExtractingCalibrationEvents.R"))
  # source(file = file.path(base_path, "Scripts", "DeadReckoning", "Main_DeadReckoning.R"))

}





sampling_start <- fread(file.path(base_path, "Notes/Metadata.csv")) %>%
  mutate(StartDate = as.Date(as.character(ReleaseDate), format = "%d-%b-%y")) %>%
  select(CollarNumber, StartDate)



## PART TWO: ANNOTATING BEHAVIOURS -----------------------------------------
# Extracting video information --------------------------------------------
# as every camera encodes its metadata slightly different, this is a quite manual process
# therefore, update this to match the cameras
# loops through all collars with associated videos
source(file = file.path(base_path, "Scripts", "RoughAlignment", "VideoInfoExtraction.R"))

# Rough alignment of the accelerometer and videos -------------------------
# use the manual slide bar to roughly align the videos with the accelerometer...
# Instructions in the Notes/VideoAlignment_Instructions.docx file
rstudioapi::navigateToFile(file = file.path(base_path, "Scripts", "RoughAlignment", "AccelDelayFinder.R"))

# Annotate the clipped segments of video ----------------------------------
# Use the matlab SyncStation to apply detailed labels
# these can be found in the Scripts/SyncStation folder
# Instructions continued in the Notes/VideoAlignment_Instructions.docx file

## PART THREE: CREATING TRAINING DATA --------------------------------------
# combine the matlab annotations and split out into the individual behaviours
source(file = file.path(base_path, "Scripts",  "BehaviouralDetection","GenerateTrainingData", "Create_TrainingData"))
# Clean the data in matlab !!!!!!!!
# then recombine the cleaned stuff back together
files <- list.files(file.path(base_path, "Data", "LabelledData", "Split"), recursive = TRUE, full.names = TRUE, pattern = "_tagged.csv")
raw_data <- lapply(files, function(file) {
  df <- fread(file)
  
  df <- df %>%
    mutate(Time = as.POSIXct((Time - 719529)*86400, origin = "1970-01-01", tz = "UTC"),
           Activity = ifelse(mech_behaviour == 0, NA, Activity),
           GroupedActivity = ifelse(mech_behaviour == 0, NA, GroupedActivity)) %>%
    select(-c(eco_behaviour, mech_behaviour))
  return(df)
})
raw_data <- bind_rows(raw_data) %>% na.omit()
fwrite(raw_data, file.path(base_path, "Data", "LabelledData", "CleanLabelledData.csv"))

# now generate the features for the cleaned data
source(file = file.path(base_path, "Scripts", "BehaviouralDetection", "GenerateTrainingData", "Functions_TrainingData.R"))
# define the feature settings
desired_window <- 1 # in seconds
sample_rate <- 50
desired_overlap <- 50
source(file = file.path(base_path, "Scripts", "BehaviouralDetection", "GenerateTrainingData", "Features_TrainingData.R"))


## PART FOUR: MAKE THE BEHAVIOURAL MODEL -----------------------------------






