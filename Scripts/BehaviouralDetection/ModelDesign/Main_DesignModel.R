#################
# Main_DesignModel

# Overview:
# Create an accelerometer-based animal behaviour classification machine learning model
# Tune hyperparameters, validate an optimal model design, train the final model

# Requires:
# Labelled feature data

# Note:
# Starting with an XGBoost design but then will possibly trial an NN design

#################

# Source functions --------------------------------------------------------
source(file = file.path(base_path, "Scripts", "BehaviouralDetection", "ModelBuilding", "Functions_HPO.R"))
source(file = file.path(base_path, "Scripts", "BehaviouralDetection", "ModelBuilding", "Functions_TuneTrainTestXGBoost.R"))

# Prep the data -----------------------------------------------------------
data <- fread(file.path(base_path, "Data", "LabelledData", paste0("FeatureLabelledData.csv")))

# any changes to the classes that need to be made
data <- data %>%
  mutate(Activity = ifelse(Activity == "Foraging_Headlevel", "Walking", Activity)) %>%
  mutate(Activity = ifelse(Activity == "Vibrating", "Stationary_Vigilance", Activity))

# split the data into groups
unique_IDs <- unique(data$ID)
ID_groups <- data.frame(
  ID = unique_IDs,
  group = sample(rep(1:3, length.out = length(unique_IDs)))
)

# Tune and train and test the model 3 times -------------------------------
for (i in 1:3){
  print(i)
  # define the test IDs for this round
  test_IDs <- ID_groups$ID[ID_groups$group == i]
  
  # Make the Model ----------------------------------------------------------
  # options for RandomForst, XGBoost, or CNN
  source(file = file.path(base_path, "Scripts", "BehaviouralDetection", "ModelDesign", "BuildSingleModel.R"))
  # individual output saved rather than directly averaged to allow post-processing experiments
}

# Train the final model ---------------------------------------------------
# Take the average of the performance of the previous models and generate a final model
# This will be what's used on the unlabelled data
#TODO: Find better way of deciding on parameters rather than averaging
source(file = file.path(base_path, "Scripts", "ModelBuilding", "GenerateFinalModel.R"))

# Apply to the unlabelled data --------------------------------------------
# this will be different for every set up... to be determined based on the data setup