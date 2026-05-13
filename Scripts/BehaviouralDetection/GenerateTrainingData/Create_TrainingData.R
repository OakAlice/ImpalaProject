# Basic formatting -------------------------------------------------------
# I just stitch the data together ----------------------------------------
files <- list.files(file.path(base_path, "Data", "LabelledData", "MatLab"), recursive = TRUE, full.names = TRUE)
raw_data <- lapply(files, function(file) {
  df <- fread(file)
  individual <- basename(dirname(file))
  df$ID <- individual
  
  df <- df %>%
    select(-func_behaviour, -eco_behaviour) %>%
    rename(Time = time,
           X = x,
           Y = y,
           Z = z
           ) 
  return(df)
})
raw_data <- bind_rows(raw_data)

# stitch the true behaviours from the key -------------------------------
mech_labels <- fread(file.path(base_path, "Data/Functional Behaviours.csv")) %>%
  rename(mech_behaviour = Num)
raw_data <- left_join(raw_data, mech_labels, by = "mech_behaviour") %>%
  rename(Number = "mech_behaviour")

# I happen to know that 2 of the individuals were incorrectly scaled so change here...
raw_data <- raw_data %>%
  mutate(X = ifelse(ID %in% c("Collar_3", "Collar_12"), X * 2048, X),
         Y = ifelse(ID %in% c("Collar_3", "Collar_12"), Y * 2048, Y),
         Z = ifelse(ID %in% c("Collar_3", "Collar_12"), Z * 2048, Z)) %>%
  mutate(X = ifelse(ID %in% c("Collar_14"), X / 8192, X),
         Y = ifelse(ID %in% c("Collar_14"), Y / 8192, Y),
         Z = ifelse(ID %in% c("Collar_14"), Z / 8192, Z))



# combine the individual behaviours into groups (based on the Explore_TrainingData.R file)
# use this script to play around with the labels 
rstudioapi::navigateToFile(file = file.path(base_path, "Scripts", "BehaviouralDetection", "GenerateTrainingData", "Explore_TrainingData.R"))
# read in the behaviour conversion key
if(file.exists(file.path(base_path, "Notes", "BehaviourConversionKey.csv"))){
  key <- fread(file.path(base_path, "Notes", "BehaviourConversionKey.csv"))
  
  raw_data <- left_join(raw_data, key, by = "Activity") %>%
    select(-c(Why))
} 

fwrite(raw_data, file.path(base_path, "Data", "LabelledData", "OriginalLabelledData.csv"))

# Now split it out into Activities and save each independently
save_split_dir <- file.path(base_path, "Data", "LabelledData", "Split")
if(!dir.exists(save_split_dir)){dir.create(save_split_dir)}
data_list <- split(raw_data, raw_data$GroupedActivity)
lapply(names(data_list), function(nm) {
  fwrite(data_list[[nm]], file.path(save_split_dir, paste0(nm, ".csv")), row.names = FALSE)
})

# Now go back into Matlab, clean and check the data. Remove anything that looks like an error.
