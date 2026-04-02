# Basic formatting -------------------------------------------------------
# I just stitch the data together ----------------------------------------
files <- list.files(file.path(base_path, "Data", "LabelledData", "MatLab"), recursive = TRUE, full.names = TRUE)
raw_data <- lapply(files, function(file) {
  df <- fread(file)
  individual <- basename(dirname(file))
  df$ID <- individual
  
  df <- df %>%
    select(-func_behaviour) %>%
    mutate(time = as.POSIXct((time - 719529)*86400, origin = "1970-01-01", tz = "UTC")) %>%
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
  rename(mech_behaviour = Num,
         MechActivity = Activity)
eco_labels <- fread(file.path(base_path, "Data/Ecological Behaviours.csv")) %>%
  rename(eco_behaviour = Num,
         EcoActivity = Activity)

raw_data <- left_join(raw_data, mech_labels, by = "mech_behaviour")
raw_data <- left_join(raw_data, eco_labels, by = "eco_behaviour")

# clean it up
raw_data <- raw_data %>% 
  select(-c(eco_behaviour, mech_behaviour)) %>%
  filter(!(is.na(MechActivity) & is.na(EcoActivity)))

# I happen to know that 2 of the individuals were incorrectly scaled so change here...
raw_data <- raw_data %>%
  mutate(X = ifelse(ID %in% c("Collar_3", "Collar_12"), X * 2048, X),
         Y = ifelse(ID %in% c("Collar_3", "Collar_12"), Y * 2048, Y),
         Z = ifelse(ID %in% c("Collar_3", "Collar_12"), Z * 2048, Z))

fwrite(raw_data, file.path(base_path, "Data", "LabelledData", "OriginalLabelledData.csv"))

