# Feature selection -------------------------------------------------------
# Doing feature selection and exploration to remove redundant features
# Possibly expand this later to select PCA or something

feature_data <- fread(file.path(base_path, "Data", "LabelledData", paste0("FeatureLabelledData.csv")))

var_threshold = 0.1
corr_threshold = 0.8

# remove the features with no variance and high correlation
# Step 1: Calculate variance for numeric columns
numeric_columns <- feature_data[, .SD, .SDcols = setdiff(names(feature_data), c("Activity", "ID", "Time", "fold"))]
variances <- numeric_columns[, lapply(.SD, var, na.rm = TRUE)]
selected_columns <- names(variances)[!is.na(variances) & variances > var_threshold]
  
# Step 2: Remove highly correlated features
numeric_columns <- numeric_columns[, ..selected_columns]
corr_matrix <- cor(numeric_columns, use = "pairwise.complete.obs")
high_corr <- caret::findCorrelation(corr_matrix, cutoff = corr_threshold)
remaining_features <- setdiff(names(numeric_columns), names(numeric_columns)[high_corr])
  
clean_feature_data <- features %>%
  select(c(!!!syms(remaining_features), "Activity", "Time", "ID")) %>% 
  na.omit()

# and save this
fwrite(clean_feature_data, file.path(base_path, "Data", "LabelledData", paste0("CleanFeatureLabelledData.csv")))
