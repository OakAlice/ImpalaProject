#################
# GenerateFinalModel

# Overview:
# Consolidate the results from the hyperparameter optimisation workflow and 
# create the final model that will be used in the behavioural prediction

# Requires:
# Labelled feature data
# results from the 3 cross-validations

#################


# Get the averaged performance metrics ------------------------------------
results <- list.files(file.path(base_path, "Output", "ClassificationModel"), pattern = "performance_metrics", full.names = TRUE)
performance <- lapply(results, function(x){
  fread(x)
  })
average_performance <- rbindlist(performance) %>%
  as.data.frame() %>%
  mutate(across(everything(), ~replace_na(., 0))) %>%
  group_by(V1) %>%
  summarise(across(where(is.numeric), list(mean = mean, sd = sd)), .groups = "drop") %>%
  rename(Class = V1)
average_performance$Class <- str_split(average_performance$Class, ": ", simplify = T)[,2]

# save them all
fwrite(average_performance, file.path(base_path, "Output", "ClassificationModel", paste0(model_choice, "_averaged_performance.csv")))

# get the single stats
final_performance <- average_performance %>%
  mutate(weighted_contrib = F1_mean * Prevalence_mean)
macro_F1 <- mean(final_performance$F1_mean)
weighted_F1 <- sum(final_performance$weighted_contrib) / sum(final_performance$Prevalence_mean)
micro_F1 <- mean(final_performance$Recall_mean)  # approximation if counts unavailable

cat("Macro F1:   ", round(macro_F1, 3), "\n")
cat("Weighted F1:", round(weighted_F1, 3), "\n")
cat("Micro F1:   ", round(micro_F1, 3), "(approx)\n")

# Confusion matrix --------------------------------------------------------
conffiles <- list.files(file.path(base_path, "Output", "ClassificationModel"), pattern = paste0(model_choice, "_conf_matrix_"), full.names = TRUE)
cm1 <- fread(conffiles[1])
cm2 <- fread(conffiles[2])
cm3 <- fread(conffiles[3])

# Sum across folds then normalise by row (true class) to get recall per class
fix_cm <- function(cm) { # need to get rid of the empty header and make remainder numeric
  m <- as.matrix(cm)
  rownames(m) <- m[, "V1"]
  m <- m[, colnames(m) != "V1"]
  class(m) <- "numeric"
  m
}
# and then if one of them has fewer classes (chance meant didnt appear)
align_cm <- function(cm, all_classes) {
  full <- matrix(0, nrow = length(all_classes), ncol = length(all_classes),
                 dimnames = list(all_classes, all_classes))
  full[rownames(cm), colnames(cm)] <- cm
  full
}
all_classes <- sort(unique(c(rownames(fix_cm(cm1)), rownames(fix_cm(cm2)), rownames(fix_cm(cm3)))))

cm_sum <- align_cm(fix_cm(cm1), all_classes) +
  align_cm(fix_cm(cm2), all_classes) +
  align_cm(fix_cm(cm3), all_classes)
cm_norm <- sweep(cm_sum, 1, rowSums(cm_sum), "/")

# make into a plot
cm_df <- as.data.frame(cm_norm)
cm_df$Predicted <- rownames(cm_df)
cm_long <- melt(setDT(cm_df), id.vars = "Predicted",
                variable.name = "Reference", value.name = "Proportion")

ggplot(cm_long, aes(x = Predicted, y = Reference, fill = Proportion)) +
  geom_tile() +
  geom_text(aes(label = paste0(round(Proportion, 2), "\n(", cm_sum, ")")), size = 3) +
  scale_fill_gradient(low = "white", high = "darkcyan") +
  my_theme() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "True Class", y = "Predicted", fill = "Recall")


# Make the final model ----------------------------------------------------
# Use the hyperparmaters found in the search
hypers <- fread(file.path(base_path, "Output", "ClassificationModel", "RandomForest_hpo_1.csv")) %>%
  slice_max(Value, n = 1)

best_mtry <- round(hypers[["mtry"]],0)
best_number_trees <- round(hypers[["number_trees"]],0)
best_max_depth <- round(hypers[["max_depth"]],0)

# Train an optimal model --------------------------------------------------
# load in all data
data <- fread(file.path(base_path, "Data", "LabelledData", paste0("FeatureLabelledData.csv")))

# train
data <- as.data.table(data)
clean_cols <- removeBadFeatures(data, var_threshold = 0.3, corr_threshold = 0.9)
data <- data %>%
  select(c(!!!syms(clean_cols), "Activity")) %>%
  na.omit() %>%
  mutate(Activity = as.factor(Activity))

# weight by class frequency
class_freq <- table(data$Activity)
class_weights <- 1 / class_freq
class_weights <- class_weights / sum(class_weights)
weight <- class_weights[data$Activity]

RF_model <- ranger(
  dependent.variable.name = "Activity",
  data = data,
  num.trees = best_number_trees,
  mtry = best_mtry,
  max.depth = best_max_depth,
  classification = TRUE,
  probability = TRUE,
  importance = "impurity",
  case.weights = weight
)

# save this mode
saveRDS(RF_model, file.path(base_path, "Output", "ClassificationModel", paste0(model_choice, "_final_model.rds")))
