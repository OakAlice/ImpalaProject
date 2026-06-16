#################
# Main_TrainingPostProcessing

# Overview:
# Develop a post-processing protocol to apply to the predictions
# Using Bayesian smoothing because my previous paper found that top be optimal on average

# Requires:
# The predictions made by the model in the previous model tuning and performance 
# estimation phase

#################

smoothing_method = "HMM" # options: "Bayesian", "HMM", "Transition", "Mode"

source(file = file.path(base_path, "Scripts", "BehaviouralDetection", "ModelDesign", "Functions_PostProcessing.R"))

## Get the transition matrix from the training data -----------------------
train_data <- fread(file.path(base_path, "Data", "LabelledData", "FeatureLabelledData.csv")) %>%
  arrange(ID, Time)
train_data <- identify_sequences(data = train_data, max_break = 2) # consider it a break if more than 2 seconds

transitions <- generate_transition_probabilities(train_data)
transition_matrix <- transitions$transition_matrix
states <- unique(train_data$Activity)

# Postprocess on each of the data segments --------------------------------
for (i in 1:3){

  test_data <- fread(file.path(base_path, "Output", "ClassificationModel", paste0(model_choice, "_test_predictions_", i, ".csv"))) %>%
    as.data.frame() %>%
    arrange(ID, Time)
  # split into sequences
  test_data <- identify_sequences(test_data, max_break = 2)
  test_data$set <- paste(test_data$ID, test_data$sequence, sep = "_")
  
  if (smoothing_method == "Bayesian"){
    test_data <- lapply(unique(test_data$set), function(x){
      dat <- test_data %>% dplyr::filter(set == x)
      if (nrow(dat) < 3) {
        dat$smoothed_class <- dat$predicted_class
        return(dat)
      }
      dat$smoothed_class <- apply_bayes_smoothing(dat, states, transition_matrix)
    
      dat
    })
    test_data <- rbindlist(test_data)
    
  } else if (smoothing_method == "HMM"){
    # Extract parameters from the training data
    train_data <- fread(file.path(base_path, "Output", "ClassificationModel", paste0(model_choice, "_training_predictions_", i, ".csv"))) %>%
      na.omit()
    train_data <- identify_sequences(train_data, max_break = 2)
    
    # train a model
    hmm_model <- make_hmm_model(train_data)
    
    # run the hmm over each of the sequences and save the smoothed class
    test_data <- lapply(unique(test_data$set), function(x){
      dat <- test_data %>% dplyr::filter(set == x)
      if (nrow(dat) < 2) {
        dat$smoothed_class <- dat$predicted_class
        return(dat)
      }
      dat$smoothed_class <- viterbi(hmm_model, as.character(dat$predicted_class))
      
      dat
    })
    test_data <- rbindlist(test_data)
    
  } else if (smoothing_method == "Transition"){
    test_data <- find_suspect_transitions(test_data, transition_probs_melted)
    test_data <- update_suspect_transitions(test_data, transition_probs_melted)
    
  } else if (smoothing_method == "Mode"){
    test_data <- rolling_mode_smooth(test_data, x = 5)
  }
  
  # Recalculate performance and save ----------------------------------------
  performance <- calculate_performance(as.factor(test_data$smoothed_class), as.factor(test_data$true_class))
  fwrite(performance$confusion_mtx$byClass, file.path(base_path, "Output", "ClassificationModel", paste0(smoothing_method, "_performance_", i, ".csv")), row.names = TRUE)
  
  # and now we save the confusion matrix to be plotted
  write.csv(performance$confusion_mtx$table, file = file.path(base_path, "Output", "ClassificationModel", paste0(smoothing_method, "_conf_matrix_", i, ".csv")), row.names = TRUE)
}

# Read the results back in to assess improvement --------------------------
smoothedfiles <- list.files(file.path(base_path, "Output", "ClassificationModel"), pattern = paste0(smoothing_method, "_performance_"), full.names = TRUE)
smoothedperformance <- lapply(smoothedfiles, function(x){
  fread(x)
})
average_performance <- rbindlist(smoothedperformance) %>%
  as.data.frame() %>%
  mutate(across(everything(), ~replace_na(., 0))) %>%
  group_by(V1) %>%
  summarise(across(where(is.numeric), list(mean = mean, sd = sd)), .groups = "drop") %>%
  rename(Class = V1)
average_performance$Class <- str_split(average_performance$Class, ": ", simplify = T)[,2]

# save them all
fwrite(average_performance, file.path(base_path, "Output", "ClassificationModel", paste0(smoothing_method, "_averaged_performance.csv")))

# get the single stats ----------------------------------------------------
final_performance <- average_performance %>%
  mutate(weighted_contrib = F1_mean * Prevalence_mean)
macro_F1 <- mean(final_performance$F1_mean)
weighted_F1 <- sum(final_performance$weighted_contrib) / sum(final_performance$Prevalence_mean)
micro_F1 <- mean(final_performance$Recall_mean)  # approximation if counts unavailable

cat("Macro F1:   ", round(macro_F1, 3), "\n")
cat("Weighted F1:", round(weighted_F1, 3), "\n")
cat("Micro F1:   ", round(micro_F1, 3), "(approx)\n")

# Make the confusion matrix plot ------------------------------------------
conffiles <- list.files(file.path(base_path, "Output", "ClassificationModel"), pattern = paste0(smoothing_method, "_conf_matrix_"), full.names = TRUE)

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
cm_sum <- fix_cm(cm1) + fix_cm(cm2) + fix_cm(cm3)
cm_norm <- sweep(cm_sum, 1, rowSums(cm_sum), "/")

# make into a plot
cm_df <- as.data.frame(cm_norm)
cm_df$Predicted <- rownames(cm_df)
cm_long <- melt(setDT(cm_df), id.vars = "Predicted",
                variable.name = "Reference", value.name = "Proportion")

ggplot(cm_long, aes(x = Predicted, y = Reference, fill = Proportion)) +
  geom_tile() +
  geom_text(aes(label = paste0(round(Proportion, 2), "\n(", cm_sum, ")")), size = 2.5) +
  scale_fill_gradient(low = "white", high = "steelblue") +
  my_theme() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Predicted", y = "TrueClass", fill = "Recall")


