# Function for testing performance ----------------------------------------

# Plot of confusion matric ------------------------------------------------
generate_confusion_plot <- function(conf_matrix_padded) {
  
  conf_matrix_df <- as.data.frame(as.table(conf_matrix_padded))
  colnames(conf_matrix_df) <- c("Actual", "Predicted", "Count")
  
  conf_matrix_df <- conf_matrix_df %>%
    group_by(Actual) %>%
    mutate(
      Total = sum(Count),
      Percentage = ifelse(Total > 0, Count / Total * 100, 0),
      Label = paste0(Count, " / ", Total)
    ) %>%
    ungroup()
  
  ggplot(
    conf_matrix_df,
    aes(x = Predicted, y = Actual, fill = Percentage)
  ) +
    geom_tile(color = "white", linewidth = 1) +
    geom_text(
      aes(label = Label),
      colour = "black",
      size = 3.5,
      fontface = "bold"
    ) +
    scale_fill_gradient(low = "white", high = "slateblue", limits = c(0, 100), name = "Percentage (%)"
    ) +
    labs(x = "Predicted Class", y = "True Class") +
    coord_fixed() +
    my_theme() +
    theme(
      legend.position = "right",
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.grid = element_blank()
    )

}

compute_metrics <- function(predicted_classes, ground_truth_labels, probabilities = NULL, target_classes = NULL) {
  # Compute confusion matrix
  confusion_matrix <- table(predicted_classes, ground_truth_labels)
  all_classes <- union(levels(predicted_classes), levels(ground_truth_labels))
  
  # Create a padded confusion matrix (in case there are missing classes)
  conf_matrix_padded <- matrix(0, nrow = length(all_classes), ncol = length(all_classes),
                               dimnames = list(all_classes, all_classes))
  conf_matrix_padded[rownames(confusion_matrix), colnames(confusion_matrix)] <- confusion_matrix
  
  confusion_mtx <- caret::confusionMatrix(conf_matrix_padded)
  byClass <- confusion_mtx$byClass
  
  # If only 2 classes, convert named vector to 1-row matrix first
  if (is.null(dim(byClass))) {
    byClass <- t(as.matrix(byClass))
    rownames(byClass) <- colnames(conf_matrix_padded)[2]  # positive class
  }
  
  # threshold-free metrics (has to be one-vs-rest per class) 
  roc_auc <- setNames(rep(NA_real_, length(all_classes)), all_classes)
  pr_auc  <- setNames(rep(NA_real_, length(all_classes)), all_classes)
  
  if (!is.null(probabilities)) {
    probabilities <- as.matrix(probabilities)
    for (cl in intersect(all_classes, colnames(probabilities))) {
      y <- as.character(ground_truth_labels) == cl
      p <- probabilities[, cl]
      
      roc_auc[cl] <- tryCatch(
        as.numeric(pROC::auc(pROC::roc(response = y, predictor = p,
                                       levels = c(FALSE, TRUE), direction = "<",
                                       quiet = TRUE))),
        error = function(e) NA_real_)
      
      pr_auc[cl] <- tryCatch(
        PRROC::pr.curve(scores.class0 = p[y], scores.class1 = p[!y])$auc.integral,
        error = function(e) NA_real_)
    }
  }
  
  # byClass rownames are "Class: X"; strip to match probability column names
  cls_names <- sub("^Class: ", "", rownames(byClass))
  
  metrics <- data.frame(
    Behaviour  = rownames(byClass),
    Precision  = byClass[, "Precision"],
    Recall     = byClass[, "Recall"],
    F1         = byClass[, "F1"],
    Accuracy   = byClass[, "Balanced Accuracy"],
    ROC_AUC    = unname(roc_auc[cls_names]),
    PR_AUC     = unname(pr_auc[cls_names]),
    Prevelance = byClass[, "Prevalence"] * length(predicted_classes),
    row.names  = NULL
  )
  
  # Fill NAs with 0 for threshold metrics only (so the model can't cheat by not
  # predicting a class). AUCs are left as NA if undefined (class absent from test
  # data) and excluded from the macro-average via na.rm.
  thr_cols <- c("Precision", "Recall", "F1", "Accuracy")
  metrics[thr_cols][is.na(metrics[thr_cols])] <- 0
  
  # Macro-average
  all_macro_metrics <- data.frame(
      Behaviour = "Macro-Average",
      Precision = mean(metrics$Precision),
      Recall = mean(metrics$Recall),
      F1 = mean(metrics$F1),
      Accuracy = mean(metrics$Accuracy),
      ROC_AUC = if (all(is.na(metrics$ROC_AUC))) NA_real_ else mean(metrics$ROC_AUC, na.rm = TRUE),
      PR_AUC = if (all(is.na(metrics$PR_AUC)))  NA_real_ else mean(metrics$PR_AUC,  na.rm = TRUE),
      Prevelance = NA
  )
  
  # Weighted by pravalnce
  weighted_macro_metrics <- data.frame(
      Behaviour = "Weighted-Macro-Average",
      Precision = weighted.mean(metrics$Precision, metrics$Prevelance, na.rm = TRUE),
      Recall = weighted.mean(metrics$Recall, metrics$Prevelance, na.rm = TRUE),
      F1 = weighted.mean(metrics$F1, metrics$Prevelance, na.rm = TRUE),
      Accuracy = weighted.mean(metrics$Accuracy, metrics$Prevelance, na.rm = TRUE),
      ROC_AUC  = if (all(is.na(metrics$ROC_AUC))) NA_real_ else
        weighted.mean(metrics$ROC_AUC, metrics$Prevelance, na.rm = TRUE),
      PR_AUC = if (all(is.na(metrics$PR_AUC))) NA_real_ else
        weighted.mean(metrics$PR_AUC, metrics$Prevelance, na.rm = TRUE),
      Prevelance = sum(metrics$Prevelance, na.rm = TRUE)
  )
  
  # targetting the target classes (not weighted because need to be considered equally)
  target_metrics <- metrics %>% 
    mutate(class = gsub("Class: ", "", Behaviour)) %>% 
    dplyr::filter(class %in% target_classes) %>% select(-class)
  target_macro_metrics <- data.frame(
    Behaviour = "Target-Macro-Average",
    Precision = mean(target_metrics$Precision),
    Recall = mean(target_metrics$Recall),
    F1 = mean(target_metrics$F1),
    Accuracy = mean(target_metrics$Accuracy),
    ROC_AUC = if (all(is.na(target_metrics$ROC_AUC))) NA_real_ else mean(target_metrics$ROC_AUC, na.rm = TRUE),
    PR_AUC = if (all(is.na(target_metrics$PR_AUC)))  NA_real_ else mean(target_metrics$PR_AUC,  na.rm = TRUE),
    Prevelance = NA
  )
  
  # all of them together
  final_metrics <- rbind(
    metrics, all_macro_metrics, weighted_macro_metrics, target_macro_metrics
  )
  
  return(list(metrics = final_metrics,
              conf_matrix_padded = conf_matrix_padded))
}


