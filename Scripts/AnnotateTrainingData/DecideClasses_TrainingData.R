#################
# CleanFeatures_TrainingData

# Overview:
# Combine the classes that cant be differentiated
# And choose the good features

# Requires:
# Labelled feature data

#################

library(randomForest)

# Very basic exploration of what is likely to be confused -----------------
dat <- fread(file.path(base_path, "Data", "LabelledData", "FeatureLabelledData.csv"))
# Drop non-feature columns
drop_cols <- c("Time", "ID")
feat_cols <- setdiff(names(dat), c(drop_cols, "Activity"))

X <- dat[, ..feat_cols]
y <- factor(dat$Activity)

# Remove any columns with NA or zero variance
X <- X[, sapply(X, function(col) !any(is.na(col)) && var(col) > 0), with = FALSE]

# Make an OOB confusion matrix with a basic RF
set.seed(42)
rf <- randomForest(x = X, y = y, ntree = 500, importance = TRUE)

# Caslculate the error rates per class
cm <- rf$confusion
class_error <- cm[, "class.error"] 
cm_counts   <- cm[, -ncol(cm)]
print(sort(class_error))

# Normalise and plot
cm_prop <- sweep(cm_counts, 1, rowSums(cm_counts), "/")
cm_dt <- as.data.table(as.table(cm_prop))
setnames(cm_dt, c("True", "Predicted", "Proportion"))

ggplot(cm_dt, aes(x = Predicted, y = True, fill = Proportion)) +
  geom_tile(colour = "white") +
  geom_text(aes(label = sprintf("%.2f", Proportion)), size = 3) +
  scale_fill_gradient2(low = "white", mid = "steelblue", high = "darkred",
                       midpoint = 0.3, limits = c(0, 1)) +
  labs(x = "Predicted", y = "True class") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Summarise which are and aren't seperable
thresh_well  <- 0.10 # < 10 % error → well separated
thresh_poor  <- 0.40 # > 40 % error → poorly separated

# the good ones
print(names(class_error[class_error < thresh_well]))
# the bad ones
print(names(class_error[class_error > thresh_poor]))

# look at the pairs that are highly confused
confused_pairs <- data.table(
  True      = character(),
  Predicted = character(),
  Prop      = numeric()
)

for (true_class in rownames(cm_prop)) {
  for (pred_class in colnames(cm_prop)) {
    if (true_class != pred_class && cm_prop[true_class, pred_class] > 0.05) {
      confused_pairs <- rbind(confused_pairs, data.table(
        True      = true_class,
        Predicted = pred_class,
        Prop      = round(cm_prop[true_class, pred_class], 3)
      ))
    }
  }
}

print(confused_pairs[order(-Prop)])
