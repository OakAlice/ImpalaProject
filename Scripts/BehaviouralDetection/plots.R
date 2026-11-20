
# Visualise behavioural clusters ------------------------------------------
# functions and then executions


# Functions ---------------------------------------------------------------
plotTraceExamples <- function(behaviours, data, individuals, n_samples, n_col) {
  
  data <- data %>% dplyr::filter(ID %in% sample(unique(data$ID), individuals))
  
  # Create plots for each behavior (with error catching)
  plots <- purrr::map(behaviours, function(behaviour) {
    tryCatch(
      {
        plot_behaviour(behaviour, n_samples, data)
      },
      error = function(e) {
        message("Skipping plot for ", behaviour, ": ", e$message)
        NULL  # Return NULL to indicate skipping
      }
    )
  })
  
  # Remove NULL plots (for behaviors with no data)
  plots <- purrr::compact(plots)
  
  # Combine plots into a single grid
  grid_plot <- cowplot::plot_grid(plotlist = plots, ncol = n_col)
  
  return(grid_plot = grid_plot)
}

# Function to create the plot for each behavior
plot_behaviour <- function(behaviour, n_samples, data) {
  df <- data %>%
    dplyr::filter(Activity == behaviour) %>%
    group_by(ID, Activity) %>%
    slice(1:n_samples) %>%
    mutate(relative_time = row_number())
  
  # Check if the filtered dataframe is empty
  if (nrow(df) == 0) {
    stop("No data available for behaviour: ", behaviour)
  }
  
  ggplot(df, aes(x = relative_time)) +
    geom_line(aes(y = X, color = "X"), show.legend = FALSE) +
    geom_line(aes(y = Y, color = "Y"), show.legend = FALSE) +
    geom_line(aes(y = Z, color = "Z"), show.legend = FALSE) +
    labs(title = paste(behaviour),
         x = NULL, y = NULL) +
    scale_color_manual(values = c(X = "salmon", Y = "turquoise", Z = "darkblue"), guide = "none") +
    facet_wrap(~ ID, nrow = 1, scales = "free_x") +
    theme_minimal() +
    theme(panel.grid = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank())
}

generate_random_colors <- function(n) {
  colors <- rgb(runif(n), runif(n), runif(n))
  return(colors)
}

plotVolume <- function(data, frequency) {
  summ <- data %>%
    dplyr::count(ID, Activity) %>%
    mutate(minutes = (n/frequency)/60)
  
  vol <- ggplot(summ, aes(x = Activity, y = minutes, fill = as.factor(ID))) +
    geom_bar(stat = "identity") +
    labs(x = "Activity",
         y = "Minutes",
         fill = "ID") +
   my_theme() +
    scale_fill_manual(values = fave_colours) +
    theme(axis.line = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1),
          panel.border = element_rect(color = "black", fill = NA),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())
  
  return(vol)
}

plotUMAP <- function(features, label_column, minimum_distance = 0.1, num_neighbours = 15, shape_metric = "euclidean", spread = 1) {
  # trying to visualise the distribution
  
  clean_cols <- clean_features(features, var_threshold = 0.3, corr_threshold = 0.9)
  features_clean <- features %>%
    select(c(!!!syms(clean_cols), "Activity", "ID")) %>%
    na.omit() %>%
    mutate(Activity = as.factor(Activity), ID = as.factor(ID))
  
  numeric <- features_clean %>% dplyr::select(-c("Activity", "ID"))
  labels <- features_clean%>% dplyr::select("Activity", "ID")
  
  umap_2D <- umap::umap(numeric, 
                        n_neighbors = num_neighbours, 
                        min_dist = minimum_distance, 
                        metric = shape_metric,
                        spread = spread)
  
  # Apply the trained UMAP model on training data
  umap_result_2D <- umap_2D$layout
  
  # Create dataframes for 2D and 3D embeddings, add labels back
  umap_df <- as.data.frame(umap_result_2D)
  colnames(umap_df) <- c("UMAP1", "UMAP2")
  umap_df <- cbind(umap_df, labels)
  
  # Plot the clusters in 2D
  UMAP_2D_plot <- ggplot(umap_df, aes(x = UMAP1, y = UMAP2, colour = Activity, shape = ID)) +
    geom_point(alpha = 0.6, size = 3) +
    my_theme() +
    labs(x = "Dimension 1", y = "Dimension 2", colour = "Activity", shape = "ID") +
    theme(legend.position = "right") +
    annotate("text", x = Inf, y = -Inf, label = paste("n_neighbors:", num_neighbours, "\nmin_dist:", minimum_distance, "\nmetric:", shape_metric),
             hjust = 1.1, vjust = -0.5, size = 3, color = "black", fontface = "italic")+
    scale_color_manual(values = fave_colours) +
    scale_shape_manual(values = c(0,1,2,3,4,5,6,7,8,9))
  
  return(UMAP_2D_plot)
}

