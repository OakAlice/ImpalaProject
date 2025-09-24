# Data Visualisation ------------------------------------------------------

base_path <- "here"
p_load(tidyverse,
       data.table)

# Functions ---------------------------------------------------------------
plotTraceExamples <- function(behaviours, data, individuals, n_samples, n_col) {
  
  data <- data %>% filter(ID %in% sample(unique(data$ID), individuals))
  
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
  
  return(list(plots = plots, 
              grid_plot = grid_plot))
}

# Function to create the plot for each behavior
plot_behaviour <- function(behaviour, n_samples, data) {
  df <- data %>%
    filter(Activity == behaviour) %>%
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

plotActivityByID <- function(data, frequency) {
  my_colours <- generate_random_colors(length(unique(data$ID)))
  # summarise into a table
  labelledDataSummary <- data %>%
    #filter(!Activity %in% ignore_behaviours) %>%
    count(ID, Activity) %>%
    filter(!Activity == "")
  
  # account for the HZ, convert to minutes
  labelledDataSummaryplot <- labelledDataSummary %>%
    mutate(minutes = (n/frequency)/60)
  
  # Plot the stacked bar graph
  plot_activity_by_ID <- ggplot(labelledDataSummaryplot, aes(x = Activity, y = minutes, fill = as.factor(ID))) +
    geom_bar(stat = "identity") +
    labs(x = "Activity",
         y = "minutes") +
    theme_minimal() +
    scale_fill_manual(values = my_colours) +
    theme(axis.line = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1),
          panel.border = element_rect(color = "black", fill = NA),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())
  
  return(list(plot = plot_activity_by_ID,
              stats = labelledDataSummaryplot))
}


# Run the script ----------------------------------------------------------
 # just stitch the data together
  files <- list.files(file.path(base_path, "LabelledData"), recursive = TRUE, full.names = TRUE)
  raw_data <- lapply(files, function(file) {
    df <- fread(file)
    filename <- tools::file_path_sans_ext(basename(file))
    num <- sub("^([0-9]{1,2}).*", "\\1", filename)
    df$ID <- num
    
    df <- df %>%
      select(-func_behaviour)
    
    return(df)
  }) 
  raw_data <- bind_rows(raw_data)
  
  # and thenI select the columns I want and give them the names that match my workflow
  raw_data <- raw_data %>%
    rename(X = x,
           Y = y,
           Z = z,
           Time = time
    )

data$Activity <- data$mech_behaviour

behaviours <- unique(data$Activity)
individuals <- length(unique(data$ID))
n_samples <- 500
n_col <- 3

plotTraceExamples(behaviours, data, individuals, n_samples, n_col)
