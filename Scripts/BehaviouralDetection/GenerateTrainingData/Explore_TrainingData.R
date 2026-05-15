# Cleaning up the training data -------------------------------------------

source(file.path(base_path, "Scripts", 'PlotFunctions.R'))
# Functions ---------------------------------------------------------------
plotTraceExamples <- function(behaviours, data, n_samples, n_col) {
  
  plots <- purrr::map(behaviours, function(behaviour) {
    tryCatch(plot_behaviour(behaviour, n_samples, data),
             error = function(e) { message("Skipping ", behaviour, ": ", e$message); NULL })
  }) %>% purrr::compact()
  
  list(plots = plots, grid_plot = cowplot::plot_grid(plotlist = plots, ncol = n_col))
}

plot_behaviour <- function(behaviour, n_samples, data) {
  df <- data %>%
    dplyr::filter(Activity == behaviour) %>%
    group_by(ID, Activity) %>%
    slice(1:n_samples) %>%
    mutate(relative_time = row_number())
  
  if (nrow(df) == 0) stop("No data available for behaviour: ", behaviour)
  
  ggplot(df, aes(x = relative_time)) +
    geom_line(aes(y = RawAX, color = "RawAX")) +
    geom_line(aes(y = RawAY, color = "RawAY")) +
    geom_line(aes(y = RawAZ, color = "RawAZ")) +
    scale_color_manual(values = c(RawAX = "salmon", RawAY = "turquoise", RawAZ = "darkblue"), guide = "none") +
    labs(title = behaviour, x = NULL, y = NULL) +
    facet_wrap(~ ID, nrow = 1, scales = "free_x") +
    theme_minimal() +
    theme(panel.grid = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank())
}

generate_random_colors <- function(n) rgb(runif(n), runif(n), runif(n))

plotActivityByID <- function(data, frequency) {
  plot_data <- data %>%
    count(ID, Activity) %>%
    filter(Activity != "") %>%
    mutate(minutes = (n / frequency) / 60)
  
  plot <- ggplot(plot_data, aes(x = Activity, y = minutes, fill = as.factor(ID))) +
    geom_bar(stat = "identity") +
    scale_fill_manual(values = generate_random_colors(n_distinct(data$ID))) +
    labs(x = "Activity", y = "minutes") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          panel.border = element_rect(color = "black", fill = NA),
          panel.grid = element_blank())
  
  list(plot = plot, stats = plot_data)
}

# Code --------------------------------------------------------------------
# Visualising the behavioural examples ------------------------------------
# raw_data$Activity <- raw_data$GroupedActivity
raw_data <- na.omit(raw_data)
#raw_data$Activity <- raw_data$GroupedActivity
plotTraceExamples(behaviours = unique(raw_data$Activity), # the behaviours to plot
                  raw_data, 
                  n_samples = 10000, # samples from each ID x Activity to plot
                  n_col = 3)

# Volume ------------------------------------------------------------------
counts <- raw_data %>% group_by(ID, Activity) %>% 
  dplyr::filter(Activity %in% target_behaviours) %>%
  arrange(utc_datetime, .by_group = TRUE) %>% 
  slice(1:20000)

vol <- counts %>% count(ID, Activity)
ggplot(vol, aes(x = Activity, y = n, fill = as.factor(ID))) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = my_colours) +
  my_theme()

