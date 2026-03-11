# Visualising the behaviours ----------------------------------------------
library(data.table)
library(tidyverse)
library(plotly)

data <- fread("DJI_20240704092742_0046_D_collar2_tagged.csv")


# Plot 1 (highlights) ------------------------------------------------------


# reshape to long format
dat_long <- melt(
  data,
  id.vars = c("time", "eco_behaviour", "mech_behaviour"),
  measure.vars = c("x", "y", "z"),
  variable.name = "axis",
  value.name = "value"
)

# Base plot of accelerometer axes
p <- ggplot(dat_long, aes(x = time, y = value, colour = axis)) +
  geom_line() +
  theme_minimal(base_size = 14) +
  labs(y = "Accel", x = "Time", colour = "Axis")

# Highlight shaded regions based on eco of mech behaviours
eco_p <- p +
  geom_rect(
    data = unique(data[, .(time, eco_behaviour)]),
    aes(xmin = time, xmax = shift(time, type = "lead"),
        ymin = -Inf, ymax = Inf,
        fill = factor(eco_behaviour)),
    inherit.aes = FALSE,
    alpha = 0.2
  ) +
  scale_fill_brewer(palette = "Set3", name = "Eco behaviour")
eco_p

# same for mechanical
mech_p <- p +
  geom_rect(
    data = unique(data[, .(time, mech_behaviour)]),
    aes(xmin = time, xmax = shift(time, type = "lead"),
        ymin = -Inf, ymax = Inf,
        fill = factor(mech_behaviour)),
    inherit.aes = FALSE,
    alpha = 0.2
  ) +
  scale_fill_brewer(palette = "Set3", name = "Mech behaviour")
mech_p

# Plot 2 (grouped) --------------------------------------------------------
library(plotly)
library(purrr)

# Function to create the plot for each behaviour
plot_behaviour <- function(behaviour, data) {
  df <- data %>%
    select(time, x, y, z, mech_behaviour) %>%
    na.omit() %>%
    filter(mech_behaviour == behaviour) %>%
    mutate(relative_time = row_number()) %>%
    ungroup()
  
  # Check if empty
  if (nrow(df) == 0) {
    stop("No data available for behaviour: ", behaviour)
  }
  
  p <- ggplot(df, aes(x = relative_time)) +
    geom_line(aes(y = x, color = "X"), show.legend = FALSE) +
    geom_line(aes(y = y, color = "Y"), show.legend = FALSE) +
    geom_line(aes(y = z, color = "Z"), show.legend = FALSE) +
    labs(title = paste(behaviour),
         x = NULL, y = NULL) +
    scale_color_manual(values = c(X = "salmon", Y = "turquoise", Z = "darkblue"),
                       guide = "none") +
    theme_minimal() +
    theme(panel.grid = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank())
  
  p
  
}

# Wrapper function for multiple behaviours
plotTraceExamples <- function(behaviours, data, n_col) {
  
  # generate plots
  plots <- purrr::map(behaviours, function(behaviour) {
    tryCatch(
      {
        plot_behaviour(behaviour, data)
      },
      error = function(e) {
        message("Skipping plot for ", behaviour, ": ", e$message)
        NULL
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

plots <- plotTraceExamples(behaviours= unique(data$mech_behaviour), data = data, n_col = 2)
plots$grid_plot
