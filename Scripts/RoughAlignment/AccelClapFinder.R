# Interactive plot for finding first clap ---------------------------------
library(tidyverse)
library(plotly)

base_path <- "D:/ImpalaProject"

collar_number <- "Collar_2" # enter here

accel_filename <- list.files(file.path(base_path, "RawData", collar_number, "Axivity"), pattern = "*?.csv", full.names = TRUE)


# load in and modify the data
accel_data <- read.csv(accel_filename, nrows = 10000, header = TRUE)
colnames(accel_data) <- c("Time", "X", "Y", "Z")

# make the plot and inspect manually
create_accel_plot(accel_data)

# enter the number of the row with the claps
x <- 416
accel_data$Time[x]

time <- as.POSIXct((accel_data$Time[x] - 719529)*86400, origin = "1970-01-01", tz = "Africa/Johannesburg")


# Function to create zoomable/interactive plot to see the claps
create_accel_plot <- function(accel_data) {
  
  plot <- plot_ly() %>%
    # Add traces for each axis
    add_trace(data = accel_data, 
              x = ~Time, 
              y = ~X, 
              name = "X",
              type = "scatter",
              mode = "lines", 
              line = list(colour = '#4f5aad'),
              hovertemplate = paste("Time: %{x}<br>",
                                    "X: %{y}<br>",
                                    "Row: %{customdata}<br>",
                                    "<extra></extra>"),
              customdata = seq_len(nrow(accel_data))) %>%
    
    add_trace(data = accel_data, 
              x = ~Time, 
              y = ~Y,
              name = "Y",
              type = "scatter",
              mode = "lines",
              line = list(colour = '#579157'),
              hovertemplate = paste("Time: %{x}<br>",
                                    "Y: %{y}<br>",
                                    "Row: %{customdata}<br>",
                                    "<extra></extra>"),
              customdata = seq_len(nrow(accel_data))) %>%
    
    add_trace(data = accel_data, 
              x = ~Time, 
              y = ~Z,
              name = "Z",
              type = "scatter",
              mode = "lines",
              line = list(colour = 'goldenrod'),
              hovertemplate = paste("Time: %{x}<br>",
                                    "Z: %{y}<br>",
                                    "Row: %{customdata}<br>",
                                    "<extra></extra>"),
              customdata = seq_len(nrow(accel_data))) %>%
    
    # Layout configuration
    layout(xaxis = list(title = "Time",
                        rangeslider = list(visible = TRUE)),
           yaxis = list(title = "Acceleration"),
           hovermode = "closest")
  
  # Add yellow background shading for flagged sections if Flags column exists
  if ("Flags" %in% names(accel_data)) {
    # Find ranges where Flags is not NA or blank
    flag_indices <- which(!is.na(accel_data$Flags) & accel_data$Flags != "")
    if (length(flag_indices) > 0) {
      # Create ranges for continuous sections
      ranges <- split(flag_indices, cumsum(c(1, diff(flag_indices) != 1)))
      
      # Add shapes for each range
      for (range in ranges) {
        plot <- plot %>%
          layout(shapes = list(
            list(
              type = "rect",
              fillcolor = "yellow",
              opacity = 0.3,
              line = list(width = 0),
              x0 = accel_data$Time[min(range)],
              x1 = accel_data$Time[max(range)],
              y0 = -Inf,
              y1 = Inf,
              layer = "below"
            )
          ))
      }
    }
  }
  
  return(plot)
}

