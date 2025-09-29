
# Plot functions ----------------------------------------------------------

# my personal theme
my_theme <- function() {
  theme_minimal(base_size = 12) +
    theme(
      panel.border = element_rect(color = "black", linewidth = 1.5, fill = NA),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      plot.background = element_blank(),
      panel.background = element_blank(),
      axis.line = element_line(color = "black"),
      axis.ticks = element_line(color = "black")
    )
}

# Colours -----------------------------------------------------------------
my_colours = c("coral", "aquamarine3", "plum", "slateblue2", "goldenrod2", 
               "deepskyblue3", "firebrick3", "lightcoral" , "darkcyan", 
               "tomato", "#34623F", "#34435E", "#E56399")


