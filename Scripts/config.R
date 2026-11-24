# Config file for the AniML code ------------------------------------------

#install.packaged("pacman")
pacman::p_load(
  tidyverse, 
  data.table,
  caret,
  future,
  future.apply,
  zoo,
  lubridate,
  rlang,
  tsfeatures,
  rBayesianOptimization,
  ranger,
  matrixStats,
  signal,
  patchwork,
  umap
)

available_axes <- c("X", "Y", "Z")
metadata_cols <- c("ID", "Time", "lat", "lon")
species <- "ClementeImpala"
sample_rate <- 50
window_length <- 2
overlap_percent <- 0
set.seed(1000)

sampling_dates <- fread("Notes/Metadata.csv") %>%
  mutate(Start = as.Date(as.character(DeploymentStart), format = "%d.%m.%Y"),
         End = as.Date(as.character(DeploymentEnd), format = "%d.%m.%y")) %>%
  select(CollarNumber, Start, End)
path_to_calinfo <- "Notes/Metadata.csv"

collars <- c("Collar_22", "Collar_23", "Collar_24", "Collar_25")
  # "Collar_11", "Collar_12", "Collar_14", 
  # "Collar_2", "Collar_15", "Collar_3", "Collar_5", "Collar_6" , "Collar_7", "Collar_8")


hpo_bounds <- list(
  mtry = c(2, 20),
  max_depth = c(5, 30),
  number_trees = c(100, 1000)
)

# Functions ---------------------------------------------------------------
source(file = file.path("Scripts", "WranglingData", "reading_artemis_data.R"))

source(file = file.path("Scripts", "BehaviouralDetection", "generate_features.R"))
source(file = file.path("Scripts", "BehaviouralDetection", "preprocessing.R"))
source(file = file.path("Scripts", "BehaviouralDetection", "performance_testing.R"))
source(file = file.path("Scripts", "BehaviouralDetection", "generate_final_model.R"))
source(file = file.path("Scripts", "BehaviouralDetection", "tune_model.R"))
source(file = file.path("Scripts", "BehaviouralDetection", "plots.R"))
source(file = file.path("Scripts", "BehaviouralDetection", "make_predictions.R"))
source(file = file.path("Scripts", "BehaviouralDetection", "postprocessing.R"))
source(file = file.path("Scripts", "BehaviouralDetection", "ecological_plots.R"))

# Plot stuff --------------------------------------------------------------
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

my_colours = c("grey37",
               "coral", 
               "goldenrod2", 
               "khaki2", 
               "aquamarine3", 
               "powderblue", 
               "plum", 
               "lightpink1")

fave_colours <- c(
  "firebrick3", "coral", "goldenrod2", "khaki2", 
  "aquamarine3", "darkcyan", "gold1",
  "springgreen3", "darkseagreen4", "turquoise2", "powderblue", "deepskyblue3", "slateblue2",
  "mediumpurple3", "mediumorchid",
  "plum", "lightpink1", "lavenderblush2")

behaviour_colours <- c(
  "Locomotion_Fast" = "coral",
  "Locomotion_Walk" = "goldenrod2",
  "Foraging_Headup" = "aquamarine3",
  "Foraging_Headdown" = "darkseagreen4",
  "Grooming" = "plum",
  "Stationary" = "deepskyblue3"
)
