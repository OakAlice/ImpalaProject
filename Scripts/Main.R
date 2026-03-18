# Main file for the analysis of the imapla data -------------------------

# Set up ------------------------------------------------------------------
setwd("C:/Users/PC/Documents/ImpalaProject")
base_path <- getwd()

pacman::p_load(
  tidyverse,
  data.table,
  zoo,
  av,
  stringr,
  shiny,
  patchwork,
  parallel
)

sample_rate <- 50

# meaningful days
sampling_start <- fread("Notes/Metadata.csv") %>%
  mutate(StartDate = as.Date(as.character(ReleaseDate), format = "%d-%b-%y")) %>%
  select(Collar, StartDate)

collars <- list.dirs("Data/RawData", recursive = FALSE) # all the ones we want to do


# Begin by aligning the data ----------------------------------------------
source(file = file.path(base_path, "Scripts", "RoughAlignment", "CustomFunctions.R"))
source(file.path(base_path, "Scripts", "RoughAlignment", "RoughAlignment_Main.R"))
