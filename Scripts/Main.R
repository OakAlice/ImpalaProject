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


# Aligning Data -----------------------------------------------------------
# meaningful days
sampling_start <- fread("Notes/Metadata.csv") %>%
  mutate(StartDate = as.Date(as.character(ReleaseDate), format = "%d-%b-%y")) %>%
  select(CollarNumber, StartDate)

collars <- list.dirs("Data/RawData", recursive = FALSE) # all the ones we want to do

for (Collar in collars){
  # Begin by aligning the data ----------------------------------------------
  source(file = file.path(base_path, "Scripts", "RoughAlignment", "CustomFunctions.R"))
  source(file.path(base_path, "Scripts", "RoughAlignment", "RoughAlignment_Main.R"))
}

# Manually Assessing Alignment / Determining Delay ------------------------
# the reality of working with technology is that some of the clocks drifted
# we need to determine the amount by which it drifted and it is easiest to do this manually
# use the following script to explore and play around with the different files
file <- file.path(base_path, "Scripts", "RoughAlignment", "AccelDelayFinder.R")