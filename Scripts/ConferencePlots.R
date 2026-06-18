# Making some plots for the conference ------------------------------------


dr_output_dir <- file.path(base_path, "Output", "DeadReckoning", "Collar_8")

days <- c("2024-06-29_Gundogged.csv", "2024-06-30_Gundogged.csv", "2024-07-01_Gundogged.csv", "2024-07-02_Gundogged.csv")
combined_data <- lapply(days, function(x){
  dat <- fread(file.path(dr_output_dir, x))
  dat$day <- str_split(x, "_", simplify = TRUE)[1]
  dat
})
combined_data <- rbindlist(combined_data)


# Gps only plots ----------------------------------------------------------
gps_only <- combined_data[!is.na(combined_data$lon), ]

# single day
ggplot(gps_only %>% dplyr::filter(day == "2024-07-01"), aes(x = lon, y = lat, colour = utc_datetime)) + 
  geom_point() + 
  my_theme()

# all days
ggplot(gps_only, aes(x = lon, y = lat, colour = utc_datetime)) + 
  geom_point() + 
  my_theme() 




# Dead reckoning ----------------------------------------------------------

completed_data_thinned <- combined_data[seq(1, nrow(combined_data), by = 200), ]  # downsample
ggplot(completed_data_thinned, 
       aes(x = DR.longitude, y = DR.latitude, colour = utc_datetime, group = 1)) + 
  geom_path(linewidth = 2, lineend = "round", linejoin = "round",  alpha = 0.5) + 
  my_theme() 
  # scale_colour_gradientn(colours = c("goldenrod2", "coral", "firebrick3"))



# Example of walking behaviour --------------------------------------------
dat <- fread(file.path(base_path, "Data", "LabelledData", "CleanedlLabelledData.csv")) %>% 
  dplyr::filter(ID == "Collar_8",
                Activity == "Locomotion_Walk") %>%
  slice(1:3000)

ggplot(dat, aes(x = seq(1:nrow(dat)))) + 
  geom_path(aes(y = RawAX.cl), linewidth = 1, colour = "darkcyan") + 
  geom_path(aes(y = RawAY.cl), linewidth = 1, colour = "goldenrod2") +
  geom_path(aes(y = RawAZ.cl), linewidth = 1, colour = "aquamarine3")+
  my_theme()
