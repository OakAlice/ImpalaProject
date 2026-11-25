# Making some plots for the conference ------------------------------------


dr_output_dir <- file.path(base_path, "Output", "DeadReckoning", "Collar_8")

days <- c("2024-06-30_Gundogged.csv", "2024-07-01_Gundogged.csv", "2024-07-02_Gundogged.csv", "2024-07-03_Gundogged.csv",  "2024-07-04_Gundogged.csv")
combined_data <- lapply(days, function(x){
  dat <- fread(file.path(dr_output_dir, x))
  dat$day <- str_split(x, "_", simplify = TRUE)[1]
  dat
})
combined_data <- rbindlist(combined_data)

# Gps only plots ----------------------------------------------------------
gps_only <- combined_data[!is.na(combined_data$lon), ]

# single day
ggplot(gps_only, aes(x = lon, y = lat)) + 
  geom_point(size = 3, colour = "goldenrod") + 
  my_theme()

# Dead reckoning ----------------------------------------------------------
library(viridis)
completed_data_thinned <- combined_data[seq(1, nrow(combined_data), by = 1000), ]  # downsample
completed_data_thinned <- completed_data_thinned[completed_data_thinned$utc_datetime < as.POSIXct("2024-07-02 23:40:00", tz = "UTC")]


ggplot(completed_data_thinned, 
       aes(x = DR.longitude.corr, y = DR.latitude.corr, colour = Activity, group = 1)) + 
  geom_path(linewidth = 2, lineend = "round", linejoin = "round",  alpha = 0.5) + 
  scale_colour_viridis_d(option = "plasma") + 
  my_theme() 
  # scale_colour_gradientn(colours = c("goldenrod2", "coral", "firebrick3"))

# and with satellite
library(ggmap)
register_google(key = GGMAP_API_KEY)
bbox <- c(
  left   = min(completed_data_thinned$DR.longitude.corr, na.rm = TRUE) - 0.01,
  bottom = min(completed_data_thinned$DR.latitude.corr, na.rm = TRUE) - 0.01,
  right  = max(completed_data_thinned$DR.longitude.corr, na.rm = TRUE) + 0.01,
  top    = max(completed_data_thinned$DR.latitude.corr, na.rm = TRUE) + 0.01
)
centre <- c(
  lon = mean(c(bbox["left"], bbox["right"])),
  lat = mean(c(bbox["bottom"], bbox["top"]))
)
satellite_map <- get_googlemap(
  center = centre,
  zoom = 15,
  maptype = "satellite"
)
ggmap(satellite_map) +
  geom_path(
    data = completed_data_thinned,
    aes(x = DR.longitude.corr, y = DR.latitude.corr, colour = Activity, group = 1),
    linewidth = 2, lineend = "round", linejoin = "round", alpha = 0.5
  ) +
  scale_colour_viridis_d(option = "plasma") +
  my_theme()



# VDBA plot
completed_data_thinned$hour <- hour(completed_data_thinned$utc_datetime)
ggplot(completed_data_thinned, aes(x = hour, y = VDBA.sm)) + 
  geom_smooth(colour = "lightcoral", fill = "goldenrod") +
  my_theme()


# activity proportions
ggplot(completed_data_thinned, aes(x = hour, fill = Activity)) + 
  geom_bar(position = "fill") +
  scale_fill_viridis_d(option = "plasma") +
  labs(y = "Proportion of behaviour") +
  my_theme()





# Just the run ------------------------------------------------------------
run_data <- completed_data_thinned %>% dplyr::filter(day == "2024-06-30",
                                                     utc_datetime > as.POSIXct("2024-06-30 10:40:00", tz = "UTC"),
                                                     utc_datetime < as.POSIXct("2024-06-30 11:40:00", tz = "UTC"))
ggplot(run_data, aes(x = DR.longitude.corr, y = DR.latitude.corr, colour = utc_datetime)) + 
  geom_point(size = 3) + 
  my_theme()

bbox <- c(
  left   = min(run_data$DR.longitude.corr, na.rm = TRUE) - 0.01,
  bottom = min(run_data$DR.latitude.corr, na.rm = TRUE) - 0.01,
  right  = max(run_data$DR.longitude.corr, na.rm = TRUE) + 0.01,
  top    = max(run_data$DR.latitude.corr, na.rm = TRUE) + 0.01
)
centre <- c(
  lon = mean(c(bbox["left"], bbox["right"])),
  lat = mean(c(bbox["bottom"], bbox["top"]))
)
satellite_map <- get_googlemap(
  center = centre,
  zoom = 16,
  maptype = "satellite"
)
ggmap(satellite_map) +
  geom_path(
    data = run_data,
    aes(x = DR.longitude.corr, y = DR.latitude.corr, colour = Activity, group = 1),
    linewidth = 2, lineend = "round", linejoin = "round", alpha = 0.5
  ) +
  scale_colour_viridis_d(option = "plasma") +
  my_theme()

# get the speed
run_data[, `:=`(
  dist_m = c(NA, distHaversine(
    cbind(DR.longitude.corr[-.N], DR.latitude.corr[-.N]),
    cbind(DR.longitude.corr[-1], DR.latitude.corr[-1])
  )),
  dt_sec = c(NA, as.numeric(diff(utc_datetime), units = "secs"))
)]
run_data[, speed_mps := dist_m / dt_sec]
run_data[, speed_kmh := speed_mps * 3.6]

run_data %>%
  summarise(
    mean_speed_kmh = mean(speed_kmh, na.rm = TRUE),
    median_speed_kmh = median(speed_kmh, na.rm = TRUE),
    sd_speed_kmh = sd(speed_kmh, na.rm = TRUE),
    min_speed_kmh = min(speed_kmh, na.rm = TRUE),
    max_speed_kmh = max(speed_kmh, na.rm = TRUE),
    q95_speed_kmh = quantile(speed_kmh, 0.95, na.rm = TRUE)
  )

ggplot(run_data, aes(x = speed_kmh)) +.000
  geom_histogram(aes(fill = ..x..), bins = 50) +
  scale_fill_viridis_c(option = "plasma") +
  labs(x = "Speed (km/h)", y = "Count") +
  my_theme()


ggplot(run_data, aes(x = DR.longitude.corr, y = DR.latitude.corr, colour = speed_kmh)) +
  geom_path(linewidth = 2) + 
  scale_colour_viridis_c(option = "plasma") +
  my_theme()



ggplot(run_data, aes(x = Activity, y = speed_kmh, fill = Activity)) +
  geom_boxplot(outlier.alpha = 0.3) +
  scale_fill_viridis_d(option = "plasma") +
  labs(y = "Speed (km/h)", x = NULL) +
  my_theme() +
  theme(legend.position = "none")






# Plot a sample of the behaviours -----------------------------------------
plot_dat <- combined_data[60000:105000,]
ggplot(plot_dat, aes(x = utc_datetime, y = 1, fill = Activity)) + 
  geom_tile(height = 1) +
  scale_fill_viridis_d(option = "plasma") +
  my_theme()


ggplot(correcteddata[10000:15000,], aes(x = utc_datetime, y = Yaw)) + geom_path(linewidth = 2) + my_theme()




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



plt_dat <- dat %>% group_by(Activity) %>% slice(1:1000)
ggplot(plt_dat, aes(x = seq(1:nrow(plt_dat)))) + 
  geom_path(aes(y = RawAX.cl), linewidth = 1, colour = "darkcyan") + 
  geom_path(aes(y = RawAY.cl), linewidth = 1, colour = "goldenrod2") +
  geom_path(aes(y = RawAZ.cl), linewidth = 1, colour = "aquamarine3")+
  my_theme()
