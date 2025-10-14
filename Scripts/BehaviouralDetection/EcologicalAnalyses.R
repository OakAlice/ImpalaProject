# Making sense of the data ------------------------------------------------
# now that we have calculated things, we can analyse it

# Combine with GPS --------------------------------------------------------
# Take the predicted behaviours and combine them with the GPS

# load in the separate files 
behaviour_data <-  fread(file.path(base_path, "Output", collar, "Behaviours_smoothed.csv"))
energy_data <- fread(file.path(base_path, "Output", collar, "Vdba.csv"))
gps_data <- fread(file.path(base_path, "RawData", collar, "Board", "Board_GPS.csv")) 

# join them based on the nearest time
setDT(behaviour_data)
setDT(energy_data)
setDT(gps_data)

# set key
setkey(gps_data, gps_timestamp)
# rolling join: for each behaviour row, find nearest GPS
data <- gps_data[behaviour_data, on = .(gps_timestamp = Time), roll = "nearest"]

# and now join the VDBA stuff in the same way
data <- data[energy_data, on = .(gps_timestamp = Time), roll = "nearest"]

# select only the columns I want to keep
data <- data %>% select(ID, gps_timestamp, Activity, lon, lat, raw_vedba_mean, vedba_mean, odba_mean) %>%
  rename(Time = gps_timestamp)

fwrite(data, file.path(base_path, "Output", paste0(collar, "_combined_data.csv")))

# Make sense of it --------------------------------------------------------
# this will be up to Senna for her SRP

# make a plot about how the energy changes across day and time
data$day <- day(data$Time)
data$hour <- hour(data$Time)
  
  ggplot(data, aes(x = hour, y = raw_vedba_mean)) +
    geom_point() +
    geom_smooth() +
    my_theme() +
    scale_color_manual(values = my_colours)
  
  # make a tile plot about which behaviour
  data[, time_only := as_hms(format(Time, "%H:%M:00"))]
  data$date_only <- as.Date(data$Time)
  # extract limits (so I know where to plot the annotation)
  x_max <- max(data$time_only, na.rm = TRUE)
  y_max <- max(data$date_only, na.rm = TRUE)
  
  ggplot(data, aes(x = time_only, y = date_only, fill = Activity)) +
    geom_tile() +
    labs(x = "Time of Day", y = "Day of Month") +
    my_theme() +
    scale_fill_manual(values = my_colours) +
    annotate("text",
             x = x_max-4000, y = y_max +0.5,
             label = collar, fontface = "bold")
  
  # make a plot about how their behaviour changes throughout the day
  minute_summary <- data %>%
    group_by(day, hour, Activity) %>%
    summarise(minutes = n())
  
  ggplot(minute_summary, aes(x = hour, y = minutes, colour = Activity))+
    geom_point() +
    geom_smooth() +
    my_theme() +
    scale_color_manual(values = my_colours)

  
  ggplot(data, aes(x = lon, y = lat)) +
    geom_point()
