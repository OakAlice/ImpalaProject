# Making sense of the data ------------------------------------------------
# now that we have calculated things, we can analyse it

# Combine with GPS --------------------------------------------------------
# Take the predicted behaviours and combine them with the GPS

# load in the separate files 
behaviour_data <-  fread(file.path(base_path, "Output", paste0(collar, "_behaviours_smoothed.csv")))
energy_data <- energy_data_file_path
gps_data <- fread(file.path(base_path, "RawData", collar, "Board", "Board_GPS.csv")) 

# join them based on the nearest time
setDT(behaviour_data)
setDT(gps_data)

# set key on gps_data timestamp
setkey(gps_data, gps_timestamp)

# join behaviour_data$Time to nearest gps_data$gps_timestamp
data <- gps_data[behaviour_data, on = .(gps_timestamp = Time), roll = "nearest"]

fwrite(data, file.path(base_path, "Output", paste0(collar, "_behaviour_GPS_data.csv")))

# Make sense of it --------------------------------------------------------
# this will be up to Senna for her SRP

# make a plot about how the energy changes across day and time
data$day <- day(data$Time)
data$hour <- hour(data$Time)
  
  ggplot(data, aes(x = hour, y = maxVDBA, colour = as.factor(day))) +
    geom_point() +
    geom_smooth() +
    my_theme() +
    scale_color_manual(values = my_colours)
  
  # make a tile plot about which behaviour
  data$time_only <- as_hms(data$Time)
  data$date_only <- as.Date(data$Time)
  # extract limits (so I know where to plot the annotation)
  x_max <- max(data$time_only, na.rm = TRUE)
  y_max <- max(data$date_only, na.rm = TRUE)
  
  ggplot(data, aes(x = time_only, y = date_only, fill = Prediction)) +
    geom_tile() +
    labs(x = "Time of Day", y = "Day of Month") +
    my_theme() +
    scale_fill_manual(values = my_colours) +
    annotate("text",
             x = x_max-4000, y = y_max +0.5,
             label = name, fontface = "bold")
  
  # make a plot about how their behaviour changes throughout the day
  minute_summary <- data %>%
    group_by(day, hour, Prediction) %>%
    summarise(minutes = n())
  
  ggplot(minute_summary, aes(x = hour, y = minutes, colour = Prediction))+
    geom_point() +
    geom_smooth() +
    my_theme() +
    scale_color_manual(values = my_colours)
  
})
