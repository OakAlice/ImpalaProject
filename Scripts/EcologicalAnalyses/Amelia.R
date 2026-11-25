# GPS analysis with Amelia ------------------------------------------------
# This is the code I wrote to help the SEP student Amelia (who annotated all the data)
# to get started on her research project.
# all the paths are relative to the USB drive I gave her

# Set up ------------------------------------------------------------------
setwd("E:/Impala")
pacman::p_load(tidyverse,
               data.table,
               ggmap,
               Gmedian,
               geosphere)
# register an API key for extracting maps
register_google(key = GGMAP_API_KEY)

# The first goal of the project was to identify a few individuals worth studying in more detail --------
# We did this by visualising all individuals and picking ones near the dam

# Firstly remove the samples from australia -------------------------------
# because the recording started 9 days before the deployment, there are bad GPS points
# e.g. from driving around the property looing for animals

# here is a file Jasmin recorded all the start dates in
# we use this to set filters
sampling_dates <- fread("Metadata.csv") %>%
  mutate(Start = as.Date(as.character(DeploymentStart), format = "%d.%m.%Y"),
         End = as.Date(as.character(DeploymentEnd), format = "%d.%m.%y"),
         Collar = gsub("Collar_", "", CollarNumber)) %>%
  select(Collar, Start, End)

# this loop will load in the data, filter the dates, format, and store
gps_files <- list.files(file.path("Data/GPS_Data"), full.names = TRUE)
gps_data <- list()
for (file in gps_files){
  data <- fread(file) %>%
    mutate(ID = str_split(basename(file), "_", simplify = TRUE)[2]) %>%
    select(ID, gps_timestamp, lon, lat)
  
  data$Date <- as.Date(data$gps_timestamp)
  
  sampling_start <- sampling_dates$Start[sampling_dates$Collar == str_split(basename(file), "_", simplify = TRUE)[2]]
  sampling_end <- sampling_dates$End[sampling_dates$Collar == str_split(basename(file), "_", simplify = TRUE)[2]]
  
  data <- data %>% 
    dplyr::filter(Date > sampling_start & Date < sampling_end)
  
  gps_data[[file]] <- data
}
gps_data <- rbindlist(gps_data) # read all the individuals back together

# plot each of the individuals into their own plot we can investiagte
maps <- list()
for (id in unique(gps_data$ID)){
  dat <- gps_data %>% dplyr::filter(ID == id)
  
  # firstly, find the central coordinate so the map can be centered for each individual
  gm <- Gmedian(as.matrix(dat[, c("lon", "lat")]))   # fast Weiszfeld-type algorithm
  map <- get_googlemap(center = c(lon = gm[1], lat = gm[2]), zoom = 13, maptype = "satellite")
  
  p <- ggmap(map) +
    geom_point(data = dat, aes(x = lon, y = lat, colour = as.Date(gps_timestamp))) +
    labs(x = "Longitude", y = "Latitude", title = id)
  
  maps[[id]] <- p
}
maps


# Narrow down choice ------------------------------------------------------
# idetified the following individuals visited the dam while collared
dam_ids <- c(5, 6, 15, 14, 12, 10)
dam_data <- gps_data %>% dplyr::filter(ID %in% dam_ids) # extarct this data

# now make a plot of just these individuals using the same code as above
gm <- Gmedian(as.matrix(dam_data[, c("lon", "lat")]))   # fast Weiszfeld-type algorithm
map <- get_googlemap(center = c(lon = gm[1], lat = gm[2]), zoom = 14, maptype = "satellite")
ggmap(map) +
  geom_point(data = dam_data, aes(x = lon, y = lat, colour = ID)) +
  labs(x = "Longitude", y = "Latitude", title = id)

# from this, we chose individual 5 (but use same code for any other individual)
collar_5_data <- gps_data %>% dplyr::filter(ID == 5)
gm <- Gmedian(as.matrix(collar_5_data[, c("lon", "lat")]))   # fast Weiszfeld-type algorithm
map <- get_googlemap(center = c(lon = gm[1], lat = gm[2]), zoom = 15, maptype = "satellite")
ggmap(map) +
  geom_point(data = collar_5_data, aes(x = lon, y = lat, colour = as.factor(as.Date(gps_timestamp)))) +
  labs(x = "Longitude", y = "Latitude")

# and we chose a single day from this: 2024-07-08
collar_5_day_data <- collar_5_data %>% dplyr::filter(as.Date(gps_timestamp) == "2024-07-08")
gm <- Gmedian(as.matrix(collar_5_day_data[, c("lon", "lat")]))   # fast Weiszfeld-type algorithm
map <- get_googlemap(center = c(lon = gm[1], lat = gm[2]), zoom = 15, maptype = "satellite")
ggmap(map) +
  geom_path(data = collar_5_day_data, aes(x = lon, y = lat, colour = gps_timestamp)) +
  labs(x = "Longitude", y = "Latitude") + 
  scale_colour_gradient(low = "#FFD700", high = "red")


# Adding in behavioural analysis ------------------------------------------
# Now that we have had a big overview of the data, we can zoom in to this one ID on the one day
# We are going to use different data now,
# the predictions from the machine learning model

drink_day <- fread("Data/Behaviour_Data/Collar_5/Board_Aligned_2024-07-08_predictions.csv")

# the gps is very messy though, so we are going to remove some of the noise using loess lines
drink_day[, t_num := as.numeric(as.POSIXct(Time))] # firstly, make time numeric
fixes <- drink_day[!is.na(lon) & !is.na(lat)] # extract the valid hits
# fit loess lines (play with the span to find something appropriate)
fit_lon <- loess(lon ~ t_num, data = fixes, span = 0.02, degree = 2,
                 control = loess.control(surface = "direct"))
fit_lat <- loess(lat ~ t_num, data = fixes, span = 0.02, degree = 2,
                 control = loess.control(surface = "direct"))
# predict back into the data to interpolate a hit for every second (so GPS matches the accel frequency)
drink_day[, `:=`(lon_sm = predict(fit_lon, newdata = data.frame(t_num = t_num)),
                 lat_sm = predict(fit_lat, newdata = data.frame(t_num = t_num)))]

# and plot this data again!
ggmap(map) +
  geom_path(data = drink_day, aes(x = lon_sm, y = lat_sm, colour = predicted_class)) +
  geom_point(data = drink_day, aes(x = lon_sm, y = lat_sm, colour = predicted_class)) +
  labs(x = "Longitude", y = "Latitude")
# wow! so exciting!!!!!!!!!!!!!!!!! so HAPPY!!!

# Distances ----------------------------------------------------------
# now that we have this data, we can begin to do some analyses
# for example, lets calculate the distance between each point

# distance metrics # Haversine equation
drink_day <- drink_day %>%
  arrange(Time) %>%
  mutate(distance_m = distHaversine(cbind(lon_sm, lat_sm),
                                     cbind(lag(lon_sm), lag(lat_sm))))

# save this data
drink_day <- drink_day %>% select(ID, Time, lat, lat_sm, lon, lon_sm, predicted_class, smoothed_class, distance_m)
fwrite(drink_day, "Output/Collar_5/Smoothed_2024-07-08.csv")


# now this is everything you need to get started on some super intersting 
# ecological analysis! Thank you for listeneing and learning and being so positive 
# today! Chris and I are SUPER excited for this project :)

# Where to next?
# A good place to start woyuld be repeating this process for another individual
# Somewhere you think would be interesting to compare
# Can you find any other cool events? (Similar to walking to the lake and drinking)
# Then can start figuring out how to remobve the starring effect from the GPS
# (HINT; you can remove the GPS error when the animla is still... the GPS should only
# move when the animal is (which we know based on behavioural predictions))
