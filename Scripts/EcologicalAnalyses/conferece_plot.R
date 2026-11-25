# quick and dirty plots for conference and presentations

register_google(key = GGMAP_API_KEY)

behaviour_colours

drink_day <- fread("Output/BehaviouralPredictions/Collar_14/Board_Aligned_2024-07-06_predictions.csv")

# the gps is very messy though, so we are going to remove some of the noise using loess lines
drink_day[, t_num := as.numeric(as.POSIXct(Time))] # firstly, make time numeric
fixes <- drink_day[!is.na(lon) & !is.na(lat)] # extract the valid hits
# fit loess lines (play with the span to find something appropriate)
fit_lon <- loess(lon ~ t_num, data = fixes, span = 0.03, degree = 2,
                 control = loess.control(surface = "direct"))
fit_lat <- loess(lat ~ t_num, data = fixes, span = 0.03, degree = 2,
                 control = loess.control(surface = "direct"))
# predict back into the data to interpolate a hit for every second (so GPS matches the accel frequency)
drink_day[, `:=`(lon_sm = predict(fit_lon, newdata = data.frame(t_num = t_num)),
                 lat_sm = predict(fit_lat, newdata = data.frame(t_num = t_num)))]

# and plot this data again!
gm <- Gmedian(as.matrix(drink_day[, c("lon_sm", "lat_sm")]))   # fast Weiszfeld-type algorithm
map <- get_googlemap(center = c(lon_sm = gm[1], lat_sm = gm[2]), zoom = 15, maptype = "satellite")
ggmap(map) +
  geom_point(data = drink_day, aes(x = lon_sm, y = lat_sm, colour = predicted_class)) +
  labs(x = "Longitude", y = "Latitude") + 
  my_theme() + 
  scale_colour_manual(values = behaviour_colours)


# and then the budget
pred_files <- list.files(file.path("Output", "BehaviouralPredictions", "Collar_14"), full.names = TRUE, pattern = ".csv")
preds <- lapply(pred_files, fread)
preds <- rbindlist(preds)
eco_plots <- generate_ecological_plots(preds, day_number = 1)
  


day_data <- preds %>% dplyr::filter(as.Date(Time) == unique(as.Date(preds$Time))[1]) %>% 
  mutate(Hour = hour(Time)) %>%
  group_by(Hour) %>%
  mutate(Minutes = row_number()/30) %>%
  dplyr::filter(Hour == 6)

day_in_a_life <- ggplot(day_data, aes(x = Minutes, y = Hour, fill = smoothed_class))+
  geom_tile() + 
  scale_fill_manual(values = behaviour_colours) + 
  my_theme()
day_in_a_life
