# Basic plots to set Rachel up for her own analysis\ ----------------------

vdba_data <- fread(file.path(file.path(base_path, "Output", "Summarised_unlabelled_data.csv")))

# make some new variables
vdba_data$time_hour <- hour(vdba_data$Time)



ggplot(vdba_data, aes(x = ID, y = basic_vedba_mean)) +
  geom_boxplot()

ggplot(vdba_data, aes(x = time_hour, y = basic_vedba_mean, colour = ID)) +
  geom_point(position = "jitter") +
  geom_smooth()
