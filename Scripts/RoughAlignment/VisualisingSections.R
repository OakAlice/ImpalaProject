# Visualising Clipped Sections ----------------------------------------------

files <- list.files("D:/ImpalaProject/RawData/Collar_2/Axivity/Clipped", pattern = "\\.csv?", full.names = TRUE)
file <- read.csv(files[1], nrows = 1000000)
colnames(file) <- c("V1", "V2", "V3", "V4")


ggplot(file) + 
  geom_line(aes(x = V1, y = V2, colour = "red")) +
  geom_line(aes(x = V1, y = V3, colour = "blue")) +
  geom_line(aes(x = V1, y = V4, colour = "green"))



# Plotting the drift ------------------------------------------------------

files <- list.files("D:/ImpalaProject/RawData/Collar_2/Axivity", pattern = "\\.csv?", full.names = TRUE)
drift_file <- read.csv(files[1], nrows = 1000000)
colnames(drift_file) <- c("V1", "V2", "V3", "V4")
drift_file <- drift_file %>%
  mutate(row = row_number(),
         diff = V1 - lag(V1))


ggplot(drift_file, aes(x = row, y = V1)) + geom_point()
ggplot(drift_file, aes(x = row, y = diff)) + geom_point()
