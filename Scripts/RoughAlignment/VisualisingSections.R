# Visualising Clipped Sections ----------------------------------------------
# looked weird in the gui and need to check


files <- list.files("D:/ImpalaProject/RawData/Collar_2/Axivity", pattern = "\\.csv?", full.names = TRUE)
file <- read.csv(files[1], nrows = 1000000)
colnames(file) <- c("V1", "V2", "V3", "V4")


ggplot(file) + 
  geom_line(aes(x = V1, y = V2, colour = "red")) +
  geom_line(aes(x = V1, y = V3, colour = "blue")) +
  geom_line(aes(x = V1, y = V4, colour = "green"))
