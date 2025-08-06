# Visulaisation code ------------------------------------------------------
# Choosing the file to read -----------------------------------------------
base_path <- "C:/Users/PC/Documents/ImpalaProject/RawData"
ID <- "Collar_3"





accel_snip <- board_accel[seq(1, min(1000000000, nrow(board_accel)), by = 100), ]

#accel_snip <- accel_snip[RawAX>(-10000), ]


ggplot(accel_snip) + 
  geom_line(aes(x = internal_timestamp, y = RawAX), colour = "goldenrod") # +












# 2. After I've modified it
board_accel_modified <- read.csv(file.path(base_path, ID, "Synced_Board_Accel.csv"), nrows = 10000)


# Basic plot --------------------------------------------------------------

ggplot(accel_snip) + 
  geom_line(aes(x = internal_timestamp, y = RawAX), colour = "goldenrod") # +
  # geom_line(aes(x = internal_timestamp, y = RawAY),  colour = "cornflowerblue") +
  # geom_line(aes(x = internal_timestamp, y = RawAZ), colour = "seagreen")

ggplot(board_accel_modified) + 
  geom_line(aes(x = adjusted_timestamp, y = RawAX), colour = "goldenrod") # +
# geom_line(aes(x = adjusted_timestamp, y = RawAY),  colour = "cornflowerblue") +
# geom_line(aes(x = adjusted_timestamp, y = RawAZ), colour = "seagreen")


# Plotting the drift ------------------------------------------------------

files <- list.files("D:/ImpalaProject/RawData/Collar_2/Axivity", pattern = "\\.csv?", full.names = TRUE)
drift_file <- read.csv(files[1], nrows = 1000000)
colnames(drift_file) <- c("V1", "V2", "V3", "V4")
drift_file <- drift_file %>%
  mutate(row = row_number(),
         diff = V1 - lag(V1))


ggplot(drift_file, aes(x = row, y = V1)) + geom_point()
ggplot(drift_file, aes(x = row, y = diff)) + geom_point()
