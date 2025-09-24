
# Really quick code to give me back my csv --------------------------------

collar <- "Collar_14"

files <- list.files(file.path(base_path, "RawData", collar, "Board/Chunked"), full.names = TRUE, pattern = ".RDA")
file <- files[14]

load(file) # comes in as accel_data
accel_data_mod <- accel_data %>%
  select(rtc_datetime, RawAX , RawAY, RawAZ) %>%
  rename(X = RawAX,
         Y = RawAY,
         Z = RawAZ,
         Time = rtc_datetime) %>%
  # mutate(Time = as.numeric(Time) / (24*3600) + 719529)
  mutate(Time = row_number())

fwrite(accel_data_mod, file.path(base_path, "RawData", collar, "Board/Chunked", "Day1.csv"))




ggplot(accel_data_mod[1:10000, ], aes(x = Time, y = X)) +
  geom_line()

