# Formatting the joey data into a consistent structure --------------------


# Functions ---------------------------------------------------------------
reformat_clemente_data <- function(x){
  dat <- fread(x)
  dat <- dat[, 1:4]
  colnames(dat) <- c("Time", "Accel.X", "Accel.Y", "Accel.Z")
  dat$Time <- as.POSIXct((dat$Time - 719529)*86400, origin = "1970-01-01", tz = "UTC")
  dat$ID <- tools::file_path_sans_ext(basename(x))
  dat
}

# Reformat the data -------------------------------------------------------
unlabelled_files <- list.files(file.path(base_path, "RawData"), pattern = ".csv", recursive = TRUE, full.names = TRUE)

unlabelled_data <- lapply(unlabelled_files, function(x){
  dat <- reformat_clemente_data(x)
  dat
})

unlabelled_data <- rbindlist(unlabelled_data)
fwrite(unlabelled_data, file.path(base_path, "Output", "Unlabelled_data.csv"))
