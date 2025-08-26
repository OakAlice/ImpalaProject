# Combining all artemis board files ---------------------------------------
# adapted from Chris Clemente -> https://github.com/cclemente/Collar_data_extraction/blob/main/Step1a_Read_in_raw_accel_files.R

# List all matching files
accel_files <- list.files(
  path = accel_dir,
  pattern = "^dataLog\\d+\\.TXT$",  # matches dataLog00000.TXT etc.
  full.names = TRUE
)

# order it
file_nums <- as.integer(gsub("\\D", "", basename(accel_files)))
accel_files <- accel_files[order(file_nums)]

bad_files <- c()  # store skipped files

accel_data <- rbindlist(
  lapply(accel_files, function(f) {
    if (file.size(f) == 0) {
      message("Skipping empty file: ", f)
      bad_files <<- c(bad_files, f)
      return(NULL)
    }
    dt <- tryCatch(
      fread(f, skip = 0),
      error = function(e) {
        message("Skipping unreadable file: ", f, " (", e$message, ")")
        bad_files <<- c(bad_files, f)
        return(NULL)
      }
    )
    dt
  }),
  use.names = TRUE,
  fill = TRUE
)

# save this as an RDA file
save(accel_data, file = file.path(accel_dir, "Board_Accel.RDA"))

# clean the system
rm(accel_data)
gc()
