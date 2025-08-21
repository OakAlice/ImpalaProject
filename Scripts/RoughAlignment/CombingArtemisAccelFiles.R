# Combining all artemis board files ---------------------------------------
# adapted from Chris Clemente -> https://github.com/cclemente/Collar_data_extraction/blob/main/Step1a_Read_in_raw_accel_files.R

# List all matching files
accel_files <- list.files(
  path = accel_dir,
  pattern = "^dataLog\\d+\\.TXT$",  # matches dataLog00000.TXT etc.
  full.names = TRUE
)

# Read & combine all files
accel_data <- rbindlist(
  lapply(accel_files, function(f) fread(f, skip = 0)),  # fread auto-skips blank lines
  use.names = TRUE,
  fill = TRUE
)

# save this as an RDA file
save(accel_data, file = file.path(accel_dir, "Board_Accel.RDA"))

# clean the system
rm(accel_data)
gc()
