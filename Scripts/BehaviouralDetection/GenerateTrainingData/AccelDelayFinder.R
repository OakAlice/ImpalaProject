# Interactive plot for finding the delay between video and accel ------------
# you'll need to do this manually
# every project will differ, but this is what worked for the impalas

# Functions ---------------------------------------------------------------
# get the video start and end time from any collar + video
get_predicted_video_times <- function(COLLAR_NUMBER, VIDEO_NUMBER){
  
  # define the videos of interest
  collar_dir <- file.path(base_path, "Data", "RawData", paste0("Collar_", COLLAR_NUMBER))
  videos <- list.files(file.path(collar_dir, "Videos"), full.names = TRUE, recursive = TRUE, pattern = "\\.MP4|MOV$")
  
  # define the specifc video and details 
  video_name <- basename(videos[VIDEO_NUMBER])
  
  # load in the metadata we extracted earlier
  video_metadata <- fread(file.path(collar_dir, "Video_metadata.csv"))
  
  # get the speific metadata for this exact video
  video_start <- video_metadata[filename == video_name, start_time]
  video_duration <- video_metadata[filename == video_name, duration_sec]
  video_end <- video_start + seconds(video_duration)
  
  # also the date, for getting the right accel chunk
  date <- as.POSIXct(basename(dirname(dirname(videos[VIDEO_NUMBER]))), format = "%d%m%Y", tz = "UTC")
  
  return(list(video_start = video_start,
              video_end = video_end,
              video_name = video_name,
              date = date))
}

# check this is the right segment and check the delay
# this value is derived from trial and error
# this shiny allows you to scroll up to a minute in either direction to find the match
# watch the video and try to align it. when you determine the delay, "clip" the relevant segment
plot_segment_app <- function(accel_data, video_start, video_end, clipped_dir_path) {
  ui <- fluidPage(
    sliderInput("delay", "Drone delay (seconds):",
                min = -60, max = 60, value = 0, step = 1),
    actionButton("save", "Save clipped accel segment"),
    plotOutput("accelPlot")
  )
  server <- function(input, output, session) {
    # reactive expression to compute accel_segment based on current delay
    accel_segment_reactive <- reactive({
      Drone_delay <- input$delay
      video_start_local <- video_start + seconds(Drone_delay)
      video_end_local   <- video_end + seconds(Drone_delay)
      video_start_utc <- with_tz(video_start_local, "UTC")
      video_end_utc   <- with_tz(video_end_local, "UTC")
      accel_segment <- accel_data[gps_time_est >= video_start_utc & gps_time_est <= video_end_utc]
      accel_segment[, X := RawAX]
      accel_segment[, Y := RawAY]
      accel_segment[, Z := RawAZ]
      accel_segment[, t_sec := as.numeric(gps_time_est - video_start_utc)]
      accel_segment[, t_minsec := sprintf("%d:%02d",
                                          as.integer(t_sec %/% 60),   # minutes
                                          as.integer(t_sec %% 60))]   # seconds
      accel_segment
    })
    # plot based on reactive accel_segment
    output$accelPlot <- renderPlot({
      accel_segment <- accel_segment_reactive()
      plot_data <- tidyr::pivot_longer(
        accel_segment,
        cols = c(X, Y, Z),
        names_to = "Axis",
        values_to = "Accel_g"
      )
      ggplot(plot_data, aes(x = t_sec, y = Accel_g, color = Axis)) +
        geom_line(alpha = 0.7) +
        labs(x = "Minutes since video start", y = "Acceleration (g)",
             title = paste0("Accelerometer Data (delay = ", input$delay, "s)")) +
        theme_minimal() +
        scale_x_continuous(
          labels = function(s) sprintf("%d:%02d", s %/% 60, s %% 60),
          breaks = scales::breaks_extended(8)
        )
    })
    # save when button is clicked
    observeEvent(input$save, {
      accel_segment <- accel_segment_reactive()
      # convert to MATLAB time
      matlab_origin <- 719529  # MATLAB datenum for 1970-01-01
      accel_segment[, time_matlab := as.numeric(gps_time_est) / 86400 + matlab_origin]
      out <- accel_segment[, .(time_matlab, X, Y, Z)]
      
      # make the directory
      if (!dir.exists(clipped_dir_path)) {
        dir.create(clipped_dir_path, recursive = TRUE)
      }
      # save with delay in filename
      vid_save_name <- tools::file_path_sans_ext(video_name)
      out_file <- file.path(
        clipped_dir_path,
        paste0(vid_save_name, "_delay", input$delay, "_clipped.csv")
      )
      fwrite(out, out_file)
      showNotification(paste("Saved:", out_file))
    })
  }
  shinyApp(ui, server)
}













# Code here ---------------------------------------------------------------
# set the variables here
COLLAR_NUMBER <- 13
collar_dir <- file.path(base_path, "Data", "RawData", paste0("Collar_", COLLAR_NUMBER))
videos <- list.files(file.path(collar_dir, "Videos"), full.names = TRUE, recursive = TRUE, pattern = "\\.MP4|MOV$")
videos
VIDEO_NUMBER <- 6

# now run this code
video_details <- get_predicted_video_times(COLLAR_NUMBER, VIDEO_NUMBER)

video_name <- video_details$video_name


# adding manually
# video_details$video_start <- as.POSIXct("2024-07-05 20:17:36", tz = "UTC")
# video_details$video_end <- video_details$video_start + 55 # duration in seconds
# video_details$video_name
# video_details$date <- as.Date(video_details$video_start)
# to check, we can convert back to local time and cross-ref with the true observations (which Jaz recorded in the field)

play <- accel_data[accel_data$gps_time_est > video_details$video_start & accel_data$gps_time_est < video_details$video_end,]
ggplot(play, aes(x = gps_time_est, y = RawAX)) + geom_path()

# now pull out the rough accelerometer section (we pull out the entire relevant day)
accel_files <- list.files(file.path(base_path, "Data", "RawData", paste0("Collar_", COLLAR_NUMBER), "Chunked"), full.names = TRUE)
load(accel_files[grep(video_details$date, accel_files)]) # comes in as accel_data

# now using the video times, visualise the segment that we thinkkkk its going to be 
plot_segment_app(accel_data = accel_data,
                 video_start = video_details$video_start, 
                 video_end = video_details$video_end,
                 clipped_dir_path = file.path(collar_dir, "Clipped") # where to save the clipped accelerometer 
                 )

# play the video in another screen and fiddle around with the delay (sliding forwards and backwards) until they seem aligned
# when they match up (close enough) then hit save and it will store the clipped segment
# ready for annotation in matlab :)






# Creating training data from animals with no video -----------------------
COLLAR_NUMBER <- 2
collar_dir <- file.path(base_path, "Data", "RawData", paste0("Collar_", COLLAR_NUMBER))

accel_files <- list.files(file.path(collar_dir, "Chunked"), full.names = TRUE)
load(accel_files[4]) # accel_data


play <- accel_data[1000000:2000000,]
ggplot(play, aes(x = utc_datetime)) + 
  geom_path(aes(y = RawAX, colour = "X")) + 
  geom_path(aes(y = RawAY, colour = "Y")) + 
  geom_path(aes(y = RawAZ, colour = "Z"))

play <- play %>% select(utc_datetime, RawAX, RawAY, RawAZ) %>%
  rename(time_matlab = utc_datetime,
         X = RawAX,
         Y = RawAY,
         Z = RawAZ) %>%
  mutate(time_matlab = as.numeric(time_matlab) / 86400 + matlab_origin)


fwrite(play, file.path(collar_dir, "Clipped", "Collar_2_Bonus2_clipped.csv"))
# Clipped Grazing 2 




# For the ones I forgot to save in the right format -----------------------
# files <- list.files("Data/RawData/Bonus", full.names = TRUE)
# matlab_origin <- 719529
# for(file in files){
#   dat <- fread(file) %>% select(gps_time_est, RawAX, RawAY, RawAZ) %>%
#     rename(time_matlab = gps_time_est,
#            X = RawAX,
#            Y = RawAY,
#            Z = RawAZ) %>%
#     mutate(time_matlab = as.numeric(time_matlab) / 86400 + matlab_origin)
#   
#   Collar <- str_split(basename(file), "_", simplify = TRUE)[1:2]
#   Collar <- paste(Collar[[1]], Collar[[2]], sep = "_")
#   
#   save_name <- file.path(base_path, "Data", "RawData", Collar, "Clipped", paste0(tools::file_path_sans_ext(basename(file)), "_clipped.csv"))
#   fwrite(dat, save_name)
# }


