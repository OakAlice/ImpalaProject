# Interactive plot for finding the delay between video and accel ------------
# you'll need to do this manually
# save results in an excel sheet - in this case I saved it to the RawData folder under Video_info.csv
# Manulaly define the video and date you want to work on ------------------

Collar <- "Collar_8"

accel_dir <- file.path(base_path, "RawData", Collar, "Board")
video_dir <- file.path(base_path, "RawData", Collar, "Videos")

videos <- list.files(video_dir, full.names = TRUE, recursive = TRUE, pattern = "\\.MP4|MOV$")
videos
# video number selection
vid_number_in_list <- 23

{
video_name <- basename(videos[vid_number_in_list]) # "DJI_20240702082054_0038_D.MP4"
date <- as.POSIXct(basename(dirname(dirname(videos[vid_number_in_list]))), format = "%d%m%Y", tz = "UTC") # "2024-07-02"

# get the metadata
video_metadata <- fread(file.path(video_dir, "Video_metadata.csv"))
video_start <- video_metadata[filename == video_name, start_time]
video_duration <- video_metadata[filename == video_name, duration_sec]
video_end <- video_start + seconds(video_duration)

# Load in the data --------------------------------------------------------
accel_files <- list.files(file.path(accel_dir, "Chunked"), full.names = TRUE)
# Load in the relevant accelerometer
load(accel_files[grep(date, accel_files)]) # comes in as accel_data
if (!inherits(accel_data$gps_time_est, "POSIXct")) {
  accel_data$gps_time_est <- as.POSIXct(accel_data$gps_time_est, tz="UTC")
}
setDT(accel_data)
}





plot_segment_app(video_start, video_end, date)





# Set the variables -----------------------------------------------------------
# Delay
# this value is derived from trial and error
# once it is determined for a single day, it can be used for all videos on that date
# this little shiny app allows you to scroll up to a minute in either direction to find the match
# watch the video and try to align it.
# when you determine the delay, save that number.
plot_segment_app <- function(video_start, video_end, date, x = 5) {
  ui <- fluidPage(
    sliderInput("delay", "Drone delay (seconds):",
                min = -120, max = 120, value = 0, step = 1),
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
      accel_segment[, X := RawAX / 8192]
      accel_segment[, Y := RawAY / 8192]
      accel_segment[, Z := RawAZ / 8192]
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
      clipped_dir_path <- file.path(accel_dir, "Clipped")
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


















# Other plot designs -------------------------------------------------------
#static
plot_segment(video_start, video_end, date, Drone_delay, x)
# check whether aligned and if not, alter the Drone_delay and try again until close enough
# record the drone delay in the spreadsheet
plot_segment <- function(video_start, video_end, date, Drone_delay, x){
  video_start_local <- video_start + seconds(Drone_delay)
  video_end_local <- video_end + seconds(Drone_delay)
  # calculate the utc time # NOTE: need to come back to and question the drone timezone
  video_start_utc <- with_tz(video_start_local, "UTC")
  video_end_utc <- with_tz(video_end_local, "UTC")
  # Extract and format segment matching video -------------------------------
  accel_segment <- accel_data[gps_time_est >= video_start_utc & gps_time_est <= video_end_utc]
  # Convert raw counts to g
  accel_segment$X <- accel_segment$RawAX / 8192
  accel_segment$Y <- accel_segment$RawAY / 8192
  accel_segment$Z <- accel_segment$RawAZ / 8192
  # seconds from video start (POSIXct difference → seconds)
  accel_segment$t_sec <- as.numeric(accel_segment$gps_time_est - video_start_utc)
  plot_data <- tidyr::pivot_longer(
    accel_segment,
    cols = c(X, Y, Z),
    names_to = "Axis",
    values_to = "Accel_g"
  )
  # plot_data <- plot_data[1:1000,]
  #for plotting in seconds. Good for short videos
  ggplot(plot_data, aes(x = t_sec, y = Accel_g, color = Axis)) +
    geom_line(alpha = 0.7) +
    labs(x = "Seconds since video start", y = "Acceleration (g)",
         title = "Accelerometer Data") +
    theme_minimal() +
    scale_x_continuous(
      breaks = function(lims) seq(ceiling(lims[1]/x)*x, floor(lims[2]/x)*x, by = x),
      minor_breaks = function(lims) seq(ceiling(lims[1]), floor(lims[2]), by = x/2)
    )
  #plots minutes and seconds
  # ggplot(plot_data, aes(x = t_sec, y = Accel_g, color = Axis)) +
  #   geom_line(alpha = 0.7) +
  #   labs(x = "Time since video start (MM:SS)", y = "Acceleration (g)",
  #        title = "Accelerometer Data") +
  #   theme_minimal() +
  #   scale_x_continuous(
  #     breaks = function(lims) seq(ceiling(lims[1]/5)*5, floor(lims[2]/5)*5, by = 10),
  #     labels = function(x) {
  #       x <- floor(x + 0.5)                 # round to nearest second
  #       sprintf("%02d:%02d", x %/% 60, x %% 60)
  #     }
  #   )
}
create_accel_plot(plot_data)
# Function to create zoomable/interactive plot to see the claps
create_accel_plot <- function(accel_data) {
  plot <- plot_ly() %>%
    # Add traces for each axis
    add_trace(data = accel_data,
              x = ~t_sec,
              y = ~Accel_g,
              name = "X",
              type = "scatter",
              mode = "lines",
              line = list(colour = '#4F5AAD'),
              hovertemplate = paste("Time: %{x}<br>",
                                    "X: %{y}<br>",
                                    "Row: %{customdata}<br>",
                                    "<extra></extra>"),
              customdata = seq_len(nrow(accel_data))) %>%
    add_trace(data = accel_data,
              x = ~t_sec,
              y = ~Y,
              name = "Y",
              type = "scatter",
              mode = "lines",
              line = list(colour = '#579157'),
              hovertemplate = paste("Time: %{x}<br>",
                                    "Y: %{y}<br>",
                                    "Row: %{customdata}<br>",
                                    "<extra></extra>"),
              customdata = seq_len(nrow(accel_data))) %>%
    add_trace(data = accel_data,
              x = ~t_sec,
              y = ~Z,
              name = "Z",
              type = "scatter",
              mode = "lines",
              line = list(colour = 'goldenrod'),
              hovertemplate = paste("Time: %{x}<br>",
                                    "Z: %{y}<br>",
                                    "Row: %{customdata}<br>",
                                    "<extra></extra>"),
              customdata = seq_len(nrow(accel_data))) %>%
    # Layout configuration
    layout(xaxis = list(title = "t_sec",
                        rangeslider = list(visible = TRUE)),
           yaxis = list(title = "Acceleration"),
           hovermode = "closest")
  return(plot)
}

