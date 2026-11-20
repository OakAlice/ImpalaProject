# function to generate some random ecological pltos for troubleshooting and visualisation
generate_ecological_plots <- function(preds){
  
  # activity budgets basic
  counts <- preds %>% 
    mutate(Hour = hour(Time)) %>% 
    group_by(Hour) %>%
    dplyr::count(smoothed_class) %>%
    mutate(Proportion = n / sum(n)) %>%
    ungroup()
  
  budget <- ggplot(counts, aes(x = as.factor(Hour), y = Proportion, fill = smoothed_class)) +
    geom_col(position = "stack") +
    scale_fill_manual(values = fave_colours) + 
    my_theme()
  
  
  # a day in the life
  day_data <- preds %>% dplyr::filter(as.Date(Time) == unique(as.Date(preds$Time))[4]) %>% 
    mutate(Hour = hour(Time)) %>%
    group_by(Hour) %>%
    mutate(Minutes = row_number()/30)
  
  day_in_a_life <- ggplot(day_data, aes(x = Minutes, y = Hour, fill = smoothed_class))+
    geom_tile() + 
    scale_fill_manual(values = fave_colours) + 
    my_theme()
  
  # gps plot
  gps_only <- na.omit(preds[, c("Time", "ID", "lon", "lat")])
  ggplot(gps_only, aes(x = lon, y = lat, colour = Time)) + 
    geom_path() + 
    my_theme()
  
  
  
  return(list(budget = budget,
              day_in_a_life = day_in_a_life))
  
}


sanity_check_plots<- function(unlabelled_data, smoothed_predictions){
  #ugugughghghghghghh the plotting was SUCH a pain
  
  raw  <- as.data.table(unlabelled_data)   # copies, so the inputs aren't re-keyed by reference
  pred <- unique(as.data.table(smoothed_predictions)[, .(ID, Time, smoothed_class)],
                 by = c("ID", "Time"))
  
  setkey(raw,  ID, Time)
  setkey(pred, ID, Time)
  
  check <- pred[raw, roll = TRUE]
  setnames(check, "smoothed_class", "Activity")
  setcolorder(check, names(raw))
  
  sanity <- plotTraceExamples(behaviours = unique(check$Activity),
                              check, 
                              individuals = 1, n_samples = 6000, n_col = 3)
  
  # one palette, defined once up front
  beh_levels <- sort(unique(na.omit(check$Activity)))
  beh_cols   <- behaviour_colours[beh_levels]
  
  plots <- vector("list", 8)
  
  for (i in 1:4) {
    start <- sample.int(nrow(check) - 100000L, 1)
    dat   <- check[start:(start + 99999L)]
    
    dat[, run := rleid(Activity)]
    runs <- dat[, .(xmin = min(Time), xmax = max(Time), Activity = Activity[1]), by = run]
    
    # trace: no legend (X/Y/Z are fixed colours, state them in the caption)
    p_acc <- ggplot(dat, aes(x = Time)) +
      geom_path(aes(y = X, colour = "X")) +
      geom_path(aes(y = Y, colour = "Y")) +
      geom_path(aes(y = Z, colour = "Z")) +
      my_theme() +
      theme(axis.title = element_blank(), axis.text = element_blank(),
            legend.position = "none")
    
    # behaviour bar: the only legend
    p_beh <- ggplot(runs, aes(xmin = xmin, xmax = xmax, ymin = 0, ymax = 1, fill = Activity)) +
      geom_rect() +
      scale_fill_manual(name = "Behaviour", values = beh_cols,
                        limits = beh_levels, drop = FALSE) +
      my_theme() +
      theme(axis.title = element_blank(), axis.text = element_blank(),
            axis.ticks.y = element_blank())
    
    plots[[2 * i - 1]] <- p_acc
    plots[[2 * i]]     <- p_beh
  }
  
  design <- c(
    area(1,  1, 10, 1), area(11, 1, 11, 1),
    area(1,  2, 10, 2), area(11, 2, 11, 2),
    area(13, 1, 22, 1), area(23, 1, 23, 1),
    area(13, 2, 22, 2), area(23, 2, 23, 2)
  )
  
  grid <- wrap_plots(plots) +
    plot_layout(design = design, guides = "collect") &
    theme(legend.position = "bottom")
  
  return(list(all_behs = sanity,
              random_grid = grid))
  
}
