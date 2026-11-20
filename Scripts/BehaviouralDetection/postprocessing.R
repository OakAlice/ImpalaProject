# Postprocessor -----------------------------------------------------------
# Lazily just using the bayes method I experimented with in an earlier paper
# all of this copied from https://github.com/OakAlice/PostProcessing

identify_sequences <- function(data, max_break = 1){
  
  if(species == "Dunford_Cat"){
    data <- data %>%
      group_by(ID) %>%
      arrange(Time, .by_group = TRUE) %>%
      mutate(
        time_sec = as.numeric(
          strptime(Time, format = "%H:%M:%S", tz = "UTC")
        ),
        day_offset = cumsum(c(0, diff(time_sec) < 0)),
        numeric_datetime = day_offset * 86400 + time_sec,
        Time = as.POSIXct(
          numeric_datetime,
          origin = "1970-01-01",
          tz = "UTC"
        )
      ) %>%
      ungroup() %>%
      select(-time_sec, -day_offset, -numeric_datetime)
  } else if (species == "Ferdinandy_Dog"){
    data <- data %>% 
      mutate(Time = as.numeric(Time))
  }
  
  data <- data %>%
    group_by(ID) %>%
    arrange(Time, .by_group = TRUE) %>%
    mutate(time_diff = difftime(Time, data.table::shift(Time)), # had to define package or errored
           break_point = ifelse(time_diff > max_break | time_diff < 0 , 1, 0),
           break_point = replace_na(break_point, 0),
           sequence = cumsum(break_point)) %>%
    select(-break_point, -time_diff) %>%
    ungroup()
  
  return(data)
  
}

identify_events <- function(data, class_col = "true_class"){
  
  class_sym <- sym(class_col)
  
  data <- data %>%
    group_by(ID, sequence) %>%
    arrange(Time, .by_group = TRUE) %>%
    mutate(
      change_point = if_else(lag(!!class_sym) == !!class_sym, 0L, 1L),
      change_point = replace_na(change_point, 0L),
      event = cumsum(change_point)
    ) %>%
    ungroup() %>%
    select(-change_point)
  
  return(data)
}

# Generate Transition probabilities ---------------------------------------
generate_transition_probabilities <- function(train_data, Hz){
  
  train_data <- train_data %>% arrange(ID, Time)
  train_data <- identify_sequences(train_data, max_break = ifelse(Hz > 1, 3, 6)) # based on window duration and buffer
  
  states <- levels(as.factor(train_data$Activity))
  n_states <- length(states)
  
  # calculate the transitions within continuous sequences
  transition_counts <- matrix(0, n_states, n_states, dimnames = list(states, states))
  train_data <- train_data %>%
    group_by(ID, sequence) %>%
    arrange(Time, .by_group = TRUE) %>%
    mutate(next_state = lead(Activity)) %>%
    ungroup()
  transitions <- na.omit(train_data[, c("Activity", "next_state")]) # remove the ones that are last in sequence
  
  # probability of each of these transitions
  for (j in seq_len(nrow(transitions))) {
    from <- as.character(transitions$Activity[j])
    to <- as.character(transitions$next_state[j])
    transition_counts[from, to] <- transition_counts[from, to] + 1
  }
  
  transition_matrix <- prop.table(transition_counts, 1)
  transition_probs <- as.data.frame(transition_matrix)
  transition_probs_melted <- transition_probs %>%
    tibble::rownames_to_column(var = "First") %>%
    pivot_longer(cols = -First, names_to = "Second", values_to = "Probability")
  
  return(list(transition_matrix = transition_matrix,
              transition_probs_melted = transition_probs_melted))
}

# Bayesian ----------------------------------------------------------------
apply_bayes_smoothing <- function(data, states, transition_matrix) {
  
  # sometimes the model predicts fewer classes than are in the training data, and so we get an imbalance
  # these steps are designed to allow the model to function even without all classes predicted
  
  setDT(data)
  
  n_time  <- nrow(data)
  n_class <- length(states)
  
  smoothed_probs <- matrix(0, n_time, n_class)
  colnames(smoothed_probs) <- states
  
  # get the liklihood
  get_likelihood <- function(dt_row, states) {
    lik <- rep(0, length(states))
    names(lik) <- states
    
    present <- intersect(states, names(dt_row))
    lik[present] <- as.numeric(dt_row[, ..present])
    
    lik
  }
  
  prior <- rep(1 / n_class, n_class)
  
  likelihood <- get_likelihood(data[1], states)
  posterior  <- prior * likelihood
  
  if (sum(posterior) == 0) {
    posterior <- prior
  } else {
    posterior <- posterior / sum(posterior)
  }
  
  smoothed_probs[1, ] <- posterior
  
  # do the bayes filter recursivly
  for (t in 2:n_time) {
    
    # prediction step
    prior <- as.numeric(transition_matrix %*% smoothed_probs[t - 1, ])
    
    # update step
    likelihood <- get_likelihood(data[t], states)
    posterior  <- prior * likelihood
    
    if (sum(posterior) == 0) {
      posterior <- prior
    } else {
      posterior <- posterior / sum(posterior)
    }
    
    smoothed_probs[t, ] <- posterior
  }
  
  # put in the sequence
  smoothed_class <- colnames(smoothed_probs)[
    max.col(smoothed_probs, ties.method = "first")
  ]
  
  smoothed_class
}


# Wrapper -----------------------------------------------------------------
postprocessing <- function(labelled_data, original_predictions, method = "Bayesian", sample_rate){

  # load in the training data and generate the transition matrix
  transition_probs <- generate_transition_probabilities(labelled_data, Hz = sample_rate)
  transition_probs_melted <- transition_probs$transition_probs_melted
  transition_matrix <- transition_probs$transition_matrix
  states <- levels(as.factor(labelled_data$Activity))
  
  # load in the ecological predictions
  original_predictions <- identify_sequences(original_predictions, max_break = ifelse(sample_rate > 1, 3, 6))
  original_predictions$set <- paste(original_predictions$ID, original_predictions$sequence, sep = "_")
    
    # run the bayes over each of the sequences and save the smoothed class
    bayes_data <- lapply(unique(original_predictions$set), function(x){
      dat <- original_predictions %>% dplyr::filter(set == x)
      if (nrow(dat) < 3) {
        dat$smoothed_class <- dat$predicted_class
        return(dat)
      }
      dat$smoothed_class <- apply_bayes_smoothing(dat, states, transition_matrix)
      
      dat
    })
    
    bayes_data <- rbindlist(bayes_data)
    
    return(bayes_data)
  
}
