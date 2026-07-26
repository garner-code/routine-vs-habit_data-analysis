count_stereo <- function(data) {

  ### insensitive to feedback?  count re-clicks on previous context doors on switch trials
  #events <- data %>%
  #  filter(switch == 1, door_oc == 1)
  #reclicks <- events %>%
  #  group_by(sub, ses, t, context) %>%
  #  summarise(n = n(), n_reclicks = n() - length(unique(door)))
  #reclicks <- reclicks %>%
  #  group_by(sub, ses, context) %>%
  #  summarise(clicks = mean(n), reclicks = mean(n_reclicks))

  tmp <- data %>% filter(switch == 1)
  reclicks <- data.frame()
  doors <- list()
  for(s in unique(tmp$sub)){
    doors[[1]] <- unique(tmp %>% filter(sub==s, context==2, door_cc==1) %>% pull(door))
    doors[[2]] <- unique(tmp %>% filter(sub==s, context==1, door_cc==1) %>% pull(door))
    for(ctx in unique(tmp$context)){

      for (trial in unique(tmp %>% filter(sub==s, context==ctx) %>% pull(t))){
        x <- tmp %>% filter(sub==s, context==ctx, t==trial)

        used <- rep(FALSE, 4)
        reclick_count <- 0
        out <- FALSE
        while(!out){
          for(click in 1:nrow(x)){
            if(x$door[click] %in% doors[[ctx]]){

              if(!all(used)){
                used[match(x$door[click],doors[[ctx]])] <- TRUE
              }else{
                reclick_count <- reclick_count+1
              }

            } else{
              out <- TRUE #they have left the previous trial's context
            }
          }
        }

        reclicks <- rbind(reclicks, data.frame(sub=s, t=trial, context=ctx, reclicks = reclick_count))
      }
    }
  }

  ### accurate?  count accuracy on stay trials
  events <- data %>%
    filter(switch == 0)
  accuracy <- events %>%
    group_by(sub, ses, t, context) %>%
    summarise(n_clicks = n(), n_correct = sum(door_cc), n_correct_oc = sum(door_oc),
              accuracy = n_correct / n_clicks, metatask_accuracy = (n_correct+n_correct_oc)/n_clicks)
  accuracy <- accuracy %>%
    group_by(sub, ses, context) %>%
    summarise(accuracy = mean(accuracy), metatask_accuracy = mean(metatask_accuracy))

  ### consistent in transitions?
  transitions <- data.frame(
    sub = integer(), ses = integer(), context = integer(),
    transition_counts = double(), transition_weights = double(), entropy = double()
  )
  print("getting the transition matrix")
  for (su in unique(data$sub)) {
    print(su)
    for (se in unique(data$ses)) {
      for (co in unique(data$context)) {
        # reduce to correct click events on stay trials
        events <- data %>% filter(switch == 0, sub == su, ses == se, context == co)

        # -------------------------------------------------------------------------
        # make the full transitions matrix
        transition_matrix <- matrix(0, nrow = 16, ncol = 16)
        doors <- unique(events$door)

        # select a trial
        for (tr in unique(events$t)) {
          trial <- events %>% filter(t == tr)

          # if there's more than one event, record door transitions
          if (nrow(trial) > 1) {
            for (i in 2:nrow(trial)) {
              door <- trial$door[i]
              previous <- trial$door[i - 1]
              transition_matrix[previous, door] <- transition_matrix[previous, door]+1
            }
          }
        }

        # -------------------------------------------------------------------------
        # TRANSITION COUNTS
        # for each door i, find the number of unique ways that this person gets to it, then take the mean across i's
        transition_counts <- colSums(transition_matrix)
        transition_counts <- mean(transition_counts[transition_counts!=0])

        # -------------------------------------------------------------------------
        # TRANSITION WEIGHTS
        # for each door i, find the door j that most often goes to it. take its probability (n clicks on j before i / n clicks on i)
        transition_weights <- colMax(data.frame(transition_matrix))/colSums(transition_matrix)
        transition_weights <- mean(transition_weights[!is.na(transition_weights)])

        # -------------------------------------------------------------------------
        # ENTROPY
        # take the probability of each transition, given the number of transitions
        # multiply by the log of its probability, sum log probabilities, and take the negative
        entropy <- transition_matrix/colSums(transition_matrix)
        entropy <- entropy * sapply(entropy,log2)
        entropy <- -colSums(entropy,na.rm=TRUE)
        entropy <- mean(entropy)

        if (!is.nan(transition_counts)) {
          # store
          transitions[nrow(transitions) + 1, ] <- data.frame(su, se, co, transition_counts, transition_weights, entropy)
        }
      }
    }
  }
  transitions_accuracy <- accuracy %>% add_column(transition_counts = transitions$transition_counts,
                                                  transition_weights = transitions$transition_weights,
                                                  entropy = transitions$entropy)

  ### following a shortest path?
  path_match <- data.frame(
    sub = integer(), ses = integer(), t = integer(), context = integer(), travelled = double(),
    travelling_match = double(), travelling_overshoot = double(), hamiltonian_match = double(), hamiltonian_overshoot = double()
  )
  print("processing match to shortest path")
  for (su in unique(data$sub)) {
    print(su)
    for (se in c(2)) {
      for (co in unique(data$context)) {
        # get clicks on stay trials
        events <- data %>% filter(switch == 0, sub == su, ses == se, context == co, door_cc == 1)

        # make the data frame
        tmp <- data.frame(sub = integer(), ses = integer(), t = integer(), context = integer())
        for (tr in unique(events$t)) {
          tmp[nrow(tmp) + 1, ] <- data.frame(su, se, tr, co)
        }

        ### travelling salesman solutions (return to start)
        opt_sub <- opt %>% filter(sub == su, context == co, algorithm == "travelling")
        df <- compare_paths(graph, events, opt_sub, "travelling")
        df_tsp <- df %>% rename(travelling_match = match, travelling_overshoot = overshoot)

        ### shortest hamiltonian path (don't return to start)
        opt_sub <- opt %>% filter(sub == su, context == co, algorithm == "hamiltonian")
        df <- compare_paths(graph, events, opt_sub, "hamiltonian")
        df_hp <- df %>% rename(hamiltonian_match = match, hamiltonian_overshoot = overshoot) %>% select(hamiltonian_match,hamiltonian_overshoot)

        # stack
        tmp <- cbind(tmp, df_tsp, df_hp)
        path_match <- rbind(path_match, tmp)
      }
    }
  }

  # put all these measures together
  path_match <- path_match %>%
    group_by(sub, ses, context) %>%
    summarise_all(mean) %>% select(!t)
  t <- transitions_accuracy %>%
    ungroup() %>%
    select(accuracy, metatask_accuracy, transition_counts, transition_weights, entropy)

  reclicks <- reclicks %>%
    group_by(sub, context) %>%
    summarise(reclicks = mean(na.omit(reclicks)))

  stereo <- bind_cols(path_match, t)
  stereo <- inner_join(stereo, reclicks, by = c("sub", "context"))

  return(stereo)
}

