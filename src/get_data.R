get_data <- function(data_path, exp, sub, ses, train_type, train_doors) {
  # reads in trial info and sample data from 'trls' and 'beh' files and formats into a
  # one-row-per-trial data frame
  success <- c()
  if (ses %in% c("ses-learn-uncertainty", "ses-main-task")) {

    if (ses %in% c("ses-learn-uncertainty")){
      success <- rbind(success, file.exists(file.path(data_path, sub, ses, "beh", paste(sub, ses,
                                                                                        "task-mforage_trls.tsv",
                                                                                        sep = "_"
      ))))
      success <- rbind(success, file.exists(file.path(data_path, sub, ses, "beh", paste(sub, ses,
                                                                                        "task-mforage_beh.tsv",
                                                                                        sep = "_"
      ))))
    } else {
      blocks <- c("b-mt1", "b-mt2", "b-st1", "b-st2")

      for (block in blocks) {

        # beh file
        success <- rbind(success,
          file.exists(file.path(data_path,sub, ses, "beh",
              paste(sub, ses, block, "task-mforage_beh.tsv", sep = "_")
            )
          )
        )
        # trls file
        success <- rbind(success,
          file.exists(file.path(data_path,sub, ses, "beh",
              paste(sub, ses, block, "task-mforage_trls.tsv", sep = "_")
            )
          )
        )

      }
    }

  } else { # KG: check this when testing the learning phase data
    haus <- c("house-1", "house-2")
    for (h in haus) {
      success <- rbind(success, file.exists(file.path(data_path, sub, ses, "beh", paste(sub,
        ses, h, "task-mforage_trls.tsv",
        sep = "_"
      ))))
      success <- rbind(success, file.exists(file.path(data_path, sub, ses, "beh", paste(sub,
        ses, h, "task-mforage_beh.tsv",
        sep = "_"
      ))))
    }
  }

  # now load data
  if (all(success)) {

    if (ses %in% c("ses-learn-uncertainty")) {
      trials <- read.table(file.path(data_path, sub, ses, "beh", paste(sub, ses, "task-mforage_trls.tsv",
                                                                       sep = "_"
      )), header = TRUE)
      trials$block <- NA
      resps <- read.table(file.path(data_path, sub, ses, "beh", paste(sub, ses, "task-mforage_beh.tsv",
                                                                      sep = "_"
      )), header = TRUE, check.names = FALSE)
      resps$block <- NA
    } else if (ses == "ses-main-task") {
      blocks <- c("b-mt1", "b-mt2", "b-st1", "b-st2")
      trials <- list()
      resps <- list()
      for (block in blocks) {
        trials[[block]] <- read.table(file.path(data_path, sub, ses, "beh", paste(sub, ses,
                                                                                  block, "task-mforage_trls.tsv",
                                                                                  sep = "_"
        )), header = TRUE)
        resps[[block]] <- read.table(file.path(data_path, sub, ses, "beh", paste(sub, ses,
                                                                                 block, "task-mforage_beh.tsv",
                                                                                 sep = "_"
        )), header = TRUE, check.names = FALSE)
        trials[[block]]$block <- block
        resps[[block]]$block <- block
      }
      trials <- do.call(rbind, trials)
      resps <- do.call(rbind, resps)
      total_trials_per_block = max(trials$t)
      trials <- trials %>%
        mutate(
          block = factor(block, levels = blocks),
          t_global = t + (as.integer(block) - 1) * total_trials_per_block,
          t = t_global
        ) %>% select(!t_global)

      resps <- resps %>%
        mutate(
          block = factor(block, levels = blocks),
          t_global = t + (as.integer(block) - 1) * total_trials_per_block,
          t = t_global
        ) %>% select(!t_global)


  }
    # } else { # KG: check this when testing the learning phase data
    #   trials <- read.table(file.path(data_path, sub, ses, "beh", paste(sub, ses, "house-1",
    #                                                                    "task-mforage_trls.tsv",
    #                                                                    sep = "_"
    #   )), header = TRUE)
    #   trials <- rbind(trials, read.table(file.path(data_path, sub, ses, "beh", paste(sub,
    #                                                                                  ses, "house-2", "task-mforage_trls.tsv",
    #                                                                                  sep = "_"
    #   )), header = TRUE))
    #
    #   resps_1 <- read.table(file.path(data_path, sub, ses, "beh", paste(sub, ses, "house-1",
    #                                                                     "task-mforage_beh.tsv",
    #                                                                     sep = "_"
    #   )), header = TRUE)
    #   resps_2 <- read.table(file.path(data_path, sub, ses, "beh", paste(sub, ses, "house-2",
    #                                                                     "task-mforage_beh.tsv",
    #                                                                     sep = "_"
    #   )), header = TRUE)
    # }

    ### trim the data remove practice trials and reset trial numbers

    # if (ses %in% c("ses-learn")){ # KG - check this when testing learning data
    #
    #   trials <- trials %>%
    #     filter(t != 999)
    #   if (version == "pilot-data-00" || version == "pilot-data-01") {
    #     resps <- resps %>%
    #       filter(cond != 3)
    #     resps$t <- resps$t - 5
    #   } else {
    #     resps_1 <- resps_1 %>%
    #       filter(cond != 3) # remove practice trials from house-1
    #     resps_1$t <- resps_1$t - 5 # adjust the trial counter for house-1, now that practice trials are gone
    #   }
    #
    #   resps_2$t <- resps_2$t + resps_1$t[nrow(resps_1)]
    #   # resps_3$t <- resps_3$t + resps_2$t[nrow(resps_2)]
    #
    #   resps <- rbind(resps_1, resps_2) #, resps_3)
    #   #resps$learn_phase <- c(rep(0,nrow(resps_1)),rep(0,nrow(resps_2)),rep(1,nrow(resps_3)))
    # }

    resps <- resps %>%
      rename(context = cond) %>%
      mutate(door_cc = case_when(door_p > 0 ~ 1, door_p == 0 ~ 0, .default = 0)) # door_cc = current context
    if (ses == "ses-learn") {
      resps <- resps %>%
        rename(ses = learn)
    } else if (ses == "ses-learn2") {
      resps <- resps %>%
        rename(ses = learn2)
    } else if (ses == "ses-learn-uncertainty") {
      resps <- resps %>%
        rename(ses = `learn-uncertainty`)
    } else if (ses == "ses-main-task") {
      resps <- resps %>%
        rename(ses = `main-task`)
    }

    # find the time of the onset of each trial, to
    # be used to calculate the RT of the first response on every trial
    ons <- resps %>% #
      group_by(sub, ses, t) %>%
      summarise(trial_start = min(onset)) %>% # this gives the first timestamp for the trial
      ungroup() #

    resps <- resps %>%
      filter(door > 0) # we only care about samples in which people clicked on a door

    ### find the important events - KG has amended to make simpler
    resps <- resps %>%
      mutate(on = c(onset[[1]],
                    case_when(diff(open_d) != 0 ~ onset[2:length(onset)], # this code is losing trials where
                              diff(door) != 0 ~ onset[2:length(onset)], # you hit the same button twice & the target is there
                              .default = NA))) %>%
      mutate(off = c(case_when(diff(open_d) != 0 ~ onset[1:length(onset) - 1],
                               diff(door) != 0 ~ onset[1:length(onset) - 1],
                               .default = NA),
                     onset[[length(onset)]])) %>%
      filter(!is.na(on) | !is.na(off)) %>%
      filter( on != off | is.na(on) | is.na(off)) %>% # KG. removing events where there was effectively no time spent there
      mutate(off = lead(off)) %>%
      filter(!is.na(on))

    # Now I want to filter for only the door selections
    resps <- resps %>% filter(open_d == 1) %>%
      group_by(t) %>%
      mutate(start = lag(off)) %>%
      ungroup()

    # now add the trial onset to the NA data
    resps <- resps %>% left_join(ons, by = c('sub', 'ses', 't')) %>%
      mutate(start = coalesce(start, trial_start)) %>%
      select(-trial_start)

    trials <- unique(resps$t)
    resps <- resps %>%
      mutate(subses = case_when(t %in% trials[1:round(length(trials) / 2)] ~ 1, .default = 2), .after=str_split_i(ses, '-', 1)) # 2

    ### code door by whether it's part of current context, other context, or no context
    doors <- resps %>%
      filter(door_cc == 1) %>%
      group_by(context) %>%
      distinct(door)
    tmp <- list()
    for (i in 1:2) {
      tmp[[i]] <- resps %>%
        filter(context == i) %>%
        mutate(door_oc = case_when((!(door %in% filter(doors, context == i)$door) & door %in%
          doors$door) ~ 1, .default = 0),.after=door_cc)
    }
    resps <- rbind(tmp[[1]], tmp[[2]]) %>%
      arrange(t)

    ### format events
    resps <- resps %>% select(!c(onset, door_p:y)) # remove unnecessary variables
    resps <- resps %>%
      filter(open_d == 1) %>% #
      select(!open_d) # find clicks

    # record whether each trial starts with a context switch
    resps <- get_switch(resps)

    # mark the train phase switch rate and context-one doors in the test phase data
    resps <- resps %>%
      mutate(train_type = NA) # keeping due to legacy reasons


    return(list(resps=resps, ons=ons))
  } else {
    stop(paste("check data for", file.path(data_path, exp, sub, ses)))
  }
}
