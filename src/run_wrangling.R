# lydia barnes, march 2024 this script extracts, formats, and summarises data from the 'doors'
# project.
# amended by KG. 2025, for the 'doors' project in 2025
# amended by KG. 2026 for the 'routine vs habit' task
rm(list=ls())
### sources
library(tidyverse)
library(zeallot) #unpack/destructure with %<-%
library(stringr)
library(here)

source(paste(here(), "src", "get_subs.R", sep="/")) # this will need amending cos of subject numbers
source(paste(here(), "src", "get_switch.R", sep="/"))
source(paste(here(), "src", "get_data.R", sep="/"))
source(paste(here(), "src", "get_task_jumps.R", sep="/"))
source(paste(here(), "src", "get_reclicks.R", sep="/"))
source(paste(here(), "src", "get_rts.R", sep="/"))
source(paste(here(), "src", "get_TE.R", sep="/"))
### settings

# !you will want to update these settings a lot during piloting, when the task code or the way you
# test changes, or when you test participants on different subsets of the task phases
exp <- 'data' # data folder
sess <- c("ses-learn-uncertainty","ses-main-task") # we want data from
sv_name <- 'routine_vs_habit' # name of the project, for labelling outputs.
# these sessions

### paths

# !if you open the project thru doors.Rproj, your working directory will automatically be the
# project path
project_path <- here()
if (!dir.exists(file.path(project_path, "res"))) {
  # check that the results directory exists. if it doesn't, create it.
  dir.create(file.path(project_path, "res"))
}

file_path <- here()
data_path <- paste(file_path, 'data', sep='/')

if (!dir.exists(data_path)) {
  stop(paste0(data_path, " does not exist"))
}

### load an up-to-date list of participants
files <- list.files(data_path, pattern = str_glue('.*(beh.tsv)'), recursive = T)
subs <- unique(str_split_i(files, "/", 1))

### extract events from the raw data

# make an empty data frame with all the variables (columns) that we will want
grp_data <- data.frame(
  sub = integer(), ses = integer(), subses = integer(), t = integer(), context = integer(), door = integer(),
  door_cc = integer(), door_oc = integer(), on = numeric(), off = numeric(), start = numeric(),
  switch = integer(), train_type = integer(), block = factor(), stringsAsFactors = FALSE
)


# create empty data frame for the response time data # KG. commented out as now deal with during get_data
# grp_ons <- data.frame(
#   sub = integer(), ses = integer(), t = integer(), context = integer(), on = integer()
# )
# for each subject and session, use the function 'get_data' to load their raw data and attach it to
# our 'grp_data' data frame with one measurement (row) per event (click or hover)
for (sub in subs) {
  print(sub)

  sid <- as.numeric(substring(sub,5,7))
  for (ses in sess) {

    train_type <- NA
    data <- get_data(data_path, exp, sub, ses, train_type) # load and format raw data
    grp_data <- rbind(grp_data, data$resps) # add to the 'grp_data' data frame so we end up with all subjects and sessions in one spreadsheet
  }
}

grp_data <- grp_data %>% mutate(door_nc = case_when(door_cc==1 ~ 0, door_oc == 1 ~ 0, .default=1), .after="door_oc")

grp_data <- get_rts(grp_data) # calculate RTs and add to the data frame
grp_data <- get_task_jumps(grp_data, "res") # now calculate task_jumps per trial
grp_data <- get_reclicks(grp_data) # and now we calculate reclicks

# now calculate TE for grouping with summary level data below
# Goal: sum transitions over periods where there has not been a switch trial - i.e.
# between blocks of stay trials.
TE_summary <- grp_data %>%
  mutate(block = ifelse(is.na(block), "lu", block)) %>%
  group_by(sub, ses, block) %>%
  mutate(stay_set = case_when(
    switch == 1 ~ 0L,
    TRUE ~ cumsum(lag(switch, default=0) == 1 & switch == 0) + 1L
  )
  ) %>% filter(stay_set > 0) %>%
  group_by(sub, ses, block, context, stay_set) %>%
  group_modify(~ get_TE_scores(.x)) %>%
  ungroup()

# now rename the blocks so only one mt block (instead of b-mt1 & b-mt2). Do the same for the
# single task blocks, for both the grp_data and the TE summary data
grp_data <- grp_data %>% mutate(block = str_extract(block, "(?<=b-)[a-z]+"))
TE_summary <- TE_summary %>% mutate(block = str_extract(block, "(?<=b-)[a-z]+")) %>%
  summarise(
    .by=c(sub, ses, block, context),
    TE = mean(TE, na.rm=TRUE)
  ) %>%
  summarise(
    .by=c(sub, ses, block),
    TE = mean(TE, na.rm=TRUE)
  )

# save the formatted data
fnl <- file.path(project_path, "res", paste(paste(sv_name, "evt", sep = "_"), ".csv", sep = ""))
write_csv(grp_data, fnl)

### extract trial averages that we want from the data
# by trial
res <- grp_data %>%
  arrange(sub, ses, subses, t, block, context, train_type) %>%
  summarise(
    .by=c(sub, ses, subses, t, block, context, train_type),
    switch = max(switch),
    n_clicks = n(),
    n_cc = sum(door_cc),
    n_oc = sum(door_oc),
    n_nc = sum(door_nc),
    task_jumps = first(task_jumps),
    reclicks = first(reclicks),
    accuracy = n_cc / n_clicks,
    setting_errors = n_oc / n_clicks,
    general_errors = n_nc / n_clicks,
    all_errors = (n_oc + n_nc) / n_clicks
  ) %>% ungroup()

# now lets get the RT data we want
max_cutoff <- 2.0 # anything more than 2 is weird
sd_cut <- 2.5 # anything more than 2.5 SDs above the mean is also weird
rt_res <- grp_data %>%
  filter(start_rt < max_cutoff,
         press_duration < max_cutoff) %>%
  arrange(sub, ses, subses, t, block, context, train_type) %>%
  group_by(sub, ses, subses, block, context, train_type) %>%
  mutate(mean_rt = mean(start_rt, na.rm = TRUE),
         sd_rt = sd(start_rt, na.rm = TRUE),
         rt_cut_off = mean_rt + sd_cut * sd_rt,
         rt = ifelse(start_rt > rt_cut_off, NA, start_rt),
         mean_press = mean(press_duration, na.rm = TRUE),
         sd_press = sd(press_duration, na.rm = TRUE),
         press_cut_off = mean_press + sd_cut * sd_press,
         dur = ifelse(press_duration > press_cut_off, NA, press_duration)
  ) %>%
  ungroup() %>%
  summarise(.by = c(sub, ses, subses, t, block, context, train_type),
            n_rt_outliers = sum(is.na(rt)),
            rt = mean(rt, na.rm = TRUE),
            n_dur_outliers = sum(is.na(dur)),
            dur = mean(dur, na.rm = TRUE),
            N = n())

# and put the data back together
res <- res %>%
  left_join(rt_res, by = c("sub", "ses", "subses", "t", "block", "context", "train_type"))

fnl <- file.path(project_path, "res", paste(paste(sv_name, "trl", sep = "_"), ".csv", sep = ""))
write_csv(res, fnl)

# now what I want to do is provide the condition level summary statistics

# get summary statistics for remaining key DVs
summary_stats <- res %>%
  group_by(sub, ses, context, block, switch, train_type) %>%
  select(accuracy, setting_errors, general_errors, all_errors, task_jumps, reclicks, rt, dur) %>%
  summarise(
    across(
      .cols = where(is.numeric),
      .fns = list(mean = ~mean(.x, na.rm = TRUE)),
      .names = "{.col}_{.fn}"
    ),
    .groups = "drop_last"
  ) %>%
  ungroup() %>%
  group_by(sub, ses, block, switch, train_type) %>%
  select(ends_with("mean")) %>%
  summarise(
    across(
      .cols = where(is.numeric),
      .fns = list(mean = ~mean(.x, na.rm = TRUE)),
      .names = "{.col}"
    ),
    .groups = "drop_last"
  ) %>%
  ungroup()


summary_stats <- summary_stats %>%
  left_join(TE_summary,
            by=c('sub','ses','block')
  ) %>%
  mutate(
    TE = ifelse(switch == 1, NA, TE)
  )

fnms <- file.path(project_path, "res", paste(paste(sv_name, "avg", sep = "_"), ".csv", sep = ""))
write_csv(summary_stats, fnms)

# now get the proportions of outliers removed for each participant
outlier_proportions <- rt_res %>%
  group_by(sub) %>%
  summarise(
    prop_rt_outliers = sum(n_rt_outliers) / sum(N),
    prop_dur_outliers = sum(n_dur_outliers) / sum(N)
  )
o_fn <- file.path(project_path, "res", paste(paste(sv_name, "outliers", sep = "_"), ".csv", sep = ""))
write_csv(outlier_proportions, o_fn)
