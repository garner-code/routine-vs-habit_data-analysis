################################################################################
########    Did our response time manip lead to slower rt in mt con?    ########
########                    Sadie lane, 2026                            ########
################################################################################

rm(list=ls())
library(tidyverse)
library(broom)
library(skimr)

setwd("C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/res")

#read in data
averages <- read_csv(
  "routine_vs_habit_avg.csv",
  na = c("", "NA")
)

outlierless <- read_csv(
  "averages_no_n_nc_no_nback_outs.csv",
  na = c("", "NA")
)

# tidy data --------------------------------------------------------------------

#just to get an idea of some summary stats
averages |>
  filter(ses == 4, switch == 0) |>
  group_by(block) |>
  summarise(
    M = mean(rt_mean),
    SD = sd(rt_mean),
    accM = mean(accuracy_mean),
    accSD = sd(accuracy_mean),
    tjsM = mean(task_jumps_mean),
    tjsSD = sd(task_jumps_mean)
  )

#no transform, outliers included
all_for_t_tests <- averages |>
  filter(ses == 4 & switch == 0) |>
  select(sub, block, rt_mean, accuracy_mean, task_jumps_mean) |>
  pivot_wider(
    names_from = block,
    values_from = c(rt_mean, accuracy_mean, task_jumps_mean)
  )

#log transform, outliers included
all_for_t_tests_log <- all_for_t_tests |>
  mutate(
    rt_mt_log  = log(rt_mean_mt + 0.001),
    rt_st_log  = log(rt_mean_st + 0.001),
    tj_mt_log = log(task_jumps_mean_mt + 0.001),
    tj_st_log = log(task_jumps_mean_st + 0.001),
  ) |>
  select(sub, rt_mt_log, rt_st_log, tj_mt_log, tj_st_log)



#no transform, no outliers
for_t_tests <- outlierless |>
  filter(ses == 4 & switch == 0) |>
  select(sub, block, rt_mean, accuracy_mean, task_jumps_mean) |>
  pivot_wider(
    names_from = block,
    values_from = c(rt_mean, accuracy_mean, task_jumps_mean)
  )

#log transform, no outliers
for_t_tests_log <- for_t_tests |>
  mutate(
    rt_mt_log  = log(rt_mean_mt + 0.001),
    rt_st_log  = log(rt_mean_st + 0.001),
    tj_mt_log = log(task_jumps_mean_mt + 0.001),
    tj_st_log = log(task_jumps_mean_st + 0.001),
  ) |>
  select(sub, rt_mt_log, rt_st_log, tj_mt_log, tj_st_log)

# analyse ----------------------------------------------------------------------
# run t tests and summaries. Output into csvs. #n.b. this is all for switch = 0.
# task jumps as well as rt


#first with outliers taken out
#which as a reminder is n_nc, and sens -
#(n-back, i.e. they weren't really mting)

t_rt_outless <- with(for_t_tests, t.test(rt_mean_st, rt_mean_mt))
t_rt_outless <- tidy(t_rt_outless)
#ns
write_csv(
  t_rt_outless,
  "C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/res/analysis/t_rt_outless.csv"
)


t_rt_outless_log <- with(for_t_tests_log, t.test(rt_st_log, rt_mt_log))
t_rt_outless_log <- tidy(t_rt_outless_log)
#ns
write_csv(
  t_rt_outless_log,
  "C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/res/analysis/t_rt_outless_log.csv"
)


t_tj_outless <- with(for_t_tests, t.test(task_jumps_mean_st, task_jumps_mean_mt))
t_tj_outless <- tidy(t_tj_outless)
#ns
write_csv(
  t_tj_outless,
  "C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/res/analysis/t_tj_outless.csv"
)


t_tj_outless_log <- with(for_t_tests_log, t.test(tj_st_log, tj_mt_log))
t_tj_outless_log <- tidy(t_tj_outless_log)
#sig with 0.6, ns with 0.65
write_csv(
  t_tj_outless_log,
  "C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/res/analysis/t_tj_outless_log.csv"
)



#and get summary stats
outless_summary <- skim_without_charts(for_t_tests)
write_csv(
  outless_summary,
  "C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/res/analysis/outless_summary.csv"
)

outless_log_summary <- skim_without_charts(for_t_tests_log)
write_csv(
  outless_log_summary,
  "C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/res/analysis/outless_log_summary.csv"
)


# now with all participants included -------------------------------------

all_t_rt <- with(all_for_t_tests, t.test(rt_mean_st, rt_mean_mt))
all_t_rt <- tidy(all_t_rt)
#sig - interesting as outlierless is ns
write_csv(
  all_t_rt,
  "C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/res/analysis/all_t_rt.csv"
)

#log adjusted is what we are interested in -> data becomes a lot more normal
all_t_rt_log <- with(all_for_t_tests_log, t.test(rt_st_log, rt_mt_log))
all_t_rt_log <- tidy(all_t_rt_log)
#sig as well
write_csv(
  all_t_rt_log,
  "C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/res/analysis/all_t_rt_log.csv"
)

all_t_tj <- with(all_for_t_tests, t.test(task_jumps_mean_st, task_jumps_mean_mt))
all_t_tj <- tidy(all_t_tj)
#ns - interesting as outlierless is sig
write_csv(
  all_t_tj,
  "C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/res/analysis/all_t_tj.csv"
)


all_summary <- skim_without_charts(all_for_t_tests)
write_csv(
  all_summary,
  "C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/res/analysis/all_summary_tj_rt_ttests.csv"
)

all_log_summary <- skim_without_charts(all_for_t_tests_log)
write_csv(
  all_log_summary,
  "C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/res/analysis/all_summary_tj_rt_ttests_log.csv"
)
