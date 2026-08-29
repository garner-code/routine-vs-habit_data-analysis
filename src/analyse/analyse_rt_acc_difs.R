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
  "averages_democohs_no_tj_outs.csv",
  na = c("", "NA")
)

outlierless <- read_csv(
  "routine_vs_habit_n_back_averages.csv",
  na = c("", "NA")
)

# tidy data --------------------------------------------------------------------

#take out outliers from the averages data
take_out <- c(8, 9, 11, 13, 22, 25, 28, 51, 61, 73, 76, 85)

averages <- averages %>%
  filter(!sub %in% take_out)

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


for_t_tests <- averages |>
  filter(ses == 4 & switch == 0) |>
  select(sub, block, rt_mean, accuracy_mean, task_jumps_mean) |>
  pivot_wider(
    names_from = block,
    values_from = c(rt_mean, accuracy_mean, task_jumps_mean)
  )

#### for Sadie
for_t_tests_log <- for_t_tests |>
  mutate(
    rt_mt_log  = log(rt_mean_mt),
    rt_st_log  = log(rt_mean_st),
    acc_mt_log = log(accuracy_mean_mt),
    acc_st_log = log(accuracy_mean_st),
  ) |>
  select(sub, rt_mt_log:acc_st_log)

averages |>
  filter(sub == 8) |>
  relocate(task_jumps_mean)

# analyse ----------------------------------------------------------------------
# run t tests and summaries. Output into csvs. #n.b. this is all for switch = 0.

# acc (no transform)

t_acc <- with(for_t_tests, t.test(accuracy_mean_st, accuracy_mean_mt)) |>
  tidy()

write_csv(
  t_acc,
  "C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/res/analysis_t_test_acc.csv"
)

sum_acc <- with(for_t_tests, summary(accuracy_mean_st, accuracy_mean_mt)) |>
  skim()

sum_acc <- for_t_tests |>
  skim_without_charts()

write_csv(
  sum_acc,
  "C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/res/analysis_sum_acc.csv"
)

#rt (with and without log bc normal distrib)

#without
t_rt <- with(for_t_tests, t.test(rt_mean_st, rt_mean_mt)) |>
  tidy()

write_csv(
  t_rt,
  "C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/res/analysis_t_rt.csv"
)


sum_rt <- for_t_tests |>
  skim_without_charts()

write_csv(
  sum_rt,
  "C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/res/analysis_sum_rt.csv"
)

#with log
t_rt_log <- with(for_t_tests, t.test(log(rt_mean_st), log(rt_mean_mt))) |>
  tidy()

write_csv(
  t_rt_log,
  "C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/res/analysis_t_rt_log.csv"
)


sum_log_rt <- for_t_tests_log |>
  skim_without_charts()

write_csv(
  sum_log_rt,
  "C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/res/analysis_sum_rt_log.csv"
)


# now checking task jumps v reclicks --------------------------------------


