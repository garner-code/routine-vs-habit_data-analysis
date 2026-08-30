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
  "averages_no_tj_no_nback_outs.csv",
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

#no transform
for_t_tests <- outlierless |>
  filter(ses == 4 & switch == 0) |>
  select(sub, block, rt_mean, accuracy_mean, task_jumps_mean) |>
  pivot_wider(
    names_from = block,
    values_from = c(rt_mean, accuracy_mean, task_jumps_mean)
  )

#log transform
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

# acc (no transform)

t_acc <- with(for_t_tests, t.test(accuracy_mean_st, accuracy_mean_mt)) |>
  tidy()

write_csv(
  t_acc,
  "C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/res/analysis/analysis_t_test_acc.csv"
)

sum_acc <- with(for_t_tests, summary(accuracy_mean_st, accuracy_mean_mt)) |>
  skim()

sum_acc <- for_t_tests |>
  skim_without_charts()

write_csv(
  sum_acc,
  "C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/res/analysis/analysis_sum_acc.csv"
)

#rt (with and without log bc normal distrib)

#without
t_rt <- with(for_t_tests, t.test(rt_mean_st, rt_mean_mt)) |>
  tidy()

write_csv(
  t_rt,
  "C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/res/analysis/analysis_t_rt.csv"
)

sum_rt <- for_t_tests |>
  skim_without_charts()

write_csv(
  sum_rt,
  "C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/res/analysis/analysis_sum_rt.csv"
)

#with log
t_rt_log <- with(for_t_tests, t.test(log(rt_mean_st), log(rt_mean_mt))) |>
  tidy()

write_csv(
  t_rt_log,
  "C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/res/analysis/analysis_t_rt_log.csv"
)

sum_log_rt <- for_t_tests_log |>
  skim_without_charts()

write_csv(
  sum_log_rt,
  "C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/res/analysis/analysis_sum_rt_log.csv"
)

#keeping above code just in case
#but accuracy has been deprecated in favour of task jumps


# now checking task jumps v reclicks --------------------------------------

#first with outliers taken out
#which as a reminder is n_nc, tjs, and sens -
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
#sig
write_csv(
  t_tj_outless,
  "C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/res/analysis/t_tj_outless.csv"
)


t_tj_outless_log <- with(for_t_tests_log, t.test(tj_st_log, tj_mt_log))
t_tj_outless_log <- tidy(t_tj_outless_log)
#sig
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

#second with all participants included

#rt has already been run (see above)

t_tj <-

t_tj_log <-

all_summary <-

all_log_summary <-


summary(averages)
