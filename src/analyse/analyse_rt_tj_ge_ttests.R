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

avgs_sum_rt_dur <- read_csv(
  "averages_rt_dur.csv",
  na = c("", "NA")
)

outlierless <- read_csv(
  "averages_rt_dur_outless.csv",
  na = c("", "NA")
)

# tidy data --------------------------------------------------------------------

#no transform, outliers included
all_for_t_tests <- avgs_sum_rt_dur |>
  filter(ses == 4 & switch == 0) |>
  select(sub, block, rt_mean, accuracy_mean, task_jumps_mean, general_errors_mean, sum_rt_dur) |>
  pivot_wider(
    names_from = block,
    values_from = c(rt_mean, accuracy_mean, task_jumps_mean, general_errors_mean, sum_rt_dur)
  )

#log transform, outliers included
all_for_t_tests_trsf <- all_for_t_tests |>
  mutate(
    rt_mt_log          = log(rt_mean_mt + 0.001),
    rt_st_log          = log(rt_mean_st + 0.001),
    tj_mt_log          = log(task_jumps_mean_mt + 0.001),
    tj_st_log          = log(task_jumps_mean_st + 0.001),
    sum_rt_dur_mt_sqrt = sqrt(sum_rt_dur_mt),
    sum_rt_dur_st_sqrt = sqrt(sum_rt_dur_st)
  ) |>
  select(sub, rt_mt_log, rt_st_log, tj_mt_log, tj_st_log, sum_rt_dur_mt_sqrt, sum_rt_dur_st_sqrt)



#no transform, no outliers
for_t_tests <- outlierless |>
  filter(ses == 4 & switch == 0) |>
  select(sub, block, rt_mean, accuracy_mean, task_jumps_mean, general_errors_mean, sum_rt_dur) |>
  pivot_wider(
    names_from = block,
    values_from = c(rt_mean, accuracy_mean, task_jumps_mean, general_errors_mean, sum_rt_dur)
  )

#log transform, no outliers
for_t_tests_trsf <- for_t_tests |>
  mutate(
    rt_mt_log          = log(rt_mean_mt + 0.001),
    rt_st_log          = log(rt_mean_st + 0.001),
    tj_mt_log          = log(task_jumps_mean_mt + 0.001),
    tj_st_log          = log(task_jumps_mean_st + 0.001),
    sum_rt_dur_mt_sqrt = sqrt(sum_rt_dur_mt),
    sum_rt_dur_st_sqrt = sqrt(sum_rt_dur_st)
  ) |>
  select(sub, rt_mt_log, rt_st_log, tj_mt_log, tj_st_log, sum_rt_dur_mt_sqrt, sum_rt_dur_st_sqrt)


# analyse ----------------------------------------------------------------------
# run t tests and summaries. Output into csvs. #n.b. this is all for switch = 0.
# rt, task jumps and ges


#first with outliers taken out
#which as a reminder is n_nc, and sens -
#(n-back, i.e. they weren't really mting)

t_rt_outless <- with(for_t_tests, t.test(rt_mean_st, rt_mean_mt, paired = T))
t_rt_outless <- tidy(t_rt_outless)
#sig
write_csv(
  t_rt_outless,
  "C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/res/analysis/t_rt_outless.csv"
)

t_rt_outless_trsf <- with(for_t_tests_trsf, t.test(rt_st_log, rt_mt_log, paired = T))
t_rt_outless_trsf <- tidy(t_rt_outless_trsf)
#sig
write_csv(
  t_rt_outless_trsf,
  "C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/res/analysis/t_rt_outless_trsf.csv"
)

t_tj_outless <- with(for_t_tests, t.test(task_jumps_mean_st, task_jumps_mean_mt, paired = T))
t_tj_outless <- tidy(t_tj_outless)
#sig
write_csv(
  t_tj_outless,
  "C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/res/analysis/t_tj_outless.csv"
)


t_tj_outless_trsf <- with(for_t_tests_trsf, t.test(tj_st_log, tj_mt_log, paired = T))
t_tj_outless_trsf <- tidy(t_tj_outless_trsf)
#sig with 0.6 (very much so)
write_csv(
  t_tj_outless_trsf,
  "C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/res/analysis/t_tj_outless_trsf.csv"
)

#ge
t_ge_outless <- with(for_t_tests, t.test(general_errors_mean_st, general_errors_mean_mt, paired = T))
t_ge_outless <- tidy(t_ge_outless)
#sig (as expected)

write_csv(
  t_ge_outless,
  "C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/res/analysis/t_ge_outless.csv"
)

#and get summary stats
outless_summary <- skim_without_charts(for_t_tests)
write_csv(
  outless_summary,
  "C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/res/analysis/outless_summary.csv"
)

outless_trsf_summary <- skim_without_charts(for_t_tests_trsf)
write_csv(
  outless_trsf_summary,
  "C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/res/analysis/outless_trsf_summary.csv"
)


# now with all participants included -------------------------------------

all_t_rt <- with(all_for_t_tests, t.test(rt_mean_st, rt_mean_mt, paired = T))
all_t_rt <- tidy(all_t_rt)
#sig
write_csv(
  all_t_rt,
  "C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/res/analysis/all_t_rt.csv"
)

#log adjusted is what we are interested in -> data becomes a lot more normal
all_t_rt_trsf <- with(all_for_t_tests_trsf, t.test(rt_st_log, rt_mt_log, paired = T))
all_t_rt_trsf <- tidy(all_t_rt_trsf)
#sig as well
write_csv(
  all_t_rt_trsf,
  "C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/res/analysis/all_t_rt_trsf.csv"
)

all_t_tj <- with(all_for_t_tests, t.test(task_jumps_mean_st, task_jumps_mean_mt))
all_t_tj <- tidy(all_t_tj)
#ns - interesting as outlierless is sig
write_csv(
  all_t_tj,
  "C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/res/analysis/all_t_tj.csv"
)

#now ge
all_t_ge <- with(all_for_t_tests, t.test(general_errors_mean_st, general_errors_mean_mt, paired = T))
all_t_ge <- tidy(all_t_ge)
#sig

write_csv(
  all_t_ge,
  "C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/res/analysis/all_t_ge.csv"
)

all_summary <- skim_without_charts(all_for_t_tests)
write_csv(
  all_summary,
  "C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/res/analysis/all_summary_tj_rt_ttests.csv"
)

all_trsf_summary <- skim_without_charts(all_for_t_tests_trsf)
write_csv(
  all_trsf_summary,
  "C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/res/analysis/all_summary_tj_rt_ttests_trsf.csv"
)


# checking if sum_rt_dur solves our problems ------------------------------
#with and without outs
#with and without sqrt trsf

all_sum_rt_dur <- with(all_for_t_tests, t.test(sum_rt_dur_st, sum_rt_dur_mt, paired = T))
all_sum_rt_dur <- tidy(all_sum_rt_dur)
#sig

all_sum_rt_dur_trsf <- with(all_for_t_tests_trsf, t.test(sum_rt_dur_st_sqrt, sum_rt_dur_mt_sqrt, paired = T))
all_sum_rt_dur_trsf <- tidy(all_sum_rt_dur_trsf)
#sig


#now with n_nc and sens < 0.6 outliers

sum_rt_dur_outless <- with(for_t_tests, t.test(sum_rt_dur_st, sum_rt_dur_mt, paired = T))
sum_rt_dur_outless <- tidy(sum_rt_dur_outless)
#sig

sum_rt_dur_outless_trsf <- with(for_t_tests_trsf, t.test(sum_rt_dur_st_sqrt, sum_rt_dur_mt_sqrt, paired = T))
sum_rt_dur_outless_trsf <- tidy(sum_rt_dur_outless_trsf)
#sig

#out of curiosity im gonna get rid of the 0.65 and 0.7 peeps
#and test them

sixfive <- c(13, 22, 25, 28, 51, 61, 63, 73, 76, 77, 84, 85)

seventy <- c(10, 13, 22, 25, 28, 42, 51, 55, 61, 63, 73, 76, 77, 78, 84, 85)

df1 <- all_for_t_tests |>
  filter(!sub %in% sixfive)

df1_trsf <- all_for_t_tests_trsf |>
  filter(!sub %in% sixfive)

df2 <- all_for_t_tests |>
  filter(!sub %in% seventy)

df2_trsf <- all_for_t_tests_trsf |>
  filter(!sub %in% seventy)

#trs rt tj and ge

with(df1_trsf, t.test(rt_st_log, rt_mt_log, paired = T))
#sig

with(df1_trsf, t.test(tj_st_log, tj_mt_log, paired = T))
#still sig

ge_sixfive <- with(df1, t.test(general_errors_mean_st, general_errors_mean_mt, paired = T))
tidy(ge_sixfive)
#still sig

########sum rt_dur

with(df1, t.test(sum_rt_dur_st, sum_rt_dur_mt, paired = T))
#sig

with(df1_trsf, t.test(sum_rt_dur_st_sqrt, sum_rt_dur_mt_sqrt, paired = T))
#sig

with(df2, t.test(sum_rt_dur_st, sum_rt_dur_mt, paired = T))
#sig

with(df2_trsf, t.test(sum_rt_dur_st_sqrt, sum_rt_dur_mt_sqrt, paired = T))
#sig
