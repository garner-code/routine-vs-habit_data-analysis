################################################################################
############           Sadie Lane 2026 tidy n-back data           ##############
################################################################################

rm(list=ls())
library(tidyverse)

#change to your wd
setwd("C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/res")

#read in data
n_back <- read_csv(
  "routine_vs_habit_nback_sum.csv",
  na = c("", "NA")
)

long_rt_tj <- read_csv(
  "long_rt_tj.csv",
  na = c("", "NA")
)


# tidying time ------------------------------------------------------------

#very simple I just want to get 85 rows with only the mt data

n_back_mt <- n_back |>
  filter(block == "mt")

write_csv(n_back_mt, "C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/res/routine_vs_habit_nback_mt.csv")

#lets also join the data to the averages data

df <- long_rt_tj |>
  filter(block == "mt") |>
  pivot_wider(
    names_from = dv:switch,
    values_from = rt_or_tj
  ) |>
  rename(
    tjs_mean_stay = task_jumps_mean_Stay,
    rt_mean_stay = rt_mean_Stay,
    tjs_mean_switch = task_jumps_mean_Switch,
    rt_mean_switch = rt_mean_Switch
  )

n_back_averages <- inner_join(df, n_back_mt, by = "sub")

n_back_averages <- n_back_averages |>
  rename(
    block = block.x
  ) |>
  select(sub:rt_mean_switch, hits:spec)


write_csv(n_back_averages, "C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/res/routine_vs_habit_n_back_averages.csv")

#finally lets make a long form of this df
#tbd if I ever need it.

# outliers ----------------------------------------------------------------

n_back_averages |>
  filter(sens > 0.7, spec > 0.8)
