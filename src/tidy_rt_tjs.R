################################################################################
############     Sadie Lane 2026 tidy response time, acc dif       #############
################################################################################

rm(list=ls())
library(tidyverse)

#change to your wd
setwd("C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/res")

#read in data
averages <- read_csv(
  "routine_vs_habit_avg.csv",
  na = c("", "NA")
)


# long --------------------------------------------------------------------

long_rt_tj <- averages |>
  filter(ses == 4) |>
  group_by(sub, block, switch) |>
  mutate(
    switch = factor(
      switch, c(0, 1), c("Stay", "Switch")
    ),
    Block = factor(
      block, c("mt", "st"), c("Multitasking", "Singletasking")
    )
  ) |>
  select(sub:switch, task_jumps_mean, rt_mean) |>
  pivot_longer(
    cols = c("task_jumps_mean", "rt_mean"),
    names_to = "dv",
    values_to = "rt_or_tj"
  )

write_csv(long_rt_tj, "C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/res/long_rt_tj.csv")


# wide --------------------------------------------------------------------

#tbd if I ever need it
