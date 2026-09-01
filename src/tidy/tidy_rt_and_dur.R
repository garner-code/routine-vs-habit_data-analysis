################################################################################
#########      Sadie Lane 2026 investigating rt and dur together      ##########
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

averages_rt_dur <- averages |>
  mutate(
    sum_rt_dur = rt_mean + dur_mean
  )

write_csv(averages_rt_dur, "C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/res/averages_rt_dur.csv")

#and make a df with outliers (n_nc and n_back sens) removed

exclude_sixty <- c(13, 22, 25, 28, 51, 61, 73, 76, 85)

exclude_sixfive <- c(13, 22, 25, 28, 51, 61, 63, 73, 76, 77, 84, 85)

averages_rt_dur_outless <- averages_rt_dur |>
  filter(!sub %in% exclude_sixty)

write_csv(averages_rt_dur_outless, "C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/res/averages_rt_dur_outless.csv")
