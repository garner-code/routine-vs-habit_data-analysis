## why are the bad rters bad?
#maybe demographics or cohs provides an answer...

rm(list=ls())
library(tidyverse)
library(paletteer)

setwd("C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/src")
source("plot_style.R")

#change to your wd
setwd("C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/res")

averages <- read_csv(
  "averages_democohs.csv",
  na = c("", "NA")
)

costs <- read_csv(
  "costs.csv",
  na = c("", "NA")
)


nonsense <- c(
  3, 5, 11, 12, 16, 18, 20, 24, 35, 45, 46, 56, 58, 60, 62,
  68, 70, 81, 82
)

average_costs <- inner_join(averages, costs, by = "sub")

average_costs <- average_costs |>
  filter(ses == 4, switch == 0) |>
  mutate(
    rt_group = ifelse(RT_cost < 0, "weird", "normal")
  ) |>
  relocate(sub, ses, block, switch, RT_cost, rt_group)
