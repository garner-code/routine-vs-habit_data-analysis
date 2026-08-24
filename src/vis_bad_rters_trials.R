## why are the bad rters bad?

rm(list=ls())
library(tidyverse)
library(paletteer)

setwd("C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/src")
source("plot_style.R")

#change to your wd
setwd("C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/res")

trials <- read_csv(
  "routine_vs_habit_trl.csv",
  na = c("", "NA")
)

costs <- read_csv(
  "costs.csv",
  na = c("", "NA")
)

trl_costs <- inner_join(trials, costs, by = "sub")

trl_costs <- trl_costs |>
  filter(ses == 4) |>
  mutate(
    rt_group = ifelse(RT_cost < 0, "weird", "normal")
  )
