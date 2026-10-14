################################################################################
######          Analyse my cohs data lm against reclicks and te         ########
######                          Sadie Lane                              ########
################################################################################

rm(list=ls())
library(tidyverse)
library(broom)
setwd("C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/src")
source("plot_style.R")

setwd("C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/res")

#read in data
split_by_block <- read_csv(
  "split_by_block.csv",
  na = c("", "NA")
)

# tidy --------------------------------------------------------------------

st_split <- split_by_block |>
  filter(block == "st")

#tranforms
  #reclicks sqrted
  #TE - no transform
  #auto - no transform
  #rout - log transform

#Automaticity

auto_trsf <- lm(auto ~ sqrt(reclicks_mean) + TE, data = st_split)
auto_trsf <- tidy(auto_trsf)

write_csv(auto_trsf, "C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/res/analysis/lm_auto_trsf.csv")

rout_trsf <- lm(log(rout) ~ sqrt(reclicks_mean) + TE, data = st_split)
rout_trsf <- tidy(rout_trsf)

write_csv(rout_trsf, "C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/res/analysis/lm_rout_trsf.csv")

