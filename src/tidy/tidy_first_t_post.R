###############################################################################
########    Investigating impact on RT, TJ first trial post switch    ##########
########                    Sadie Lane, 2026                          ##########
################################################################################

rm(list=ls())
library(tidyverse)

setwd("C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/res")

switch_impact <- read_csv(
  "routine_vs_habit_sw_frst-scnd_stvmt_avg.csvv",
  na = c("", "NA")
)


# initial attempt ---------------------------------------------------------
