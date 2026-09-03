################################################################################
############    Benefits of habits, routines on multitasking    ################
############                  Sadie lane, 2026                  ################
################################################################################

rm(list=ls())
library(tidyverse)
library(broom)

setwd("C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/res")

#read in data
averages <- read_csv(
  "routine_vs_habit_avg.csv",
  na = c("", "NA")
)

perform_dat <- read_csv(
  "perform_dat_errors.csv",
  na = c("", "NA")
)

# TE and Reclicks ---------------------------------------------------------

#rt
rtcost_mod <- lm(RT_cost ~ TE + reclicks_mean, data = perform_dat)
summary(rtcost_mod)
#ns

rtcost_trsf_mod <- lm(RT_cost ~ TE + sqrt(reclicks_mean), data = perform_dat)
summary(rtcost_trsf_mod)
#ns

#task jumps (our accuracy proxy)
tjcost_mod <- lm(tj_cost ~ TE + reclicks_mean, data = perform_dat)
summary(tjcost_mod)
#ns

tjcost_trsf_mod <- lm(tj_cost ~ TE + sqrt(reclicks_mean), data = perform_dat)
summary(tjcost_trsf_mod)
#ns


# TE, reclicks and all_errors_stay ----------------------------------------

rtcost_mod_err <- lm(RT_cost ~ TE + reclicks_mean + errors_stay, data = perform_dat)
summary(rtcost_mod_err)
#ns

rtcost_trsf_mod_err <- lm(RT_cost ~ TE + sqrt(reclicks_mean) + log(errors_stay + 0.001), data = perform_dat)
summary(rtcost_mod_err)
#ns


tjcost_mod_err <- lm(tj_cost ~ TE + reclicks_mean + errors_stay, data = perform_dat)
summary(tjcost_mod_err)
#sig for errors (but with no transforms which is bad practice bc trsfs make more normal)

tjcost_trsf_mod_err <- lm(tj_cost ~ TE + sqrt(reclicks_mean) + log(errors_stay + 0.001), data = perform_dat)
summary(tjcost_trsf_mod_err)
#ns



