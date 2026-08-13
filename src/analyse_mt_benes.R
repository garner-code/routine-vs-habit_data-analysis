################################################################################
############    Benefits of habits, routines on multitasking    ################
############                  Sadie lane, 2026                  ################
################################################################################

library(tidyverse)
library(skimr)

setwd("C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/res")

#read in data
averages <- read_csv(
  "averages_democohs.csv",
  na = c("", "NA")
)

perform_dat <- read_csv(
  "perform_dat.csv",
  na = c("", "NA")
)


# analyse -----------------------------------------------------------------


# Reclicks only -----------------------------------------------------------

#rt
rt_reclicks_mod <- lm(RT_cost ~ reclicks_mean, data = perform_dat)
summary(rt_reclicks_mod)
#ns

rt_reclicks_mod_sqrt <- lm(RT_cost ~ sqrt(reclicks_mean + 0.0001), data = perform_dat)
summary(rt_reclicks_mod_sqrt)
#ns

#acc

acc_reclicks_mod <- lm(acc_cost ~ reclicks_mean, data = perform_dat)
summary(acc_reclicks_mod)
#ns


# TE only -----------------------------------------------------------------

rt_TE_mod <- lm(RT_cost ~ TE, data = perform_dat)
summary(rt_TE_mod)
#ns

rt_TE_mod_sqrt <- lm(RT_cost ~ sqrt(TE + 0.0001), data = perform_dat)
summary(rt_TE_mod_sqrt)
#ns

#acc

acc_TE_mod <- lm(acc_cost ~ TE, data = perform_dat)
summary(acc_TE_mod)
#sig

#TE reliably predicts accuracy

# TE and Reclicks ---------------------------------------------------------


#rt (ns)
rt_both_mod <- lm(RT_cost ~ TE + reclicks_mean, data = perform_dat)
summary(rt_both_mod)


rt_both_mod_sqrt <- lm(RT_cost ~ TE + sqrt(reclicks_mean + 0.0001), data = perform_dat)
summary(rt_both_mod_sqrt)

#acc

acc_both_mod <- lm(acc_cost ~ TE + reclicks_mean, data = perform_dat)
summary(acc_both_mod)
#sig

#so TE and reclicks reliably predicts accuracy, but not rt.







