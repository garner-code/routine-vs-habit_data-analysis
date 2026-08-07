################################################################################
############    Benefits of habits, routines on multitasking    ################
############                  Sadie lane, 2026                  ################
################################################################################

library(tidyverse)

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

mod <- lm(RT_cost ~ TE + sqrt(reclicks_mean + 0.0001), data = perform_dat)
summary(mod)

