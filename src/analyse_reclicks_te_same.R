################################################################################
############    To what extent are reclicks and te the same?    ################
############                  Sadie lane, 2026                  ################
################################################################################

library(tidyverse)
library(paletteer)
setwd("C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/src")
source("function_safe_se.R")
source("plot_style.R")

setwd("C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/res")

#read in data
averages <- read_csv(
  "averages_democohs.csv",
  na = c("", "NA")
)


# test outliers -----------------------------------------------------------

averages_reclicks <- averages |>
  filter(ses == 4, switch == 1) |>
  group_by(sub) |>
  mutate(
    reclicks_sd = sd(reclicks_mean)
  )

averages_reclicks |>
  filter(reclicks_sd > 3)
#so sub 30 is outlier reclicks

averages_te <- averages |>
  filter(ses == 4, switch == 0) |>
  group_by(sub) |>
  mutate(
    TE_sd = sd(TE)
  )

averages_te |>
  filter(TE_sd > 2.5) |>
  relocate(TE_sd)
  #so no outliers for TE
  #highly highly dense data


# tidy/transform to fit normal distrib -----------------------------------------



# Analyse -----------------------------------------------------------------




