################################################################################
########            Did type of task lead impact rt, acc?               ########
########                    Sadie lane, 2026                            ########
################################################################################

library(tidyverse)
library(paletteer)
library(afex)
library(emmeans)
library(PsyR)
setwd("C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/src")
source("function_safe_se.R")
source("plot_style.R")

setwd("C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/res")

#read in data
averages <- read_csv(
  "averages_democohs.csv",
  na = c("", "NA")
)

#run a MANOVA

# tidy data ---------------------------------------------------------------


# analyse -----------------------------------------------------------------


