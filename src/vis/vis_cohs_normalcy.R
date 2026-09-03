################################################################################
##########                      Sadie Lane 2026                  ###############
##########              checking normalcy of COHS scores         ###############
################################################################################

rm(list=ls())
library(tidyverse)
library(paletteer)
setwd("C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/src")
source("plot_style.R")

setwd("C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/res")

#read in data
avgs_cohs <- read_csv(
  "averages_democohs.csv",
  na = c("", "NA")
)

# tidy --------------------------------------------------------------------

avgs_cohs <- avgs_cohs |>
  filter(ses == 4) |>
  select(sub:switch, reclicks_mean, task_jumps_mean, rout:sub_lang) |>
  mutate(
    sub_eng = str_detect(sub_lang, "nglish")
  )


# vis ---------------------------------------------------------------------

#first want to look at duration scores
#bc if we see really non-normative data and non-normative duration I think it
#will be fair to conclude they were just kinda checked out
#or answering yes to everything


