################################################################################
######                Get demographic summary statistics                ########
######                          Sadie Lane                              ########
################################################################################

rm(list=ls())
library(tidyverse)
library(knitr)
library(kableExtra)

setwd("C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/res")

#read in data
demos <- read_csv(
  "demo_cohs.csv",
  na = c("", "NA")
)

demos |>
  summarise(
    sd_age = sd(sub_age)
  )

summary(demos$sub_age)

sd(demos$sub_age, na.rm = T)
