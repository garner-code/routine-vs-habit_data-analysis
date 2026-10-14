################################################################################
#########      Sadie Lane 2026 investigating rt and dur together      ##########
################################################################################

rm(list=ls())
library(tidyverse)
setwd("C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/src")
source("plot_style.R")

#change to your wd
setwd("C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/res")

#read in data
averages <- read_csv(
  "averages_rt_dur.csv",
  na = c("", "NA")
)
#quick tidy
rt_dur <- averages |>
  filter(ses == 4) |>
  select(sub:switch, sum_rt_dur)


#first qq
rt_dur |>
  ggplot(aes(sample = sum_rt_dur)) +
  geom_qq() +
  geom_qq_line() +
  theme_classic() +
  plot_style()

rt_dur |>
  ggplot(aes(sample = sqrt(sum_rt_dur))) +
  geom_qq() +
  geom_qq_line() +
  theme_classic() +
  plot_style()

#run with and without sqrt trsf

#next histograms

rt_dur |>
  ggplot(aes(x = sum_rt_dur)) +
  geom_histogram(binwidth = 0.01) +
  theme_classic() +
  plot_style()

rt_dur |>
  ggplot(aes(x = sum_rt_dur)) +
  geom_boxplot() +
  theme_classic() +
  plot_style()



