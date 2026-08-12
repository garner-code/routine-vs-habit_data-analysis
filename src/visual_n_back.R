################################################################################
############          Sadie Lane 2026 visualise n-back data       ##############
################################################################################

rm(list=ls())
library(tidyverse)
library(paletteer)
setwd("C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/src")
source("plot_style.R")

#change to your wd
setwd("C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/res")

#read in data
n_back <- read_csv(
  "routine_vs_habit_nback_mt.csv",
  na = c("", "NA")
)


# distribution ----------------------------------------------------------------

#obviously non-normal bc they had to hit a certain bar to be good enough to
#participate

#so lets make a boxplot

n_back |>
  pivot_longer(
    cols = c("sens", "spec"),
    names_to = "DV",
    values_to = "Score"
  ) |>
  ggplot() +
  geom_boxplot(aes(x = DV, y = Score, fill = DV)) +
  theme_classic() +
  plot_style() +
  scale_fill_paletteer_d("wesanderson::Darjeeling1")

ggsave(
  "box_n_back_mt.png",
  path = ("C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/plots"),
  width = 14,
  height = 8
)

#indicates the following are outliers (per IQR)
n_back |>
  filter(sens < 0.45)

n_back |>
  filter(spec < 0.82)

#qq plot

n_back |>
  pivot_longer(
    cols = c("sens", "spec"),
    names_to = "DV",
    values_to = "Score"
  ) |>
  ggplot(aes(sample = Score, colour = DV)) +
  geom_qq() +
  geom_qq_line() +
  theme_classic() +
  plot_style() +
  scale_colour_paletteer_d("wesanderson::Darjeeling1")

ggsave(
  "qq_n_back_mt.png",
  path = ("C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/plots"),
  width = 14,
  height = 8
)
