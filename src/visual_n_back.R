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

n_back_averages <- read_csv(
  "routine_vs_habit_n_back_averages.csv",
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


# potential outlier cut offs ----------------------------------------------
#the rationale is we want proof that they are in fact multitasking
#i am not too concerned with if they are doing it super well
#just well enough to indicate they are trying

n_back |>
  ggplot(aes(x = sens)) +
  geom_histogram(binwidth = 0.01, fill = "#FF0000FF") +
  plot_style() +
  theme_classic() +
  geom_vline(xintercept = 0.6, linetype = 3) +
  geom_vline(xintercept = 0.55, linetype = 3) +
  labs(
    subtitle = "Heathcote et al. 2014 uses > 0.55 (yields n = 79)
    0.6 yields n = 76
    iqr cut off is too low (0.45 i.e. less than random)"
  )

n_back |>
  ggplot(aes(x = spec)) +
  geom_histogram(binwidth = 0.01, fill = "#00A08AFF") +
  plot_style() +
  theme_classic() +
  labs(
    subtitle = "include all?"
  )

