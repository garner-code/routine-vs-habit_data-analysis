################################################################################
#####            script to vis reclicks and te at st v mt               ########
#####                      Sadie Lane, 2026                             ########
################################################################################

rm(list=ls())
library(tidyverse)
library(paletteer)
setwd("C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/src")
source("plot_style.R")

#change to your wd
setwd("C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/res")

#read in data
split_by_block <- read_csv(
  "split_by_block.csv",
  na = c("", "NA")
)


# vis ---------------------------------------------------------------------

split_by_block |>
  select(sub, block, reclicks_mean, TE) |>
  pivot_longer(
    cols = c("reclicks_mean", "TE"),
    names_to = "dv",
    values_to = "score"
  ) |>
  filter(dv == "TE") |>
  ggplot(aes(y = block, x = score)) +
  geom_boxplot(width = 0.2, fill = "blue") +
  geom_violin(alpha = 0.3, fill = "blue") +
  plot_style()


#more nuanced look at TE
split_by_block |>
  select(sub, block, reclicks_mean, TE) |>
  pivot_longer(
    cols = c("reclicks_mean", "TE"),
    names_to = "dv",
    values_to = "score"
  ) |>
  filter(dv == "TE") |>
  ggplot(aes(y = score, x = dv)) +
  geom_boxplot() +
  geom_violin(alpha = 0.3) +
  plot_style()


split_by_block |>
  select(sub, block, reclicks_mean, TE) |>
  pivot_wider(
    names_from = "block",
    values_from = c("reclicks_mean", "TE")
  ) |>
  ggplot(aes(y = score, x = dv)) +
  geom_boxplot() +
  geom_violin(alpha = 0.3) +
  plot_style()


