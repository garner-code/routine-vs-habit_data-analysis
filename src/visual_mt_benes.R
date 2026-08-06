################################################################################
############          SL + KGG vis benes of reclicks, TE          ##############
################################################################################


rm(list=ls())
library(tidyverse)
library(paletteer)
setwd("C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/src")
source("plot_style.R")

#change to whatever n size is
setwd("C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/res")

#read in data
averages <- read_csv(
  "routine_vs_habit_avg.csv",
  na = c("", "NA")
)

ind_predictors <- averages |>
  select(sub:switch, reclicks_mean, TE) |>
  filter(ses == 4, block == "st") |>
  pivot_wider(
    names_from = switch,
    values_from = c("reclicks_mean", "TE")
  ) |>
  mutate(
    reclicks_mean = reclicks_mean_1,
    TE = TE_0
  ) |>
  select(sub, block, reclicks_mean, TE)

costs <- averages |>
  filter(ses == 4 & switch == 0) |>
  group_by(sub) |>
  summarise(RT_cost = rt_mean[block == "mt"] - rt_mean[block == "st"],
            acc_cost = accuracy_mean[block == "mt"] - accuracy_mean[block == "st"])

costs |> ggplot(aes(x=RT_cost)) +
  geom_histogram()

costs |>
  pivot_longer(
    cols = c("RT_cost", "acc_cost"),
    names_to = "names",
    values_to = "values"
  ) |>
  ggplot(aes(x = names, y = values)) +
  geom_boxplot() +
  plot_style() +
  theme_classic()


#qqnorm

costs |>
  pivot_longer(
    cols = c("RT_cost", "acc_cost"),
    names_to = "names",
    values_to = "values"
  ) |>
  ggplot(aes(sample = values, colour = names)) +
  geom_qq() +
  geom_qq_line() +
  theme_classic() +
  #facet_wrap()
  plot_style()

perform_dat <- inner_join(ind_predictors,
                          costs,
                          by="sub")

mod <- lm(RT_cost ~ TE + sqrt(reclicks_mean+0.0001), data=perform_dat)
summary(mod)
