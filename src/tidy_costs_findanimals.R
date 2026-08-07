################################################################################
############          SL + KGG vis tidy benes of reclicks, TE     ##############
################################################################################
#script to output a df for statistical analysis of costs
#of mt v st

rm(list=ls())
library(tidyverse)
setwd("C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/src")
source("plot_style.R")

#change to whatever wd is
setwd("C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/res")

#read in data
averages <- read_csv(
  "routine_vs_habit_avg.csv",
  na = c("", "NA")
)

split_by_block <- read_csv(
  "split_by_block.csv",
  na = c("","NA")
)

#create ind_predictors using the split_by_block df
ind_predictors <- split_by_block |>
  filter(block == "st") |>
  select(sub:TE)


#create costs df (difference scores in rt and acc per sub)
costs <- averages |>
  filter(ses == 4 & switch == 0) |>
  group_by(sub) |>
  summarise(
    RT_cost = rt_mean[block == "mt"] - rt_mean[block == "st"],
    acc_cost = accuracy_mean[block == "mt"] - accuracy_mean[block == "st"]
    )

#check normalcy of costs - histo, box and qq
costs |>
  ggplot(aes(x = RT_cost)) +
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
  plot_style()

perform_dat <- inner_join(ind_predictors, costs, by="sub")

write_csv(perform_dat, "C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/res/perform_dat.csv")
