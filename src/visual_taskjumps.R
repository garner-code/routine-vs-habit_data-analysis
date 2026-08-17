################################################################################
############             Sadie Lane 2026 vis task jumps           ##############
################################################################################


rm(list=ls())
library(tidyverse)
library(paletteer)
library(ggrepel)
setwd("C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/src")
source("plot_style.R")

#change to your wd
setwd("C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/res")

#read in data
averages <- read_csv(
  "routine_vs_habit_avg.csv",
  na = c("", "NA")
)

counts <- read_csv(
  "routine_vs_habit_nc_trial_counts.csv",
  na = c("", "NA")
)


# tidy --------------------------------------------------------------------

mt1 <- counts |>
  filter(ses == 4, block == "b-mt1")

median(mt1$n_nc_trials)
IQR(mt1$n_nc_trials)
quantile(mt1$n_nc_trials, 0.75)
quantile(mt1$n_nc_trials, 0.25)

6 + 1.5*6
#outliers at 15

mt2 <- counts |>
  filter(ses == 4, block == "b-mt2")

median(mt2$n_nc_trials)
IQR(mt2$n_nc_trials)
quantile(mt2$n_nc_trials, 0.75)
quantile(mt2$n_nc_trials, 0.25)

2 + 1.5*2

counts_outlierless <- counts |>
  filter(ses == 4, n_nc_trials < 15)

counts_outlierless |>
  count(block == "b-st2")


# min threshold for n_nc_trials -------------------------------------------
#n_nc_trials = total number of trials included in the task jumps analysis
#bc we exclude trials where they had ANY general errors
#to avoid a trial being a "task jump" when they were in fact just confused/lost

#boxplot
counts |>
  filter(ses == 4) |>
  ggplot(aes(x = block, y = n_nc_trials, fill = block)) +
  geom_text_repel(aes(label = sub)) +
  geom_point(alpha = 0.3) +
  geom_boxplot(alpha = 0.3) +
  theme_classic()



#violin plot
counts |>
  filter(ses == 4) |>
  ggplot(aes(x = block, y = n_nc_trials, fill = block)) +
  geom_text_repel(aes(label = sub)) +
  geom_violin() +
  theme_classic()



# plotting potential outlier cutoffs --------------------------------------

counts_outlierless |>
  filter(ses == 4) |>
  ggplot(aes(x = block, y = n_nc_trials, fill = block)) +
  geom_text_repel(aes(label = sub)) +
  geom_point(alpha = 0.3) +
  geom_boxplot(alpha = 0.3) +
  theme_classic()





