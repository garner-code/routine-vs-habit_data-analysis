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

mcounts <- counts |>
  filter(ses == 4) |>
  group_by(sub) |>
  summarise(
    mean_n_nc = mean(n_nc_trials)
  )

summary(mcounts$mean_n_nc)

# min threshold for n_nc_trials -------------------------------------------
#n_nc_trials = total number of trials included in the task jumps analysis
#bc we exclude trials where they had ANY general errors
#to avoid a trial being a "task jump" when they were in fact just confused/lost

#boxplot
mcounts |>
  ggplot(aes(y = mean_n_nc)) +
  geom_boxplot(alpha = 0.3, fill = "cadetblue") +
  plot_style() +
  theme_classic()

mcounts |>
  ggplot(aes(y = mean_n_nc, x = sub))
  geom_histogram(binwidth = 0.01)

#calc outliers


# plotting potential outlier cutoffs --------------------------------------

counts_outlierless |>
  filter(ses == 4) |>
  ggplot(aes(x = block, y = n_nc_trials, fill = block)) +
  geom_text_repel(aes(label = sub)) +
  geom_point(alpha = 0.3) +
  geom_boxplot(alpha = 0.3) +
  theme_classic()





