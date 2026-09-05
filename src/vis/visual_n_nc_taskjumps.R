################################################################################
############            Sadie Lane 2026 vis n_nc task jumps       ##############
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
    mean_n_nc = mean(n_nc_trials),
    mean_non = mean(n_non_nc_trials)
  )

#sd is 10.4
#grandmean is 4.72

4.72 + 2.5*10.4
#therefore 30.72

4.72 + 3*10.4
#therefore 35.92

mean(mcounts$mean_n_nc)

summary(mcounts$mean_n_nc)

#thus IQR outlier is

# min threshold for n_nc_trials -------------------------------------------
#n_nc_trials = total number of trials included in the task jumps analysis
#bc we exclude trials where they had ANY general errors
#to avoid a trial being a "task jump" when they were in fact just confused/lost

#boxplot
mcounts |>
  ggplot(aes(y = mean_n_nc)) +
  geom_boxplot(alpha = 0.3, fill = "cadetblue") +
  plot_style() +
  theme_classic() +
  geom_hline(yintercept = 6)

mcounts |>
  ggplot(aes(x = mean_n_nc)) +
  geom_histogram(binwidth = 0.5, fill = "purple") +
  scale_x_continuous(breaks = seq(0, 60, by = 5)) +
  plot_style() +
  theme_classic() +
  geom_vline(xintercept = 6.5, linetype = "dotted") +
  geom_vline(xintercept = 30.72, linetype = "dotted") +
  geom_vline(xintercept = 35.92, linetype = "dotted") +
  labs(
    subtitle = "IQR outlier is anything > 6.5, lose n = 16
    mean + 2.5sd is anything > 30.72, lose n = 3
    mean + 3sd is anything > 35.92, lose n = 2",
  )

sni
mcounts |>
  ggplot(aes(y = mean_non)) +
  geom_boxplot()

summary(mcounts$mean_non)

x <- mean(mcounts$mean_non)
y <- sd(mcounts$mean_non)

mcounts |>
  filter(mean_non < 73.5)

# plotting potential outlier cutoffs --------------------------------------

counts_outlierless |>
  filter(ses == 4) |>
  ggplot(aes(x = block, y = n_nc_trials, fill = block)) +
  geom_text_repel(aes(label = sub)) +
  geom_point(alpha = 0.3) +
  geom_boxplot(alpha = 0.3) +
  theme_classic()


mcounts |>
  filter(mean_n_nc > 30.72)


