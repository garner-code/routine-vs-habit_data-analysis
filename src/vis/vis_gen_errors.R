################################################################################
#####         Visualising distribution and normalcy of gen errors         ######
#####                           Sadie Lane 2026                           ######
################################################################################

rm(list=ls())
library(tidyverse)
library(paletteer)

setwd("C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/src")
source("plot_style.R")

setwd("C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/res")

#read in data

averages <- read_csv(
  "routine_vs_habit_avg.csv",
  na = c("", "NA")
)



# tidy --------------------------------------------------------------------

ges <- averages |>
  filter(ses == 4, switch == 0) |>
  select(sub:switch, general_errors_mean)

out_check <- averages |>
  filter(ses == 4) |>
  select(sub:switch, general_errors_mean)

# qq ----------------------------------------------------------------------

ges|>
  ggplot(aes(sample = general_errors_mean)) +
  geom_qq() +
  geom_qq_line() +
  theme_classic() +
  plot_style() +
  facet_wrap( ~ block) +
  labs(
    title = "mean general errors (stay trials)",
    subtitle = "no adjust"
  )

#checked log and sqrt, best fit is no adjust

# histo -------------------------------------------------------------------

ges |>
  filter(block == "st") |>
  ggplot(aes(x = general_errors_mean)) +
  geom_histogram(binwidth = 0.001) +
  theme_classic() +
  plot_style() +
  geom_vline(xintercept = IQR_out_ges, linetype = 3) +
  geom_vline(xintercept = sd_out_twofive, linetype = 3) +
  geom_vline(xintercept = sd_out_three, linetype = 3) +
  labs(
    title = "ST, mean general errors (stay trials)",
    subtitle = "no adjust, IQR then 2.5 then 3"
  )


ges |>
  filter(block == "mt") |>
  ggplot(aes(x = general_errors_mean)) +
  geom_histogram(binwidth = 0.001) +
  theme_classic() +
  plot_style() +
  geom_vline(xintercept = IQR_out_ges, linetype = 3) +
  geom_vline(xintercept = sd_out_twofive, linetype = 3) +
  geom_vline(xintercept = sd_out_three, linetype = 3) +
  labs(
    title = "MT, mean general errors (stay trials)",
    subtitle = "no adjust, IQR then 2.5 then 3"
  )

summary(out_check)
IQR_ges <- IQR(out_check$general_errors_mean)
IQR_out_ges <- 0.0202 + 1.5*IQR_ges

sd_ges <- sd(out_check$general_errors_mean)
sd_out_twofive <- 0.0202 + 2.5*sd_ges
sd_out_three <- 0.0202 + 3*sd_ges

