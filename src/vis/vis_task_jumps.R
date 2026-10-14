################################################################################
############            Sadie Lane 2026 vis task jumps            ##############
################################################################################


rm(list=ls())
library(tidyverse)
library(paletteer)
library(ggrepel)
library(performance)
setwd("C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/src")
source("plot_style.R")

#change to your wd
setwd("C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/res")

tjs_no_outs <- read_csv(
  "averages_no_tj_outs.csv",
  na = c("", "NA")
)
#quick tidy

tjs_no_outs <- tjs_no_outs |>
  filter(ses == 4) |>
  select(sub, ses, block, switch, task_jumps_mean)

tj_mt_switch <- tjs_no_outs |>
  filter(block == "mt", switch == 1)


tj_mt_stay <- tjs_no_outs |>
  filter(block == "mt", switch == 0)


tj_st_switch <- tjs_no_outs |>
  filter(block == "st", switch == 1)

tj_st_stay <- tjs_no_outs |>
  filter(block == "st", switch == 0)


# histograms -------------------------------------------------------------------

#with the outliers that were cut for not having enough trials to count task jumps
#we now need to visualise the actual task jump data itself for outliers.
#so without data from subs 8 9 and 11


################   tj_mt_switch
summary(tj_mt_switch$task_jumps_mean)
tj_mt_switch_iqr <- IQR(tj_mt_switch$task_jumps_mean, na.rm = T)
tj_mt_switch_iqr_out <- 0.1429 + 1.5*tj_mt_switch_iqr

tj_mt_switch_sd <- sd(tj_mt_switch$task_jumps_mean, na.rm = T)
tj_mt_switch_sd_2.5 <- 0.1413 + 2.5*tj_mt_switch_sd
tj_mt_switch_sd_3 <- 0.1413 + 3*tj_mt_switch_sd

tj_mt_switch |>
  ggplot(aes(x = task_jumps_mean)) +
  geom_histogram(binwidth = 0.01, colour = "magenta", fill = "magenta") +
  plot_style() +
  theme_classic() +
  geom_vline(xintercept = tj_mt_switch_iqr_out, linetype = 3) +
  geom_vline(xintercept = tj_mt_switch_sd_2.5, linetype = 3) +
  geom_vline(xintercept = tj_mt_switch_sd_3, linetype = 3) +
  labs(
    title = "mt_switch",
    subtitle = "in order: iqr, 2.5sd and 3sd"
  )

################   tj_mt_stay
summary(tj_mt_stay$task_jumps_mean)
tj_mt_stay_iqr <- IQR(tj_mt_stay$task_jumps_mean, na.rm = T)
tj_mt_stay_iqr_out <- 0.1606 + 1.5*tj_mt_stay_iqr

tj_mt_stay_sd <- sd(tj_mt_stay$task_jumps_mean, na.rm = T)
tj_mt_stay_sd_2.5 <- 0.19155 + 2.5*tj_mt_stay_sd
tj_mt_stay_sd_3 <- 0.19155 + 3*tj_mt_stay_sd

tj_mt_stay |>
  ggplot(aes(x = task_jumps_mean)) +
  geom_histogram(binwidth = 0.01, colour = "magenta", fill = "magenta") +
  plot_style() +
  theme_classic() +
  geom_vline(xintercept = tj_mt_stay_iqr_out, linetype = 3) +
  geom_vline(xintercept = tj_mt_stay_sd_2.5, linetype = 3) +
  geom_vline(xintercept = tj_mt_stay_sd_3, linetype = 3) +
  labs(
    title = "mt_stay",
    subtitle = "in order: iqr, 2.5sd and 3sd"
  )

################   tj_st_switch

summary(tj_st_switch$task_jumps_mean)
tj_st_switch_iqr <- IQR(tj_st_switch$task_jumps_mean, na.rm = T)
tj_st_switch_iqr_out <- 0.125 + 1.5*tj_st_switch_iqr

tj_st_switch_sd <- sd(tj_st_switch$task_jumps_mean, na.rm = T)
tj_st_switch_sd_2.5 <- 0.1319 + 2.5*tj_st_switch_sd
tj_st_switch_sd_3 <- 0.1319 + 3*tj_st_switch_sd


tj_st_switch |>
  ggplot(aes(x = task_jumps_mean)) +
  geom_histogram(binwidth = 0.01, colour = "magenta", fill = "magenta") +
  plot_style() +
  theme_classic() +
  geom_vline(xintercept = tj_st_switch_iqr_out, linetype = 3) +
  geom_vline(xintercept = tj_st_switch_sd_2.5, linetype = 3) +
  geom_vline(xintercept = tj_st_switch_sd_3, linetype = 3) +
  labs(
    title = "st_switch",
    subtitle = "in order: iqr, 2.5sd and 3sd"
  )


################   tj_st_stay

summary(tj_st_stay$task_jumps_mean)
tj_st_stay_iqr <- IQR(tj_st_stay$task_jumps_mean, na.rm = T)
tj_st_stay_iqr_out <- 0.069 + 1.5*tj_st_stay_iqr

tj_st_stay_sd <- sd(tj_st_stay$task_jumps_mean, na.rm = T)
tj_st_stay_sd_2.5 <- 0.1438 + 2.5*tj_st_stay_sd
tj_st_stay_sd_3 <- 0.1438 + 3*tj_st_stay_sd

tj_st_stay |>
  ggplot(aes(x = task_jumps_mean)) +
  geom_histogram(binwidth = 0.01, colour = "magenta", fill = "magenta") +
  plot_style() +
  theme_classic() +
  geom_vline(xintercept = tj_st_stay_iqr_out, linetype = 3) +
  geom_vline(xintercept = tj_st_stay_sd_2.5, linetype = 3) +
  geom_vline(xintercept = tj_st_stay_sd_3, linetype = 3) +
  labs(
    title = "st_stay",
    subtitle = "in order: iqr, 2.5sd and 3sd"
  )


#in summary, mean + 2.5*sd seems appropriate for where to cut out people who
#task jump too often



# make dfs with tj outliers cut -------------------------------------------

tj_mt_switch |>
  filter(task_jumps_mean > tj_mt_switch_sd_2.5)
#sub 2, 73, 79, 81, 84

tj_mt_stay |>
  filter(task_jumps_mean > tj_mt_stay_sd_2.5)
#73, 79, 81

tj_st_switch |>
  filter(task_jumps_mean > tj_st_switch_sd_2.5)
#2, 13, 73

tj_st_stay |>
  filter(task_jumps_mean > tj_st_stay_sd_2.5)
#2, 13, 79, 81

exclude <- c(6:8, 51, 52, 289, 290, 292, 313:315, 321:323, 334)

tjs_no_outs_post <- tjs_no_outs[exclude, "task_jumps_mean"] <- NA

write_csv(tjs_no_outs_post, "C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/res/no_tj_outs_nnc_meantj.csv")


# now qqnorms -------------------------------------------------------------


#without cuts
tjs_no_outs |>
  ggplot(aes(sample = task_jumps_mean)) +
  geom_qq(colour = "magenta") +
  geom_qq_line() +
  theme_classic() +
  facet_grid(block ~ switch) +
  plot_style() +
  labs(
    title = "only n_nc data cut",
    subtitle = "no transform"
  )

tjs_no_outs |>
  ggplot(aes(sample = sqrt(task_jumps_mean))) +
  geom_qq(colour = "magenta") +
  geom_qq_line() +
  theme_classic() +
  facet_grid(block ~ switch) +
  plot_style() +
  labs(
    title = "only n_nc data cut",
    subtitle = "sqrt transform"
  )

tjs_no_outs |>
  ggplot(aes(sample = log(task_jumps_mean + 0.001))) +
  geom_qq(colour = "magenta") +
  geom_qq_line() +
  theme_classic() +
  facet_grid(block ~ switch) +
  plot_style() +
  labs(
    title = "only n_nc data cut",
    subtitle = "log transform"
  )


#with cuts
tjs_no_outs_post |>
  ggplot(aes(sample = task_jumps_mean)) +
  geom_qq(colour = "magenta") +
  geom_qq_line() +
  theme_classic() +
  facet_grid(block ~ switch) +
  plot_style() +
  labs(
    title = "n_nc and tjouts cut",
    subtitle = "no transform"
  )

tjs_no_outs_post |>
  ggplot(aes(sample = sqrt(task_jumps_mean))) +
  geom_qq(colour = "magenta") +
  geom_qq_line() +
  theme_classic() +
  facet_grid(block ~ switch) +
  plot_style() +
  labs(
    title = "n_nc and tjouts cut",
    subtitle = "sqrt transform"
  )

tjs_no_outs_post |>
  ggplot(aes(sample = log(task_jumps_mean + 0.001))) +
  geom_qq(colour = "magenta") +
  geom_qq_line() +
  theme_classic() +
  facet_grid(block ~ switch) +
  plot_style() +
  labs(
    title = "n_nc and tjouts cut",
    subtitle = "log transform"
  )




