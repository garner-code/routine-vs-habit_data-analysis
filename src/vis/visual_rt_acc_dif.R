################################################################################
############     Sadie Lane 2026 vis response time, acc dif       ##############
################################################################################

rm(list=ls())
library(tidyverse)
library(paletteer)
setwd("C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/src")
source("plot_style.R")

#change to your wd
setwd("C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/res")

#read in data
averages <- read_csv(
  "routine_vs_habit_avg.csv",
  na = c("", "NA")
)

long_rt_acc <- read_csv(
  "long_rt_acc.csv",
  na = c("", "NA")
)

# tidy some thigns --------------------------------------------------------


# plot thigns -------------------------------------------------------------

averages |>
  filter(ses == 4) |>
  group_by(sub, block, switch) |>
  mutate(
    switch = factor(switch, c(0, 1), c("Stay", "Switch")),
    block = factor(block, c("st", "mt"), c("Singletasking", "Multitasking"))
  ) |>
  summarise(
    rt_mean = mean(rt_mean)
  ) |>
  ggplot(aes(x = block, y = rt_mean, colour = block, fill = block)) +
  geom_violin(alpha = 0.3) +
  geom_point() +
  geom_line(aes(group = sub), alpha = 0.5, colour = "grey") +
  stat_summary(fun = "mean", geom = "point", color = "black", size = 2) +
  scale_color_paletteer_d("vangogh::Cypresses") +
  scale_fill_paletteer_d("vangogh::Cypresses") +
  facet_grid(. ~ switch) +
  theme_classic() +
  plot_style() +
  theme(
    strip.background = element_rect(fill = "white", color = "white", linewidth = 0.5),
    axis.text.x = element_blank()
  ) +
  ylim(c(0.1, 0.9)) +
  labs(
    title = "Reaction Time Manipulation Works",
    subtitle = "Means\nStay: mt = 0.449, st = 0.412 \nSwitch: mt = 0.518, st = 0.494",
    x = "Block",
    y = "Mean Response Time (ms)",
    colour = "Block",
    fill = "Block"
  )

ggsave(
  "rt_dif.png",
  path = ("C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/plots"),
  width = 8,
  height = 8,
)

#now task jumps

averages |>
  filter(ses == 4) |>
  group_by(sub, block, switch) |>
  mutate(
    switch = factor(switch, c(0, 1), c("Stay", "Switch")),
    block = factor(block, c("st", "mt"), c("Singletasking", "Multitasking"))
  ) |>
  summarise(
    task_jumps_mean = mean(task_jumps_mean)
  ) |>
  ggplot(aes(x = block, y = task_jumps_mean, colour = block, fill = block)) +
  geom_violin(alpha = 0.3) +
  geom_point() +
  geom_line(aes(group = sub), alpha = 0.5, colour = "grey") +
  stat_summary(fun = "mean", geom = "point", color = "black", size = 2) +
  scale_color_paletteer_d("vangogh::Cypresses") +
  scale_fill_paletteer_d("vangogh::Cypresses") +
  facet_grid(. ~ switch) +
  theme_classic() +
  plot_style() +
  theme(
    strip.background = element_rect(fill = "white", color = "white", linewidth = 0.5),
    axis.text.x = element_blank()
  ) +
  labs(
    title = "RT dif is not due to accuracy",
    subtitle = "Means\nStay: mt = 0.920, st = 0.952 \nSwitch: mt = 0.323,st = 0.351",
    x = "Block",
    y = "Task Jumps",
    colour = "Block",
    fill = "Block",
    alpha = "Block"
  )

ggsave(
  "acc_dif.png",
  path = ("C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/plots"),
  width = 8,
  height = 8,
)


# make qqs ----------------------------------------------------------------

#qq no log
long_rt_acc |>
  mutate(
    block = factor(block, c("st", "mt"), c("Singletasking", "Multitasking"))
  ) |>
  ggplot(aes(sample = rt_or_acc, colour = dv)) +
  geom_qq() +
  geom_qq_line() +
  theme_classic() +
  plot_style() +
  facet_grid(switch ~ block) +
  scale_color_paletteer_d("fishualize::Oncorhynchus_mykiss") +
  scale_fill_paletteer_d("fishualize::Oncorhynchus_mykiss") +
  theme(
    strip.background = element_rect(fill = "white", color = "white", linewidth = 0.5)
  )

ggsave(
  "qq_rt_acc.png",
  path = ("C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/plots"),
  width = 8,
  height = 8,
)

# run RT analysis with and without log

#qq with log
long_rt_acc |>
  mutate(
    block = factor(block, c("st", "mt"), c("Singletasking", "Multitasking"))
  ) |>
  ggplot(aes(sample = log(rt_or_acc), colour = dv)) +
  geom_qq() +
  geom_qq_line() +
  theme_classic() +
  plot_style() +
  facet_grid(switch ~ block) +
  scale_color_paletteer_d("fishualize::Oncorhynchus_mykiss") +
  scale_fill_paletteer_d("fishualize::Oncorhynchus_mykiss") +
  theme(
    strip.background = element_rect(fill = "white", color = "white", linewidth = 0.5)
  ) +
  labs(
    title = "log adjusted",
    colour = "Dependent Variable",
  )

ggsave(
  "qq_rt_acc_log.png",
  path = ("C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/plots"),
  width = 8,
  height = 8,
)
