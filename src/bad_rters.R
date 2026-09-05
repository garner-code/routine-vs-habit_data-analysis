## who are the bad rters, and why???

rm(list=ls())
library(tidyverse)
library(paletteer)

setwd("C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/src")
source("plot_style.R")

#change to your wd
setwd("C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/res")

averages <- read_csv(
  "routine_vs_habit_avg.csv",
  na = c("", "NA")
)

n_back_tjs <- read_csv(
  "routine_vs_habit_n_back_task_jumps.csv",
  na = c("", "NA")
)

n_back_tjs |>
  filter(nc_mt1 < 10, nc_mt2 < 10) |>
  filter(sens > 0.74, spec > 0.80)

exclude_tj <- c(
  2, 8, 9, 10, 11, 13, 22, 25, 28, 29, 35, 42, 43, 45, 46,
  51, 52, 54, 55, 61, 63, 65, 66, 73, 76, 77, 78, 84, 85
  )
exclude_tj <- data.frame(sub = exclude_tj) |>
  rename(sub = exclude_tj)

costs |>
  filter(RT_cost < 0)

nonsense <- c(
  3, 5, 11, 12, 16, 18, 20, 24, 35, 45, 46, 56, 58, 60, 62,
  68, 70, 81, 82
  )
nonsense <- data.frame(sub = nonsense)

inner_join(exclude_tj, costs, by = "sub")


nonsense_joined <- inner_join(nonsense, averages, by = "sub")

#graph to vis the faster on mt people
nonsense_joined |>
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
  )

nonsense_joined |>
  filter(ses == 4) |>
  group_by(sub, block, switch) |>
  mutate(
    switch = factor(switch, c(0, 1), c("Stay", "Switch")),
    block = factor(block, c("st", "mt"), c("Singletasking", "Multitasking"))
  ) |>
  summarise(
    tj_mean = mean(task_jumps_mean)
  ) |>
  ggplot(aes(x = block, y = tj_mean, colour = block, fill = block)) +
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
  )

