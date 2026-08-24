## why are the bad rters bad?

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

costs <- read_csv(
  "costs.csv",
  na = c("", "NA")
)


nonsense <- c(
  3, 5, 11, 12, 16, 18, 20, 24, 35, 45, 46, 56, 58, 60, 62,
  68, 70, 81, 82
)

average_costs <- inner_join(averages, costs, by = "sub")

average_costs <- average_costs |>
  filter(ses == 4, switch == 0) |>
  mutate(
    rt_group = ifelse(RT_cost < 0, "weird", "normal")
    ) |>
  relocate(sub, ses, block, switch, RT_cost, rt_group)

average_costs |>
  ggplot(aes(x = block, y = TE, colour = rt_group, fill = rt_group, shape = rt_group)) +
  geom_violin(alpha = 0.3) +
  geom_line(aes(group = sub), alpha = 0.5, colour = "grey") +
  geom_point(size = 2,) +
  plot_style() +
  theme_classic() +
  scale_color_paletteer_d("wesanderson::IsleofDogs1") +
  scale_fill_paletteer_d("wesanderson::IsleofDogs1")
#not te

average_costs |>
  ggplot(aes(x = block, y = task_jumps_mean, colour = rt_group, fill = rt_group, shape = rt_group)) +
  geom_violin(alpha = 0.3) +
  geom_line(aes(group = sub), alpha = 0.5, colour = "grey") +
  geom_point(size = 2,) +
  plot_style() +
  theme_classic() +
  scale_color_paletteer_d("wesanderson::IsleofDogs1") +
  scale_fill_paletteer_d("wesanderson::IsleofDogs1")
#not tjs

average_costs |>
  ggplot(aes(x = block, y = setting_errors_mean, colour = rt_group, fill = rt_group, shape = rt_group)) +
  geom_violin(alpha = 0.3) +
  geom_line(aes(group = sub), alpha = 0.5, colour = "grey") +
  geom_point(size = 2,) +
  plot_style() +
  theme_classic() +
  scale_color_paletteer_d("wesanderson::IsleofDogs1") +
  scale_fill_paletteer_d("wesanderson::IsleofDogs1")
#not setting errors

average_costs |>
  ggplot(aes(x = block, y = general_errors_mean, colour = rt_group, fill = rt_group, shape = rt_group)) +
  geom_violin(alpha = 0.3) +
  geom_line(aes(group = sub), alpha = 0.5, colour = "grey") +
  geom_point(size = 2,) +
  plot_style() +
  theme_classic() +
  scale_color_paletteer_d("wesanderson::IsleofDogs1") +
  scale_fill_paletteer_d("wesanderson::IsleofDogs1")
#not ges

average_costs |>
  ggplot(aes(x = block, y = all_errors_mean, colour = rt_group, fill = rt_group, shape = rt_group)) +
  geom_violin(alpha = 0.3) +
  geom_line(aes(group = sub), alpha = 0.5, colour = "grey") +
  geom_point(size = 2,) +
  plot_style() +
  theme_classic() +
  scale_color_paletteer_d("wesanderson::IsleofDogs1") +
  scale_fill_paletteer_d("wesanderson::IsleofDogs1")
#not all errors

average_costs |>
  ggplot(aes(x = block, y = dur_mean, colour = rt_group, fill = rt_group, shape = rt_group)) +
  geom_violin(alpha = 0.3) +
  geom_line(aes(group = sub), alpha = 0.5, colour = "grey") +
  geom_point(size = 2,) +
  plot_style() +
  theme_classic() +
  scale_color_paletteer_d("wesanderson::IsleofDogs1") +
  scale_fill_paletteer_d("wesanderson::IsleofDogs1")
#not dur

average_costs |>
  ggplot(aes(x = block, y = rt_mean, colour = rt_group, fill = rt_group, shape = rt_group)) +
  geom_violin(alpha = 0.3) +
  geom_line(aes(group = sub), alpha = 0.5, colour = "grey") +
  geom_point(size = 2,) +
  plot_style() +
  theme_classic() +
  scale_color_paletteer_d("wesanderson::IsleofDogs1") +
  scale_fill_paletteer_d("wesanderson::IsleofDogs1")

