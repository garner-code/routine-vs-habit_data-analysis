################################################################################
############          Sadie Lane 2026 visualise n-back data       ##############
################################################################################

rm(list=ls())
library(tidyverse)
library(paletteer)
setwd("C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/src")
source("plot_style.R")

#change to your wd
setwd("C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/res")

#read in data
n_back <- read_csv(
  "routine_vs_habit_n_back_averages.csv",
  na = c("", "NA")
)

# sens and spec relfirst ------------------------------------------------------------

#make data longer and only stay trials
sens_spec <- n_back |>
  select(sub, block, sens, spec) |>
  pivot_longer(
    cols = sens:spec,
    names_to = "DV",
    values_to = "sens_or_spec",
  )

#relation of sensitivity and spec
#even tho this
sens_spec |>
  mutate(
    Block = factor(block, c("mt", "st"), c("Multitasking", "Singletasking"))
  ) |>
  ggplot(aes(x = DV, y = sens_or_spec, colour = DV, fill = DV)) +
  geom_violin(alpha = 0.3) +
  geom_point() +
  geom_line(aes(group = sub), alpha = 0.5, colour = "grey") +
  stat_summary(fun = "mean", geom = "point", color = "black", size = 2) +
  scale_color_paletteer_d("wesanderson::Darjeeling1") +
  scale_fill_paletteer_d("wesanderson::Darjeeling1") +
  theme_classic() +
  plot_style() +
  theme(
    strip.background = element_rect(fill = "white", color = "white", linewidth = 0.5),
    axis.text.x = element_blank()
  ) +
  ylim(c(0.1, 1)) +
  labs(
    title = "relation of sensitivity and spec",
  )


# all dvs v stay trials ---------------------------------------------

sens_spec_stay <- n_back |>
  select(sub, block, acc_mean_stay, rt_mean_stay, sens, spec) |>
  pivot_longer(
    cols = acc_mean_stay:spec,
    names_to = "dv_name",
    values_to = "dv_score",
  ) |>
  mutate(
    dv_name = fct_relevel(dv_name, "sens", "spec", "acc_mean_stay", "rt_mean_stay")
  )

sens_spec_stay |>
  ggplot(aes(x = dv_name, y = dv_score, colour = dv_name, fill = dv_name)) +
  geom_violin(alpha = 0.3) +
  geom_point() +
  geom_line(aes(group = sub), alpha = 0.5, colour = "grey") +
  stat_summary(fun = "mean", geom = "point", color = "black", size = 2) +
  scale_color_paletteer_d("wesanderson::Darjeeling1") +
  scale_fill_paletteer_d("wesanderson::Darjeeling1") +
  theme_classic() +
  plot_style() +
  theme(
    strip.background = element_rect(fill = "white", color = "white", linewidth = 0.5),
    axis.text.x = element_blank()
  ) +
  ylim(c(0.1, 1)) +
  labs(
    title = "all dvs - stay trials"
  )


# all dvs switch trials ---------------------------------------------------

sens_spec_switch <- n_back |>
  select(sub, block, acc_mean_switch, rt_mean_switch, sens, spec) |>
  pivot_longer(
    cols = acc_mean_switch:spec,
    names_to = "dv_name",
    values_to = "dv_score",
  ) |>
  mutate(
    dv_name = fct_relevel(dv_name, "sens", "spec", "acc_mean_switch", "rt_mean_switch")
  )

sens_spec_switch |>
  ggplot(aes(x = dv_name, y = dv_score, colour = dv_name, fill = dv_name)) +
  geom_violin(alpha = 0.3) +
  geom_point() +
  geom_line(aes(group = sub), alpha = 0.5, colour = "grey") +
  stat_summary(fun = "mean", geom = "point", color = "black", size = 2) +
  scale_color_paletteer_d("wesanderson::Darjeeling1") +
  scale_fill_paletteer_d("wesanderson::Darjeeling1") +
  theme_classic() +
  plot_style() +
  theme(
    strip.background = element_rect(fill = "white", color = "white", linewidth = 0.5),
    axis.text.x = element_blank()
  ) +
  ylim(c(0.1, 1)) +
  labs(
    title = "all dvs - switch trials"
  )
