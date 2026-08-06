################################################################################
############    Sadie Lane vis mt v st response time, acc dif     ##############
################################################################################
rm(list=ls())
library(tidyverse)
library(paletteer)
setwd("C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/src")
source("plot_style.R")

#change to whatever n size is
setwd("C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/res")

#read in data
averages <- read_csv(
  "routine_vs_habit_avg.csv",
  na = c("", "NA")
)

averages |>
  filter(ses == 4) |>
  group_by(sub, block, switch) |>
  mutate(
    switch = factor(
      switch, c(0, 1), c("Stay", "Switch")
    ),
    Block = factor(
      block, c("mt", "st"), c("Multitasking", "Singletasking")
    )
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
    fill = "Block",
    alpha = "Block"
  )

ggsave(
  "rt_dif.png",
  path = ("C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/plots"),
  width = 14,
  height = 8,
)

#now accuracy

averages |>
  filter(ses == 4) |>
  group_by(sub, block, switch) |>
  mutate(
    switch = factor(
      switch, c(0, 1), c("Stay", "Switch")
    ),
    Block = factor(
      block, c("mt", "st"), c("Multitasking", "Singletasking")
    )
  ) |>
  summarise(
    accuracy_mean = mean(accuracy_mean)
  ) |>
  ggplot(aes(x = block, y = accuracy_mean, colour = block, fill = block)) +
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
  ylim(c(0.1, 1)) +
  labs(
    title = "RT dif is not due to accuracy",
    subtitle = "Means\nStay: mt = 0.920, st = 0.952 \nSwitch: mt = 0.323,st = 0.351",
    x = "Block",
    y = "Mean Accuracy",
    colour = "Block",
    fill = "Block",
    alpha = "Block"
  )

ggsave(
  "acc_dif.png",
  path = ("C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/plots"),
  width = 14,
  height = 8,
)


# make qqs ----------------------------------------------------------------

qq <- averages |>
  filter(ses == 4) |>
  group_by(sub, block, switch) |>
  mutate(
    switch = factor(
      switch, c(0, 1), c("Stay", "Switch")
    ),
    Block = factor(
      block, c("mt", "st"), c("Multitasking", "Singletasking")
    )
  ) |>
  select(sub:switch, accuracy_mean, rt_mean) |>
  pivot_longer(
    cols = c("accuracy_mean", "rt_mean"),
    names_to = "dv",
    values_to = "rt_or_acc"
  )


# run RT analysis with and without log
qq |>
  ggplot(aes(sample = log(rt_or_acc), colour = dv)) +
  geom_qq() +
  geom_qq_line() +
  theme_classic() +
  plot_style() +
  facet_grid(switch ~ block)

# run t-tests --------------------------------------------------------
# (and summaries)

averages |>
  filter(ses == 4 & switch == 0) |>
  group_by(block) |>
  summarise(M=mean(rt_mean),
            SD=sd(rt_mean),
            accM = mean(accuracy_mean),
            accSD = sd(accuracy_mean))


for_t_tests <- averages |>
  filter(ses == 4 & switch == 0) |>
  select(sub, block, rt_mean, accuracy_mean) |>
  pivot_wider(
   names_from = block,
   values_from = c(rt_mean, accuracy_mean)
  )

# do RT with and without log
with(for_t_tests, t.test(log(rt_mean_st), log(rt_mean_mt)))
with(for_t_tests, t.test(accuracy_mean_st, accuracy_mean_mt))
