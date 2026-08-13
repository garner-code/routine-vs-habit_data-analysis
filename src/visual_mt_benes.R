################################################################################
############              SL vis  benes of reclicks, TE           ##############
################################################################################

rm(list=ls())
library(tidyverse)
library(paletteer)
setwd("C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/src")
source("plot_style.R")

#change to whatever wd is
setwd("C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/res")

#read in data
averages <- read_csv(
  "routine_vs_habit_avg.csv",
  na = c("", "NA")
)

perform_dat <- read_csv(
  "perform_dat.csv",
  na = c("", "NA")
)


# visualise normalcy ------------------------------------------------------

#create costs df (difference scores in rt and acc per sub)
costs <- averages |>
  filter(ses == 4 & switch == 0) |>
  group_by(sub) |>
  summarise(
    RT_cost = rt_mean[block == "mt"] - rt_mean[block == "st"],
    acc_cost = accuracy_mean[block == "mt"] - accuracy_mean[block == "st"]
  )

#check normalcy of costs - box and qq

costs |>
  pivot_longer(
    cols = c("RT_cost", "acc_cost"),
    names_to = "names",
    values_to = "values"
  ) |>
  ggplot(aes(x = names, y = values, fill = names)) +
  geom_boxplot() +
  plot_style() +
  theme_classic() +
  paletteer::scale_fill_paletteer_d("wesanderson::Moonrise3")

#no transform qq
costs |>
  pivot_longer(
    cols = c("RT_cost", "acc_cost"),
    names_to = "names",
    values_to = "values"
  ) |>
  ggplot(aes(sample = values, colour = names)) +
  geom_qq() +
  geom_qq_line() +
  theme_classic() +
  plot_style() +
  paletteer::scale_colour_paletteer_d("wesanderson::Moonrise3")

ggsave(
  "qq_costs.png",
  path = ("C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/plots"),
  width = 8,
  height = 8
)


#sqrt transform qq - future me: some values get lost in this transformation (= 0)
costs |>
  pivot_longer(
    cols = c("RT_cost", "acc_cost"),
    names_to = "names",
    values_to = "values"
  ) |>
  filter(names == "RT_cost") |>
  ggplot(aes(sample = sqrt(values + 0.001))) +
  geom_qq(colour = "#F4B5BDFF") +
  geom_qq_line(colour = "#F4B5BDFF") +
  theme_classic() +
  plot_style()

ggsave(
  "qq_costs_sqrt.png",
  path = ("C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/plots"),
  width = 8,
  height = 8
)


# vis costs with geom_line connecting -------------------------------------

costs |>
  pivot_longer(
    cols = c("RT_cost", "acc_cost"),
    names_to = "names",
    values_to = "values"
  ) |>
  ggplot(aes(x = names, y = values, colour = names, fill = names)) +
  geom_violin(alpha = 0.3) +
  geom_point() +
  geom_line(aes(group = sub), alpha = 0.5, colour = "grey") +
  stat_summary(fun = "mean", geom = "point", color = "black", size = 2) +
  scale_color_paletteer_d("wesanderson::Moonrise3") +
  scale_fill_paletteer_d("wesanderson::Moonrise3") +
  theme_classic() +
  plot_style() +
  theme(
    strip.background = element_rect(fill = "white", color = "white", linewidth = 0.5),
    axis.text.x = element_blank()
  ) +
  labs(
    y = "costs (mt - st)"
  )


# now visualise linear models ---------------------------------------------

#reclicks

#rt
perform_dat |>
  ggplot(aes(x = reclicks_mean, y = RT_cost)) +
  geom_point(shape = 21, size = 3, fill = "#ECCBAEFF", colour = "black") +
  geom_smooth(method = lm, formula = 'y ~ x', se = F, colour = "black") +
  theme_classic() +
  plot_style() +
  labs(
    title = "Reclicks does not predict rt cost"
  )

ggsave(
  "cost_re_rt_ns.png",
  path = ("C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/plots"),
  width = 8,
  height = 8
)

#acc
perform_dat |>
  ggplot(aes(x = reclicks_mean, y = acc_cost)) +
  geom_point(shape = 21, size = 3, fill = "#ECCBAEFF", colour = "black") +
  geom_smooth(method = lm, formula = 'y ~ x', se = F, colour = "black") +
  theme_classic() +
  plot_style() +
  labs(
    title = "Reclicks does not predict acc cost"
  )

ggsave(
  "cost_re_acc_ns.png",
  path = ("C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/plots"),
  width = 8,
  height = 8
)

#te

#rt
perform_dat |>
  ggplot(aes(x = TE, y = RT_cost)) +
  geom_point(shape = 21, size = 3, fill = "#046C9AFF", colour = "black") +
  geom_smooth(method = lm, formula = 'y ~ x', se = F, colour = "black") +
  theme_classic() +
  plot_style() +
  labs(
    title = "TE does not predict RT cost"
  )

ggsave(
  "cost_TE_rt_ns.png",
  path = ("C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/plots"),
  width = 8,
  height = 8
)

#acc
perform_dat |>
  ggplot(aes(x = TE, y = acc_cost)) +
  geom_point(shape = 21, size = 3, fill = "#046C9AFF", colour = "black") +
  geom_smooth(method = lm, formula = 'y ~ x', se = F, colour = "black") +
  theme_classic() +
  plot_style() +
  labs(
    title = "TE predicts acc cost",
    subtitle = "p < 0.05 (p = 0.003)"
  )

ggsave(
  "cost_TE_acc_sig.png",
  path = ("C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/plots"),
  width = 8,
  height = 8
)

#reclicks AND te
perform_long_z <- perform_dat |>
  mutate(
    reclicks_mean = (reclicks_mean - mean(reclicks_mean)) / sd(reclicks_mean),
    TE = (TE - mean(TE)) / sd(TE)
  ) |>
  pivot_longer(
    cols = c("reclicks_mean", "TE"),
    names_to = "variables",
    values_to = "values"
  )


#rt
perform_long_z |>
  ggplot(aes(x = values, y = RT_cost, fill = variables, colour = variables)) +
  geom_point(shape = 21, size = 3, colour = "black") +
  geom_smooth(method = lm, formula = 'y ~ x', se = F) +
  theme_classic() +
  plot_style() +
  scale_fill_paletteer_d("wesanderson::Darjeeling2") +
  scale_colour_paletteer_d("wesanderson::Darjeeling2") +
  labs(
    title = "reclicks and TE together don't predict rt cost",
    x = "z score"
  )

ggsave(
  "cost_reTE_rt_ns.png",
  path = ("C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/plots"),
  width = 8,
  height = 8
)

#acc
perform_long_z |>
  ggplot(aes(x = values, y = acc_cost, fill = variables, colour = variables)) +
  geom_point(shape = 21, size = 3, colour = "black") +
  geom_smooth(method = lm, formula = 'y ~ x', se = F) +
  theme_classic() +
  plot_style() +
  scale_fill_paletteer_d("wesanderson::Darjeeling2") +
  scale_colour_paletteer_d("wesanderson::Darjeeling2") +
  labs(
    title = "reclicks and TE together predict accuracy cost",
    subtitle = "p < 0.05 (p = 0.007)",
    x = "z score"
  )

ggsave(
  "cost_reTE_acc_sig.png",
  path = ("C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/plots"),
  width = 8,
  height = 8
)
