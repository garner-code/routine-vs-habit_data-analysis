##################              DV VIS            ##############################
#sadie lane, 2026

library(tidyverse)
library(paletteer)
setwd("C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/src")
source("function_dv_histo.R")
source("function_safe_se.R")
source("plot_style.R")
source("function_safe_se.R")

#change to whatever n size is
setwd("C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/res")

#read in data
averages <- read_csv(
  "routine_vs_habit_avg.csv",
  na = c("", "NA")
)

trials <- read_csv(
  "routine_vs_habit_trl.csv",
  na = c("", "NA")
)

rainbow <- paletteer::paletteer_c("viridis::turbo", n = 85)

#so now make histograms, where across each the subs each have a unique colour
#so we can visualise how they look per dv
#these histograms are going to be AT TEST ONLY


# messy code to make histograms -------------------------------------------

#tidy trial data ahead of plotting
trials <- trials |>
  filter(ses == 4) |>
  select(sub, ses, t, block, switch, accuracy, reclicks, rt, general_errors) |>
  group_by(sub, switch, block) |>
  mutate(
    switch = factor(switch, levels = c(0, 1), labels = c("Stay", "Switch")),
    block = factor (block, levels = c("st", "mt"), labels = c("Singletask", "Multitask"))
  )

#accuracy
trials |>
  summarise(
    accuracy_mean = mean(accuracy),
    se = safe_se(accuracy),
    ymin =  accuracy_mean - se,
    ymax = accuracy_mean + se,
  ) |>
  histogram(sub, accuracy_mean, ymin, ymax) +
  plot_style()

ggsave(
  "accuracy_histo.png",
  path = ("C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/plots"),
  width = 14,
  height = 8
)

#reclicks
trials |>
  summarise(
    mean_reclicks = mean(reclicks),
    se = safe_se(reclicks),
    ymin =  mean_reclicks - se,
    ymax = mean_reclicks + se,
  ) |>
  histogram(sub, mean_reclicks, ymin, ymax) +
  plot_style()

ggsave(
  "reclicks_histo.png",
  path = ("C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/plots"),
  width = 14,
  height = 8
)

#rt_mean
trials |>
  summarise(
    mean_rt = mean(rt, na.rm = T),
    se = safe_se(rt),
    ymin =  mean_rt - se,
    ymax = mean_rt + se,
  ) |>
  histogram(sub, mean_rt, ymin, ymax) +
  plot_style()

ggsave(
  "rt_mean_histo.png",
  path = ("C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/plots"),
  width = 14,
  height = 8
)

#TE

averages |>
  filter(ses == 4) |>
  select(sub, ses, block, switch, TE) |>
  group_by(sub, switch, block) |>
  mutate(
    se = safe_se(TE),
    ymin =  TE - se,
    ymax = TE + se,
    switch = factor(switch, levels = c(0, 1), labels = c("Stay", "Switch")),
    block = factor (block, levels = c("st", "mt"), labels = c("Singletask", "Multitask"))
  ) |>
  ggplot() +
  geom_col(
    aes(x = sub, y = TE, colour = sub, fill = sub),
    position = position_dodge(0.9)
  ) +
  geom_errorbar(
    aes(x = sub, y = TE, ymin = ymin, ymax = ymax),
    width = 0.2, position = position_dodge(0.9)
  ) +
  facet_grid(block ~ .) +
  scale_color_paletteer_c("viridis::turbo") +
  scale_fill_paletteer_c("viridis::turbo") +
  plot_style()

ggsave(
  "TE_histo.png",
  path = ("C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/plots"),
  width = 14,
  height = 8
)

#general_errors
trials |>
  summarise(
    mean_general_errors = mean(general_errors),
    se = safe_se(general_errors),
    ymin =  mean_general_errors - se,
    ymax = mean_general_errors + se,
  ) |>
  histogram(sub, mean_general_errors, ymin, ymax) +
  plot_style()

ggsave(
  "gen_errors_histo.png",
  path = ("C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/plots"),
  width = 14,
  height = 8
)
