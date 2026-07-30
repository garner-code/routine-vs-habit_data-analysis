##################              DV VIS            ##############################
#sadie lane, z5418956, 2026

library(tidyverse)
library(paletteer)


#change to whatever n size is
setwd("C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/data")

#read in data
averages <- read_csv(
  "routine_vs_habit_avg.csv",
  na = c("", "NA")
)

trials <- read_csv(
  "routine_vs_habit_trl.csv",
  na = c("", "NA")
)

#so now make histograms, where across each the subs each have a unique colour
#so we can visualise how they look per dv
#these histograms are going to be AT TEST ONLY


# messy code to make histograms -------------------------------------------

#tidy trial data ahead of plotting
trials <- trials |>
  filter(ses == 4) |>


  select(sub, ses, t, block, switch, accuracy, reclicks, rt, general_errors, TE) |>
  #look at above line

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
  plot_style

ggsave(
  "",
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
  plot_style

ggsave(
  "",
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
  histogram(sub, mean_rt, ymin, ymax) |>
  plot_style

ggsave(
  "",
  path = ("C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/plots"),
  width = 14,
  height = 8
)

#TE

#tbd


#general_errors
trials |>
  summarise(
    mean_general_errors = mean(general_errors),
    se = safe_se(general_errors),
    ymin =  mean_general_errors - se,
    ymax = mean_general_errors + se,
  ) |>
  histogram(sub, mean_general_errors, ymin, ymax) +
  plot_style

ggsave(
  "",
  path = ("C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/plots"),
  width = 14,
  height = 8
)
