################################################################################
################        Sadie Lane TE v Reclicks        ########################
################################################################################

library(tidyverse)
library(paletteer)
setwd("C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/src")
source("function_dv_histo.R")
source("function_safe_se.R")
source("plot_style.R")
source("function_safe_se.R")

#setwd("C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/res")
#read in data
averages <- read_csv(
  "routine_vs_habit_avg.csv",
  na = c("", "NA")
)

trials <- read_csv(
  "routine_vs_habit_trl.csv",
  na = c("", "NA")
)

#now plot

averages |>
  mutate(
    block = factor(
      block,
      levels = c("st", "mt"),
      labels = c("Singletask", "Multitask")
    )
  ) |>
  filter(ses == 4, switch == 1) |>
  ggplot(aes(x = reclicks_mean, y = TE)) +
  geom_jitter() +
  geom_smooth(method = lm, formula = "y ~ x", linewidth = 1.25, se = F) +
  facet_wrap( ~ block ) +
  theme_classic() +
  plot_style() +
  labs(
    x = "Mean Reclicks",
    y = "Transition Entropy"
  )
ggsave(
  "reclicks_te.png",
  path = ("C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/plots"),
  width = 14,
  height = 8,
)
