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

#first lets tidy our data in prep of a qq plot (norm distrib)

reclicks_te <- averages |>
  select(sub:switch, reclicks_mean, TE) |>
  filter(ses == 4) |>
  pivot_wider(
    names_from = switch,
    values_from = c("reclicks_mean", "TE")
  ) |>
  mutate(
    reclicks_mean = reclicks_mean_1,
    TE = TE_0
  ) |>
  select(sub, block, reclicks_mean, TE) |>
  pivot_longer(
    cols = c("reclicks_mean", "TE"),
    names_to = "dv",
    values_to = "reclicks_or_TE"
  )

reclicks_te |>
  ggplot(aes(sample = reclicks_or_TE, colour = dv)) +
  geom_qq() +
  geom_qq_line() +
  theme_classic() +
  plot_style() +
  facet_wrap(. ~ block) +
  scale_color_paletteer_d("wesanderson::Darjeeling2") +
  labs(
    subtitle = "participant 30 is the outlier"
  )

ggsave(
  "qq_facet_block.png",
  path = ("C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/plots"),
  width = 14,
  height = 8
)

# with outlier p30 gone

reclicks_te |>
  filter(sub != 30) |>
  ggplot(aes(sample = reclicks_or_TE, colour = dv)) +
  geom_qq() +
  geom_qq_line() +
  theme_classic() +
  plot_style() +
  facet_wrap(. ~ block) +
  scale_color_paletteer_d("wesanderson::Darjeeling2") +
  labs(
    subtitle = "participant 30 is the outlier"
  )

ggsave(
  "qq_facet_block_no30.png",
  path = ("C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/plots"),
  width = 14,
  height = 8
)
