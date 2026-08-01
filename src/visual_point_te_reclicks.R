################################################################################
################      Sadie Lane TE v Reclicks Scatter    ######################
################################################################################

library(tidyverse)
library(paletteer)
library(ggrepel)
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

#first lets tidy our data in prep of our scatter plot

reclicks_te_point <- averages |>
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
  select(sub, block, reclicks_mean, TE)

#now lets plot the data

#first with labels bc it will be interesting
reclicks_te_point |>
  ggplot(aes(x = reclicks_mean, y = TE, label = sub)) +
  geom_point() +
  geom_text_repel() +
  facet_wrap(. ~ block) +
  theme_classic() +
  plot_style()

ggsave(
  "te_v_reclicks_geomtext.png",
  path = ("C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/plots"),
  width = 14,
  height = 8
)

#next im going to eyeball out some outliers (sub 30)
reclicks_te_point |>
  filter(sub != 30) |>
  ggplot(aes(x = reclicks_mean, y = TE)) +
  geom_point() +
  geom_smooth(method = lm, se = F) +
  facet_wrap(. ~ block) +
  theme_classic() +
  plot_style() +
  labs(
    title = "sub 30 removed bc reclicks = 15",
    subtitle = "pearsons r without sub 30\nmt = -0.555\nst = -0.388"
  )

ggsave(
  "te_v_reclicks_without_sub30.png",
  path = ("C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/plots"),
  width = 14,
  height = 8
)

mt_only <- reclicks_te_point |>
  filter(block == "mt", sub != 30)

mt_cor <- cor(mt_only$reclicks_mean, mt_only$TE, method = "pearson")

st_only <- reclicks_te_point |>
  filter(block == "st", sub != 30)

st_cor <- cor(st_only$reclicks_mean, st_only$TE, method = "pearson")
