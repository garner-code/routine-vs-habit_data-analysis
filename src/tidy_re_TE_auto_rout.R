################################################################################
##########       A script to output wide and long dfs with        ##############
##########        reclicks, TE, automaticity and routine          ##############
##########                  Sadie Lane, 2026                      ##############
################################################################################

rm(list=ls())
library(tidyverse)

#set your working drive to where your csvs are
setwd("C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/res")

#read in data
averages_democohs <- read_csv(
  "averages_democohs.csv",
  na = c("", "NA")
)


#wide

wide_re_te_auto_rout <- averages_democohs |>
  select(sub:switch, reclicks_mean, TE, auto, rout) |>
  filter(ses == 4) |>
  pivot_wider(
    names_from = switch,
    values_from = c("reclicks_mean", "TE")
  ) |>
  mutate(
    reclicks_mean = reclicks_mean_1,
    TE = TE_0
  ) |>
  select(sub, block, reclicks_mean, TE, auto, rout) |>
  pivot_wider(
    names_from = block,
    values_from = c("reclicks_mean", "TE", "auto", "rout")
  )

write_csv(wide_re_te_auto_rout, "C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/res/wide_re_te_auto_rout.csv")

#long

long_re_te_auto_rout <- averages_democohs |>
  select(sub:switch, reclicks_mean, TE, auto, rout) |>
  filter(ses == 4) |>
  pivot_wider(
    names_from = switch,
    values_from = c("reclicks_mean", "TE")
  ) |>
  mutate(
    reclicks_mean = reclicks_mean_1,
    TE = TE_0
  ) |>
  select(sub, block, reclicks_mean, TE, auto, rout) |>
  pivot_longer(
    cols = reclicks_mean:rout,
    names_to = "dv",
    values_to = "values"
  )

write_csv(long_re_te_auto_rout, "C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/res/long_re_te_auto_rout.csv")

#long except split by mt and st

split_by_block <- averages_democohs |>
    select(sub:switch, reclicks_mean, TE, auto, rout) |>
    filter(ses == 4) |>
    pivot_wider(
      names_from = switch,
      values_from = c("reclicks_mean", "TE")
    ) |>
    mutate(
      reclicks_mean = reclicks_mean_1,
      TE = TE_0
    ) |>
    select(sub, block, reclicks_mean, TE, auto, rout)

write_csv(split_by_block, "C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/res/split_by_block_ge.csv")
