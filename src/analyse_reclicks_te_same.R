################################################################################
############    To what extent are reclicks and te the same?    ################
############                  Sadie lane, KGG 2026              ################
################################################################################

rm(list=ls())
library(tidyverse)

setwd("C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/res")

#read in data
split_by_block <- read_csv(
  "split_by_block.csv",
  na = c("", "NA")
)


# tidy our data -----------------------------------------------------------

split_by_block <- split_by_block |>
  select(sub:TE)

# test outliers -----------------------------------------------------------

averages_reclicks <- averages |>
  filter(ses == 4, switch == 1) |>
  group_by(sub) |>
  mutate(
    reclicks_sd = sd(reclicks_mean)
  )

averages_reclicks |>
  filter(reclicks_sd > 3)
#so sub 30 is outlier reclicks

averages_te <- averages |>
  filter(ses == 4, switch == 0) |>
  group_by(sub) |>
  mutate(
    TE_sd = sd(TE)
  )

averages_te |>
  filter(TE_sd > 2.5) |>
  relocate(TE_sd)
  #so no outliers for TE
  #highly highly dense data

# Analyse -----------------------------------------------------------------


mt_only <- split_by_block |>
  filter(block == "mt", sub != 30)

mt_cor <- cor(mt_only$reclicks_mean, mt_only$TE, method = "pearson")

st_only <- split_by_block |>
  filter(block == "st")

with(
  reclicks_te_point |> filter(block == "st"),
  cor.test(reclicks_mean, TE, method="spearman")
  )

st_cor <- cor(st_only$reclicks_mean, st_only$TE, method = "pearson")

