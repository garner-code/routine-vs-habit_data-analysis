################################################################################
############    To what extent are reclicks and te the same?    ################
############                  Sadie lane, KGG 2026              ################
################################################################################

rm(list=ls())
library(tidyverse)
library(gtsummary)

setwd("C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/res")

#read in data
split_by_block <- read_csv(
  "split_by_block.csv",
  na = c("", "NA")
)


# tidy our data -----------------------------------------------------------

split_by_block <- split_by_block |>
  select(sub:TE) |>
  group_by(sub) |>
  mutate(
    reclicks_sd = sd(reclicks_mean),
    TE_sd = sd(TE)
  )

#note for future self - for whatever reason sd cannot
#calculate individually for an st and mt group
#but proceeding as if sub 30 should be excluded (it should)

# test outliers -----------------------------------------------------------

#reclicks

split_by_block |>
  filter(reclicks_sd > 3) |>
  select(sub, block, reclicks_mean, reclicks_sd) |>
  tbl_summary(
  )

#TE
split_by_block |>
  filter(TE_sd > 3)

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

