################################################################################
############    To what extent are reclicks and te the same?    ################
############                  Sadie lane, KGG 2026              ################
################################################################################

rm(list=ls())
library(tidyverse)
library(broom)
library(gtsummary)

setwd("C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/res")

#read in data
split_by_block <- read_csv(
  "split_by_block.csv",
  na = c("", "NA")
)


# tidy our data -----------------------------------------------------------

split <- split_by_block |>
  select(sub:TE) |>
  group_by(sub) |>
  mutate(
    reclicks_sd = sd(reclicks_mean),
    TE_sd = sd(TE)
  )

mt_only <- split |>
  filter(block == "mt", sub != 30)

st_only <- split |>
  filter(block == "st")

split_sqrt <- split_by_block |>
  mutate(
    reclicks_mean_sqrt = sqrt(reclicks_mean + 0.0001),
    TE_sqrt = sqrt(TE + 0.0001)
  ) |>
  select(sub, block, reclicks_mean_sqrt, TE_sqrt)

mt_sqrt <- split_sqrt |>
  filter(block == "mt", sub != 30)

st_sqrt <- split_sqrt |>
  filter(block == "st")

#note for future self - for whatever reason sd cannot
#calculate individually for an st and mt group
#but proceeding as if sub 30 should be excluded (it should)

# test outliers -----------------------------------------------------------

#reclicks

split |>
  filter(reclicks_sd > 3) |>
  select(sub, block, reclicks_mean, reclicks_sd) |>
  tbl_summary(
  )

#TE
split |>
  filter(TE_sd > 3)

# Analyse -----------------------------------------------------------------

#correlations

#no transform
pearson <- cor.test(st_only$reclicks_mean, st_only$TE, method = "pearson") |>
  tidy()

write_csv(
  pearson,
  "C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/res/analysis_pearson_reclicks_TE.csv"
  )


spearman <- cor.test(st_only$reclicks_mean, st_only$TE, method = "spearman") |>
  tidy()

write_csv(
  spearman,
  "C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/res/analysis_spearman_reclicks_TE.csv"
)

#sqrt transform (as reclicks more normally distributed when sqrted)

pearson_sqrt <- cor.test(st_sqrt$reclicks_mean_sqrt, st_sqrt$TE_sqrt, method = "pearson") |>
  tidy()

write_csv(
  pearson_sqrt,
  "C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/res/analysis_pearson_sqrt_reclicks_TE.csv"
)

spearman_sqrt <- cor.test(st_sqrt$reclicks_mean_sqrt, st_sqrt$TE_sqrt, method = "spearman") |>
  tidy()

write_csv(
  spearman_sqrt,
  "C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/res/analysis_spearman_sqrt_reclicks_TE.csv"
)
