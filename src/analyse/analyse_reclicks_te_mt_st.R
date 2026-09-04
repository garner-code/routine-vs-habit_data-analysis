################################################################################
#####            script to analyse reclicks and te at st v mt           ########
#####                      Sadie Lane, 2026                             ########
################################################################################
rm(list=ls())
library(tidyverse)
library(broom)

setwd("C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/res")
#read in data

split_by_block <- read_csv(
  "split_by_block.csv",
  na = c("", "NA")
)


# tidy --------------------------------------------------------------------
#make wide + remove our 9 outliers (who were)
remove <- c(8, 9, 11, 13, 22, 25, 28, 51, 61, 73, 76, 85)

df_test <- split_by_block |>
  select(sub, block, reclicks_mean, TE) |>
  pivot_wider(
    names_from = "block",
    values_from = c("reclicks_mean", "TE")
  ) |>
  filter(!sub %in% remove)


# check norms -------------------------------------------------------------
#in sum:
  #reclicks_mean_mt -> no transform
  #reclicks_mean_st -> sqrt
  #TE_mt -> non normal (wil inappropriate due to 0s)
  #TE_st -> non normal ("")

# analyse -----------------------------------------------------------------

#transforms first

trsf_rclck_mt_st <- with(df_test, t.test(sqrt(reclicks_mean_st), reclicks_mean_mt, paired = T))
trsf_rclck_mt_st <- tidy(trsf_rclck_mt_st)
#reclicks scores are sig higher on mt than on st

TE_mt_st <- with(df_test, t.test(TE_st, TE_mt, paired = T))
TE_mt_st <- tidy(TE_mt_st)
#TE scores are sig higher on st than on mt

