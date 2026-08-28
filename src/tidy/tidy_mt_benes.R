################################################################################
############          SL + KGG  tidy benes of reclicks, TE        ##############
################################################################################
#script to output a df for statistical analysis of costs
#of mt v st

rm(list=ls())
library(tidyverse)

#change to whatever wd is
setwd("C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/res")

#read in data
averages <- read_csv(
  "routine_vs_habit_avg.csv",
  na = c("", "NA")
)

split_by_block <- read_csv(
  "split_by_block.csv",
  na = c("","NA")
)


#create ind_predictors using the split_by_block df
ind_predictors <- split_by_block |>
  filter(block == "st") |>
  select(sub:general_errors_mean)


#create costs df (difference scores in rt and acc per sub)
costs <- averages |>
  filter(ses == 4 & switch == 0) |>
  group_by(sub) |>
  summarise(
    RT_cost = rt_mean[block == "mt"] - rt_mean[block == "st"],
    acc_cost = accuracy_mean[block == "mt"] - accuracy_mean[block == "st"]
    )

perform_dat <- inner_join(ind_predictors, costs, by = "sub")

write_csv(perform_dat, "C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/res/perform_dat.csv")


# and impact compared to cohs ---------------------------------------------
cohs_preds <- split_by_block |>
  filter(block == "st") |>
  select(sub, auto, rout)

perform_cohs <- inner_join(cohs_preds, costs, by = "sub")

write_csv(perform_cohs, "C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/res/perform_cohs.csv")



# with ge -----------------------------------------------------------------


