#sorting outliers for n_nc_trials (pre task jump calc)
#then task jumps themselves
#then finally n_back sens criteria
#kgg and sl 2026
rm(list=ls())
library(tidyverse)

#change to whatever wd is
setwd("C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/res")

#read in data
averages <- read_csv(
  "routine_vs_habit_avg.csv",
  na = c("", "NA")
)


# first exclusion n_nc trials ---------------------------------------------

#output csv without task jump outliers for n_nc_trials
#where outlier is mean - 2.5sds
#in this case if more than 30.72 non counted trials

#subs 8, 9, 11 are outliers on tjs
subs2excl <- c(8,9,11)

averages <- averages %>%
  filter(!sub %in% subs2excl)
write_csv(averages, "C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/res/averages_no_n_nc_outs.csv")

# exclusion 2 - nback -----------------------------------------------------
#criteria is < 0.60 across both sens and spec
#only sens has outliers

exclude_sixty <- c(13, 22, 25, 28, 51, 61, 73, 76, 85)

averages <- averages |>
  filter(!sub %in% exclude_sixty)

write_csv(averages, "C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/res/averages_no_n_nc_no_nback_outs.csv")

