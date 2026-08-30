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

averages_democohs <- read_csv(
  "averages_democohs.csv",
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

averages_democohs <- averages_democohs |>
  filter(!sub %in% subs2excl)

write_csv(averages_democohs, "C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/res/averages_democohs_no_n_nc_outs.csv")

#averages[43:48, "task_jumps_mean"] <- NA
#9
#averages[49:54, "task_jumps_mean"] <- NA
#11
#averages[61:66, "task_jumps_mean"] <- NA

#8
#averages_democohs[43:48, "task_jumps_mean"] <- NA
#9
#averages_democohs[49:54, "task_jumps_mean"] <- NA
#11
#averages_democohs[61:66, "task_jumps_mean"] <- NA



# exclusion 2 - tjs ------------------------------------------------------------

#now get rid of the people who had enough trials to calc a tj score,
#but whose tj score itself was an outlier.
#anyone who has even one outlier, across all four permutations
#i.e across mt or st, stay or switch
tj_exclude <- c(2, 13, 73, 79, 81, 84)


averages <- averages |>
  filter(!sub %in% tj_exclude)

write_csv(averages, "C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/res/averages_no_tj_outs.csv")

averages_democohs <- averages_democohs |>
  filter(!sub %in% tj_exclude)

write_csv(averages_democohs, "C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/res/averages_democohs_no_tj_outs.csv")

# exclusion 3 - nback -----------------------------------------------------
#criteria is < 0.6 across both sens and spec
#only sens has outliers

sens_exclude <- c(13, 23, 25, 28, 51, 61, 73, 76, 85)

averages <- averages |>
  filter(!sub %in% sens_exclude)

write_csv(averages, "C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/res/averages_no_tj_no_nback_outs.csv")

averages_democohs <- averages_democohs |>
  filter(!sub %in% sens_exclude)

write_csv(averages, "C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/res/averages_democohs_no_tj_no_nback_outs.csv")

