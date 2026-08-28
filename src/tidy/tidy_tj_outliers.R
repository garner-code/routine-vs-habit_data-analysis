#output csv without task jump outliers
#where outlier is mean - 2.5sds
#in this case if more than 30.72 non counted trials
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

subs2excl <- averages %>%
  filter(x < 50) %>%
  unique(sub)
#subs 8, 9, 11 are outliers on tjs
subs2excl <- c(8,9,11)

averages <- averages %>%
  filter(!sub %in% subs2excl)




averages[43:48, "task_jumps_mean"] <- NA
#9
averages[49:54, "task_jumps_mean"] <- NA
#11
averages[61:66, "task_jumps_mean"] <- NA

write_csv(averages, "C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/res/averages_no_tj_outs.csv")

#8
averages_democohs[43:48, "task_jumps_mean"] <- NA
#9
averages_democohs[49:54, "task_jumps_mean"] <- NA
#11
averages_democohs[61:66, "task_jumps_mean"] <- NA

write_csv(averages_democohs, "C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/res/averages_democohs_no_tj_outs.csv")
