#####################              COHS ANALYSIS              ##################
#sadie lane 2026
#script to score COHS and output a csv file which
#may be easily joined to existing averages dataset

rm(list=ls())
library(tidyverse)
library(psychTools)
library(psych)

#set your workingdrive
setwd("C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/res")

#read in data
averages <- read_csv(
  "routine_vs_habit_avg.csv",
  na = c("", "NA")
)

#obtained from score_cohs.R
COHS <- read_csv(
  "demo_cohs.csv",
  na = c("", "NA")
)

#make sub a factor (forlater joining)
averages$sub <- as.factor(averages$sub)



# Scoring COHS ------------------------------------------------------------

#Now it would be good to score the scale
#using psych package, organise according to automaticity and routine
#Following guide by Revelle (2025)

#routine = 1, 4, 6, 7, 10, 12, 13, 14, 15, 17, 18, 20, 22, 24, 27

#automaticity = 2, 3, 5, 8, 9, 11, 16, 19, 21, 23, 25, 26

key <- list(
  rout = c("cohs_1", "cohs_4", "cohs_6", "cohs_7", "cohs_10", "cohs_12",
           "cohs_13", "cohs_14", "cohs_15", "cohs_17", "cohs_18",
           "cohs_20", "cohs_22", "cohs_24", "cohs_27"),
  auto = c("cohs_2", "cohs_3", "cohs_5", "cohs_8", "cohs_9", "cohs_11",
           "cohs_16", "cohs_19", "cohs_21", "cohs_23", "cohs_25",
           "cohs_26")
)

#create scale
scale <- scoreItems(key, COHS)

#short output:
print(scale)
#long output:
print(scale, short = FALSE)

#create raw scores, then view head and tail (rounded to 2 d.p.)
raw_scores <- scale$scores
headTail (round(raw_scores, 2))

#describe
describe(raw_scores)

#graph - commented out is a line to save the graph :)
pairs.panels(raw_scores, pch = '.')
ggsave(
  "cohs_data_distrib.png",
  path = ("C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/plots"),
  width = 14,
  height = 8
)


# make df with sub number mapped onto raw scores --------------------------

raw_df <- as.data.frame(raw_scores)
raw_df$sub <- 1:nrow(raw_df)

raw_df <- raw_df |>
  relocate(sub)

raw_df$sub <- as.factor(raw_df$sub)

#histo built into psych package
histogram_cohs(raw_df, sub, auto)


# join df with averages onto averages --------------------------------------------

joint_averages_cohs <- full_join(averages, raw_df, by = 'sub')

write_csv(joint_averages_cohs, "C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/res/joint_averages_cohs.csv")

