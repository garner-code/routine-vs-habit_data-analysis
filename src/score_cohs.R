#####################              COHS ANALYSIS              ##################
#sadie lane z5418956

library(tidyverse)
library(paletteer)
library(psychTools)
library(psych)

#setwd("C:/Users/user/OneDrive - UNSW/2026!/Honours/Data/routine_habit/res")

averages <- read_csv(
  "routine_vs_habit_avg.csv",
  na = c("", "NA")
)

COHS <- read_csv(
  "cohs_42ps.csv",
  na = c("", "NA")
)

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
#print(scale)
#long output:
#print(scale, short = FALSE)

#create raw scores, then view head and tail (rounded to 2 d.p.)
raw_scores <- scale$scores
#headTail (round(raw_scores, 2))

#describe
#describe(raw_scores)

#graph - commented out is a line to save the graph :)
#pairs.panels(raw_scores, pch = '.')
#ggsave("18_participants_default_graph.png")


# make df with sub number mapped onto raw scores --------------------------

raw_df <- as.data.frame(raw_scores)
raw_df$sub <- 1:nrow(raw_df)

raw_df <- raw_df |>
  relocate(sub)

raw_df$sub <- as.factor(raw_df$sub)



