################################################################################
#################    Sadie Lane tidy raw democohs 2026          ################
################################################################################
#take in the raw csv outputted by qualtrics and output something
#tidy that can be joined to our averages data

rm(list=ls())
library(tidyverse)

#set to your wd
setwd("C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/res")

joint_averages_cohs <- read_csv(
  "joint_averages_cohs.csv",
  na = c("", "NA")
)

averages <- read_csv(
  "routine_vs_habit_avg.csv",
  na = c("", "NA")
)
#make sub a factor (for later joining)
joint_averages_cohs$sub <- as.factor(joint_averages_cohs$sub)
averages$sub <- as.factor(averages$sub)

#now read in qualtrics csv file, relabel cols and assign datatypes
raw_democohs <- read_csv(
  "demo_cohs_raw.csv",
  na = c("", "NA"),
  skip = 3,
  col_names = c(
    "start_date", "end_date", "status", "ip", "progress", "dur", "complete",
    "recorded_date", "response_id", "last_name", "first_name", "email", "data_ref",
    "lat", "long", "channel", "user_lang", "sub", "sub_age", "sub_gender", "sub_gender_spec",
    "sub_hand", "sub_lang", "cohs_1", "cohs_2", "cohs_3", "cohs_4", "cohs_5",
    "cohs_6", "cohs_7", "cohs_8", "cohs_9", "cohs_10", "cohs_11", "cohs_12",
    "cohs_13", "cohs_14", "cohs_15", "cohs_16", "cohs_17", "cohs_18", "cohs_19",
    "cohs_20", "cohs_21", "cohs_22", "cohs_23", "cohs_24", "cohs_25", "cohs_26",
    "cohs_27", "data_pol_violate"
    ),
  col_types = c(
    "?", "?", "?", "?", "d", "d", "l", "?", "c", "c", "c", "c", "c",
    "d", "d", "c", "c", "d", "d", "d", "f", "c", "f", "c", "c", "c",
    "c", "c", "c", "c", "c", "c", "c", "c", "c", "c", "c", "c", "c",
    "c", "c", "c", "c", "c", "c", "c", "c", "c", "c", "c", "c"
  )
)

#now tidy what we just read in and transform all the cohs qs to just numbers
tidy_democohs <- raw_democohs |>
  select(dur, sub:cohs_27) |>
  relocate(sub, dur) |>
  mutate(
    across(c(cohs_1:cohs_27), parse_number)
  )

write_csv(tidy_democohs, "C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/res/demo_cohs.csv")

#we joint the scored cohs onto averages in  the score_cohs.R script
#lets also join the demographics onto the averages file

demos <- tidy_democohs |>
  select(sub:sub_lang) |>
  mutate(sub = factor(sub))

joint_averages_demos <- full_join(averages, demos, by = 'sub')

write_csv(joint_averages_demos, "C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/res/joint_averages_demos.csv")

#and finally join everything and write a csv.

averages_democohs <- full_join(joint_averages_cohs, demos, by = 'sub')

write_csv(averages_democohs, "C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/res/averages_democohs.csv")


