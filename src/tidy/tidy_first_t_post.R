###############################################################################
########    Investigating impact on RT, TJ first trial post switch    ##########
########                    Sadie Lane, 2026                          ##########
################################################################################

rm(list=ls())
library(tidyverse)

setwd("C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/res")

trials <- read_csv(
  "routine_vs_habit_trl.csv",
  na = c("", "NA")
)


# initial attempt ---------------------------------------------------------

#we need to figure out how to tidy the data in such a way that only the
#first trial post a switch is included
#so lets start by trying to select that in a simple dataframe

trials_simple <- trials |>
  filter(ses == 4) |>
  select(sub:context, switch, n_rt_outliers, rt)


#create a df with just first trial post switch
#we want a function which goes through each row, when it detects that switch is
#equal to 1, it adds the following row to the dataframe.
#but we only want this to be the case within the sub, i.e. if the final trial is
#a switch trial, we want to make sure that it doesn't count the
#first trial of the next sub as being a post-switch trial

trials_simple |>
  filter(switch == 1)

post_switch {
  if(switch == 1)
}
