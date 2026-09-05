################################################################################
##########                      Sadie Lane 2026                  ###############
##########              checking normalcy of COHS scores         ###############
################################################################################

rm(list=ls())
library(tidyverse)
library(paletteer)
setwd("C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/src")
source("plot_style.R")

setwd("C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/res")

#read in data
avgs_cohs <- read_csv(
  "averages_democohs.csv",
  na = c("", "NA")
)

# tidy --------------------------------------------------------------------

cohs_tidy <- avgs_cohs |>
  filter(ses == 4, block == "st", switch == 0) |> #just to get one per person
  select(sub:switch, reclicks_mean, task_jumps_mean, rout:sub_lang) |>
  mutate(
    sub_eng = str_detect(sub_lang, "nglish")
  ) |>
  relocate(sub, rout:sub_lang, sub_eng)


# vis ---------------------------------------------------------------------

#first want to look at duration scores
#bc if we see really non-normative data and non-normative duration I think it
#will be fair to conclude they were just kinda checked out
#or answering yes to everything

cohs_tidy |>
  filter(dur < 10000) |>
  ggplot(aes(x = dur)) +
  geom_histogram(binwidth = 30) +
  plot_style()

cohs_tidy |>
  filter(dur > 10000)

#edit: dur is inappropriate bc it tracks TOTAL TIME OF PAGE OPEN
#so when I was having the form open before participants arrived, it tracked that
#hence why up until ~ sub 25-30  (when I was pre prepping forms) you see like 120 minutes.


#ok so next lets look at actual distribution of cohs scores


#automaticity
cohs_tidy |>
  ggplot(aes(sample = auto)) +
  geom_qq() +
  geom_qq_line() +
  plot_style() +
  labs(
    title = "cohs auto"
  )

cohs_tidy |>
  ggplot(aes(x = auto)) +
  geom_histogram(binwidth = 0.1) +
  plot_style()


#routine
cohs_tidy |>
  ggplot(aes(sample = log(rout))) +
  geom_qq() +
  geom_qq_line() +
  plot_style() +
  labs(
    title = "cohs routine"
  )

cohs_tidy |>
  ggplot(aes(x = log(rout))) +
  geom_histogram(binwidth = 0.1) +
  plot_style() +
  labs(
    title = "log routine"
  )

cohs_tidy |>
  ggplot(aes(x = rout)) +
  geom_histogram(binwidth = 0.1) +
  plot_style() +
  labs(
    title = "no trsf routine"
  )




