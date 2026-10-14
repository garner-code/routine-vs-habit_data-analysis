#quickly checking whether habits actually occured, but they took long enough to
#inhibit them

rm(list=ls())
library(tidyverse)
library(paletteer)
library(broom)
setwd("C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/src")
source("plot_style.R")

#change to whatever wd is
setwd("C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/res")

#read in data
averages <- read_csv(
  "routine_vs_habit_avg.csv",
  na = c("", "NA")
)

resids <- read_csv(
  "errors_partialled_reclicks_TE.csv",
  na = c("", "NA")
)

#tidy

df <- averages |>
  filter(ses == 4, block == "st", switch == 1) |>
  select(sub, reclicks_mean, rt_mean)

for_test <- cross_join(df, resids)

df |>
  ggplot(aes(y = reclicks_mean, x = rt_mean)) +
  geom_point()

with(df, cor.test(reclicks_mean, rt_mean))

cor <- with(for_test, cor.test(rReclicks, rt_mean))
tidy(cor)

mod <- lm(rReclicks ~ rt_mean, data = for_test)
summary(mod)

