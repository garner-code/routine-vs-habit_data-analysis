################################################################################
#####      Partial regression plots, TE, reclicks and all_errors_stay     ######
#####                           Sadie Lane 2026                           ######
################################################################################

rm(list=ls())
library(tidyverse)
library(paletteer)

setwd("C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/src")
source("plot_style.R")

setwd("C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/res")

#read in data

partial <- read_csv(
  "errors_partialled_reclicks_TE.csv",
  na = c("", "NA")
)

trsf_partial <- read_csv(
  "trsf_errors_partialled_reclicks_TE.csv",
  na = c("", "NA")
)


#relation of reclicks and te, partialling out all_errors_stay

#first with no transforms

partial |>
  ggplot(aes(x = rReclicks, y = rTE)) +
  geom_point() +
  geom_smooth(, formula = 'y ~ x', method = 'lm', se = F) +
  plot_style() +
  theme_classic() +
  labs(
    title = "errors partialled out, no tranform",
    subtitle = "stay trials only"
  )

#and second with sqrt transform for reclicks, log + 0.001 for all_errors
trsf_partial |>
  ggplot(aes(x = trsf_rTE, y = trsf_rReclicks)) +
  geom_point() +
  geom_smooth(, formula = 'y ~ x', method = 'lm', se = F) +
  plot_style() +
  theme_classic() +
  labs(
    title = "errors partialled out, sqrt(reclicks) and log(errors) tranforms",
    subtitle = "stay trials only"
  )

