################################################################################
#####      Partial regression plots, TE, reclicks and all_errors_stay     ######
#####                           Sadie Lane 2026                           ######
################################################################################

rm(list=ls())
library(tidyverse)
library(paletteer)
library(ggtext)

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

reclicks_x_errors <-read_csv(
  "trsf_reclicks_x_errors.csv",
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
  geom_point(shape = 21, size = 3, fill = "#899DA4FF", colour = "black") +
  geom_smooth(method = 'lm', formula = 'y ~ x', se = F, colour = "#C93312FF") +
  plot_style() +
  theme(plot.caption = element_markdown()) +
  theme_classic() +
  labs(
    title = "Transition Entropy predicts Reclicks, holding All Errors constant",
    subtitle = bquote('p < 0.001,' ~ R^2 ~ '= 0.203'),
    x = "Transition Entropy | All Errors",
    y = "Reclicks | All Errors"
  )

ggsave(
  "trsf_partial.png",
  path = ("C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/plots/thesis"),
  height = 6,
  width = 6
)

# reclicks x errors -------------------------------------------------------


reclicks_x_errors |>
  ggplot(aes(x = log_errors_stay, y = trsf_rReclicks)) +
  geom_point(shape = 21, size = 3, fill = "#899DA4FF", colour = "black") +
  geom_smooth(method = 'lm', formula = 'y ~ x', se = F, colour = "#C93312FF") +
  plot_style() +
  theme_classic() +
  labs(
    title = "All Errors do not predict Reclicks",
    subtitle = "p > 0.05",
    y = "Reclicks | All Errors",
    x = "All Errors Stay Trials (log transformed)"
  )

ggsave(
  "reclicks_x_errors_partial.png",
  path = ("C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/plots/thesis"),
  height = 6,
  width = 6
)
