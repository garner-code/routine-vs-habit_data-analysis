################################################################################
#####                     Partial regression plots                        ######
#####      reclicks pred by te, holding constant perform errors and rt    ######
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
  "rt_errors_partialled.csv",
  na = c("", "NA")
)

errors <- read_csv(
  "rtpart_reclicks_x_errors.csv",
  na = c("", "NA")
)

rts <- read_csv(
  "rtpart_reclicks_x_rt.csv",
  na = c("", "NA")
)

# plot relicks pred by reclicks -------------------------------------------

partial |>
  ggplot(aes(x = rTE, y = rReclicks)) +
  geom_point(shape = 21, size = 3.5, stroke = 1.1, fill = "#899DA495", colour = "black") +
  geom_smooth(method = 'lm', formula = 'y ~ x', se = T, colour = "#C93312FF", fill = "#C93312FF", fullrange = TRUE) +
  scale_x_continuous(limits = c(-0.6, 0.6)) +
  plot_style() +
  theme(
    axis.title = element_text(face = "bold"),
    axis.title.x = element_text(margin = margin (t = 15)),
    axis.title.y = element_text(margin = margin (r = 15)),
    axis.line = element_line(colour = "grey"),
    axis.ticks = element_line(colour = "grey"),
    plot.margin = margin(t = 15, r = 15, b = 15, l = 15, unit = "pt")
  ) +
  annotate(
    geom = "text",
    size = 4.5,
    x = 0.3,
    y = 5,
    fontface = "italic",
    label = "p = .036 \nr = -.231\nβ = -1.43"
  ) +
  labs(
    x = "TE | Performance Errors + RT",
    y = "Reclicks | Performance Errors + RT"
  )

ggsave(
  "partial_with_rts.png",
  path = ("C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/plots/thesis"),
  height = 5,
  width = 5
)

#performance errors predict reclicks

errors |>
  ggplot(aes(x = rErrs_tertmean, y = rReclicks_tertmean)) +
  geom_point(shape = 21, size = 3.5, stroke = 1.1, fill = "#899DA495", colour = "black") +
  geom_smooth(method = 'lm', formula = 'y ~ x', se = F, colour = "#C93312FF") +
  plot_style() +
  theme(
    axis.title = element_text(face = "bold"),
    axis.title.x = element_text(margin = margin (t = 15)),
    axis.title.y = element_text(margin = margin (r = 15)),
    axis.line = element_line(colour = "grey"),
    axis.ticks = element_line(colour = "grey"),
    plot.margin = margin(t = 15, r = 15, b = 15, l = 15, unit = "pt")
  ) +
  annotate(
    geom = "text",
    size = 4.5,
    x = 0.25,
    y = 5,
    fontface = "italic",
    label = "p = .196\nr = -.142\n β = -2.52"
  ) +
  labs(
    y = "Reclicks | TE + RT",
    x = "Performance Errors | TE + RT"
  ) +
  coord_cartesian(clip = "off")

ggsave(
  "reclicks_x_errors_partial_rt.png",
  path = ("C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/plots/thesis"),
  height = 5,
  width = 5
)


#rt predict relicks
rts |>
  ggplot(aes(x = rRTs_teerrors, y = rReclicks_teerrors)) +
  geom_point(shape = 21, size = 3.5, stroke = 1.1, fill = "#899DA495", colour = "black") +
  geom_smooth(method = 'lm', formula = 'y ~ x', se = F, colour = "#C93312FF") +
  plot_style() +
  theme(
    axis.title = element_text(face = "bold"),
    axis.title.x = element_text(margin = margin (t = 15)),
    axis.title.y = element_text(margin = margin (r = 15)),
    axis.line = element_line(colour = "grey"),
    axis.ticks = element_line(colour = "grey"),
    plot.margin = margin(t = 15, r = 15, b = 15, l = 15, unit = "pt")
  ) +
  annotate(
    geom = "text",
    size = 4.5,
    x = 0.25,
    y = 5,
    fontface = "italic",
    label = "p = .149\nr = -.160\n β = -2.72"
  ) +
  labs(
    y = "Reclicks | TE + Performance Errors",
    x = "RT | TE + Performance Errors"
  ) +
  coord_cartesian(clip = "off")

ggsave(
  "reclicks_x_PEs_partial_rt.png",
  path = ("C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/plots/thesis"),
  height = 5,
  width = 5
)
