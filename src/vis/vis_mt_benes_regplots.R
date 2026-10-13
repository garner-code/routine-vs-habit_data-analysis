################################################################################
############              SL vis  benes of reclicks, TE           ##############
################################################################################

rm(list=ls())
library(tidyverse)
library(paletteer)
setwd("C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/src")
source("plot_style_two.R")

#change to whatever wd is
setwd("C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/res")

#read in data
rt_x_reclicks <- read_csv(
  "rt_x_reclicks.csv",
  na = c("", "NA")
)

rt_x_TE <- read_csv(
  "rt_x_TE.csv",
  na = c("", "NA")
)

rt_x_gen <- read_csv(
  "rt_x_gen.csv",
  na = c("", "NA")
)


TJ_x_reclicks <- read_csv(
  "TJ_x_reclicks.csv",
  na = c("", "NA")
)

TJ_x_TE <- read_csv(
  "TJ_x_TE.csv",
  na = c("", "NA")
)

TJ_x_gen <- read_csv(
  "TJ_x_gen.csv",
  na = c("", "NA")
)

# rt ----------------------------------------------------------------------

#reclicks
rt_x_reclicks |>
  mutate(rRT_x_reclicks = rRT_x_reclicks * 1000) |>
  ggplot(aes(x = rReclicks_x_rt, y = rRT_x_reclicks)) +
  geom_point(shape = 21, size = 3.5, stroke = 1.1, fill = "#899DA495", colour = "black") +
  geom_smooth(method = 'lm', formula = 'y ~ x', se = F, colour = "#1F78B4FF") +
  #scale_x_continuous(limits = c(0, 3)) +
  plot_style_two() +
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
    size = 4.3,
    x = 4,
    y = -160,
    fontface = "italic",
    label = "p = .201  \nr = -.154 \nβ = -0.006"
  ) +
  labs(
    y = "RT Cost | TE + General Errors",
    x = "Reclicks | TE + General Errors"
  )

ggsave(
  "RTcost_x_reclicks.png",
  path = ("C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/plots/thesis"),
  height = 4,
  width = 4
)

#TE
rt_x_TE |>
  mutate(rRT_x_TE = rRT_x_TE * 1000) |>
  ggplot(aes(x = rTE_x_rt, y = rRT_x_TE)) +
  geom_point(shape = 23, size = 3.5, stroke = 1.1, fill = "#899DA495", colour = "black") +
  geom_smooth(method = 'lm', formula = 'y ~ x', se = F, colour = "#1F78B4FF") +
  plot_style_two() +
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
    size = 4.3,
    x = -0.3,
    y = -160,
    fontface = "italic",
    label = "p = .802  \nr = .030  \nβ = -0.006"
  ) +
  labs(
    y = "RT Cost | Reclicks + General Errors",
    x = "TE | Reclicks + General Errors"
  )

ggsave(
  "RTcost_x_TE.png",
  path = ("C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/plots/thesis"),
  height = 4,
  width = 4
)

#ges
rt_x_gen |>
  mutate(rRT_x_gen = rRT_x_gen * 1000) |>
  ggplot(aes(x = rGen_x_rt, y = rRT_x_gen)) +
  geom_point(shape = 24, size = 3.5, stroke = 1.1, fill = "#899DA495", colour = "black") +
  plot_style_two() +
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
    size = 4.3,
    x = 0.075,
    y = -160,
    fontface = "italic",
    label = "p = .757  \nr = -.037  \nβ = -0.130"
  ) +
  labs(
    y = "RT Cost | Reclicks + TE",
    x = "General Errors | Reclicks + TE"
  )

ggsave(
  "RTcost_x_errors.png",
  path = ("C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/plots/thesis"),
  height = 4,
  width = 4
)

# task jumps --------------------------------------------------------------

#reclicks
TJ_x_reclicks |>
  ggplot(aes(x = rReclicks_x_TJ, y = rTJ_x_reclicks)) +
  geom_point(shape = 21, size = 3.5, stroke = 1.1, fill = "#899DA495", colour = "black") +
  geom_smooth(method = 'lm', formula = 'y ~ x', se = F, colour = "#33A02CFF") +
  plot_style_two() +
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
    size = 4.3,
    x = 4,
    y = -0.45,
    fontface = "italic",
    label = "p = .800  \nr = -.031  \nβ = -0.003"
  ) +
  labs(
    y = "TJ Cost | TE + General Errors",
    x = "Reclicks | TE + General Errors"
  )

ggsave(
  "TJcost_x_reclicks.png",
  path = ("C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/plots/thesis"),
  height = 4,
  width = 4
)

#TE
TJ_x_TE |>
  ggplot(aes(x = rTE_x_tj, y = rTJ_x_TE)) +
  geom_point(shape = 23, size = 3.5, stroke = 1.1, fill = "#899DA495", colour = "black") +
  geom_smooth(method = 'lm', formula = 'y ~ x', se = F, colour = "#33A02CFF") +
  plot_style_two() +
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
    size = 4.3,
    x = -0.3,
    y = -0.4,
    fontface = "italic",
    label = "p = .285  \nr = .129  \nβ = 0.063"
  ) +
  labs(
    y = "TJ Cost | Reclicks + General Errors",
    x = "TE | Reclicks + General Errors"
  )

ggsave(
  "TJcost_x_TE.png",
  path = ("C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/plots/thesis"),
  height = 4,
  width = 4
)

#gen errs
TJ_x_gen |>
  ggplot(aes(x = rGen_x_tj, y = rTJ_x_gen)) +
  geom_point(shape = 24, size = 3.5, stroke = 1.1, fill = "#899DA495", colour = "black") +
  plot_style_two() +
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
    size = 4.3,
    x = 0.075,
    y = -0.45,
    fontface = "italic",
    label = "p = .051  \nr = .233  \nβ = 2.008"
  ) +
  labs(
    y = "TJ Cost | Reclicks + TE",
    x = "General Errors | Reclicks + TE"
  )

ggsave(
  "TJcost_x_errors.png",
  path = ("C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/plots/thesis"),
  height = 4,
  width = 4
)

