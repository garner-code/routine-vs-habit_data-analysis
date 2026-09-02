################################################################################
#####         script for pretty plots from t tests comparing            ########
#####                 rt, task jumps and gen errors                     ########
#####                      Sadie Lane, 2026                             ########
################################################################################

rm(list=ls())
library(tidyverse)
library(paletteer)
library(ggsignif)
library(ggdist)
library(ggpp)
setwd("C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/src")
source("plot_style.R")

#change to your wd
setwd("C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/res")

#read in data
averages <- read_csv(
  "routine_vs_habit_avg.csv",
  na = c("", "NA")
)

# tidy --------------------------------------------------------------------

graph_ts <- averages |>
  filter(ses == 4, switch == 0) |>
  group_by(sub, block) |>
  mutate(
    block = factor(block, c("st", "mt"), c("Singletasking", "Multitasking"))
  ) |>
  mutate(
    rt_mean = mean(rt_mean),
    tj_mean = mean(task_jumps_mean),
    ges_mean = mean(general_errors_mean),
    x_pos = if_else(block == "Singletasking", 1, 2)
  )

# plot --------------------------------------------------------------------
pal_fill <- c("#A6CEE390", "#1F78B490", "#B2DF8A90", "#33A02C90", "#FDBF6F90", "#FF7F0090")
pal_colour <- paletteer_d("RColorBrewer::Paired")

rt_pal_fill <- pal_fill[c(1, 2)]
rt_pal_colour <- pal_colour[c(1,2)]

graph_ts |>
  ggplot(aes(x = switch, y = rt_mean)) +
  stat_slab(
    aes(colour = block, fill = block), width = 0.3,
    data = ~ filter(.x, block == "Singletasking"),
    side = "left", alpha = 0.5, position = position_nudge(x = -0.5)
    ) +
  stat_slab(
    aes(colour = block, fill = block), width = 0.3,
    data = ~ filter(.x, block == "Multitasking"),
    side = "right", alpha = 0.5, position = position_nudge(x = 0.5),
  ) +
  geom_boxplot(
    aes(colour = block, fill = block), alpha = 0.5, width = 0.05,
    data = ~ filter(.x, block == "Singletasking"),
    position = position_nudge(x = -0.45), outlier.color = NA
  ) +
  geom_boxplot(
    aes(colour = block, fill = block), alpha = 0.5, width = 0.05,
    data = ~ filter(.x, block == "Multitasking"),
    position = position_nudge(x = 0.45), outlier.color = NA
  ) +
  geom_point(
    aes(stroke = 1.1, colour = block, fill = block, group = sub),
    data = ~ filter(.x, block == "Singletasking"),
    position = position_dodgenudge(width = 0.2, x = -0.25), shape = 21, size = 3.5
  ) +
  geom_point(
    aes(stroke = 1.1, colour = block, fill = block, group = sub),
    data = ~ filter(.x, block == "Multitasking"),
    position = position_dodgenudge(width = 0.2, x = 0.25), shape = 21, size = 3.5
  ) +
  scale_fill_manual(values = rt_pal_fill) +
  scale_colour_manual(values = rt_pal_colour) +
  plot_style() +
  theme(
    axis.title = element_text(face = "bold"),
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.title.y = element_text(margin = margin (r = 15)),
    axis.line = element_line(colour = "grey"),
    axis.ticks = element_blank(),
    legend.position = "none",
    strip.background = element_rect(fill = "white", color = "white", linewidth = 0.5)
  ) +
  labs(
    y = "Mean RT (ms)"
  )

ggsave(
  "rt_dif_ttest_raincloud.png",
  path = ("C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/plots/thesis"),
  width = 4,
  height = 6,
)


#task jumps
tj_pal <- pal[c(3, 4)]

graph_ts |>
  ggplot(aes(x = block, y = tj_mean)) +
  geom_violin(aes(colour = block, fill = block), alpha = 0.3) +
  geom_point(
    aes(colour = block, fill = block, shape = block, group = sub)
  ) +
  geom_line(
    aes(group = sub),
    alpha = 0.5,
    colour = "grey"
  ) +
  stat_summary(fun = "mean", geom = "point", color = "black", size = 2) +
  scale_colour_manual(values = tj_pal) +
  scale_fill_manual(values = tj_pal) +
  theme_classic() +
  plot_style() +
  theme(
    strip.background = element_rect(fill = "white", color = "white", linewidth = 0.5),
    axis.title.x = element_blank(),
    legend.position = "none"
  ) +
  ylim(c(0, 1)) +
  labs(
    title = "Task Jumps do not significantly differ",
    y = "Mean Task Jumps",
    colour = "Block",
    fill = "Block"
  )

ggsave(
  "tj_dif_ttest.png",
  path = ("C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/plots/thesis"),
  width = 6,
  height = 6,
)

#gen errors
ge_pal <- pal[c(9, 10)]

graph_ts |>
  ggplot(aes(x = block, y = ges_mean)) +
  geom_violin(aes(colour = block, fill = block), alpha = 0.3) +
  geom_point(
    aes(colour = block, fill = block, shape = block, group = sub)
  ) +
  geom_line(
    aes(group = sub),
    alpha = 0.5,
    colour = "grey"
  ) +
  stat_summary(fun = "mean", geom = "point", color = "black", size = 2) +
  scale_colour_manual(values = ge_pal) +
  scale_fill_manual(values = ge_pal) +
  theme_classic() +
  plot_style() +
  theme(
    strip.background = element_rect(fill = "white", color = "white", linewidth = 0.5),
    axis.title.x = element_blank(),
    legend.position = "none"
  ) +
  ylim(c(0, 0.5)) +
  labs(
    title = "Task Jumps do not significantly differ",
    y = "Mean General Errors",
    colour = "Block",
    fill = "Block"
  )


stat_pointinterval(
  aes(colour = block, fill = block), alpha = 0.5, width = 0.05,
  data = ~ filter(.x, block == "Singletasking"),
  position = position_nudge(x = -0.45)
) +
  stat_pointinterval(
    aes(colour = block, fill = block), alpha = 0.5, width = 0.05,
    data = ~ filter(.x, block == "Multitasking"),
    position = position_nudge(x = 0.45)
  ) +
