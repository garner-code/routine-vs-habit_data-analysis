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
    ges_mean = mean(general_errors_mean)
  )

pal_fill <- c("#A6CEE390", "#1F78B490", "#B2DF8A90", "#33A02C90", "#FDBF6F90", "#FF7F0090")
pal_colour <- paletteer_d("RColorBrewer::Paired")

rt_pal_fill <- pal_fill[c(1, 2)]
rt_pal_colour <- pal_colour[c(1, 2)]

tj_pal_fill <- pal_fill[c(3, 4)]
tj_pal_colour <- pal_colour[c(3, 4)]

ge_pal_fill <- pal_fill[c(5, 6)]
ge_pal_colour <- pal_colour[c(7, 8)]

# plot --------------------------------------------------------------------

graph_ts |>
  ggplot(aes(x = switch, y = rt_mean)) +
  stat_slab(
    aes(fill = block), width = 0.3, colour = "black",
    data = ~ filter(.x, block == "Singletasking"),
    side = "left", alpha = 0.5, position = position_nudge(x = -0.5)
    ) +
  stat_slab(
    aes(fill = block), width = 0.3, colour = "black",
    data = ~ filter(.x, block == "Multitasking"),
    side = "right", alpha = 0.5, position = position_nudge(x = 0.5),
  ) +
  geom_boxplot(
    aes(fill = block), alpha = 0.5, width = 0.05, colour = "black",
    data = ~ filter(.x, block == "Singletasking"),
    position = position_nudge(x = -0.45), outlier.color = NA
  ) +
  geom_boxplot(
    aes(fill = block), alpha = 0.5, width = 0.05, colour = "black",
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
    axis.ticks.x = element_blank(),
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

#remove outliers first
outs <- c(8, 9, 11, 13, 22, 25, 28, 51, 61, 73, 76, 85)

graph_ts |>
  filter(!sub %in% outs) |>
  ggplot(aes(x = switch, y = tj_mean)) +
  stat_slab(
    aes(fill = block), width = 0.3, colour = "black",
    data = ~ filter(.x, block == "Singletasking"),
    side = "left", alpha = 0.5, position = position_nudge(x = -0.5)
  ) +
  stat_slab(
    aes(fill = block), width = 0.3, colour = "black",
    data = ~ filter(.x, block == "Multitasking"),
    side = "right", alpha = 0.5, position = position_nudge(x = 0.5),
  ) +
  geom_boxplot(
    aes(fill = block), alpha = 0.5, width = 0.05, colour = "black",
    data = ~ filter(.x, block == "Singletasking"),
    position = position_nudge(x = -0.45), outlier.color = NA
  ) +
  geom_boxplot(
    aes(fill = block), alpha = 0.5, width = 0.05, colour = "black",
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
  scale_fill_manual(values = tj_pal_fill) +
  scale_colour_manual(values = tj_pal_colour) +
  plot_style() +
  theme(
    axis.title = element_text(face = "bold"),
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.title.y = element_text(margin = margin (r = 15)),
    axis.line = element_line(colour = "grey"),
    axis.ticks.x = element_blank(),
    legend.position = "none",
    strip.background = element_rect(fill = "white", color = "white", linewidth = 0.5)
  ) +
  labs(
    y = "Mean Task Jumps"
  )

ggsave(
  "tj_dif_ttest_raincloud.png",
  path = ("C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/plots/thesis"),
  width = 4,
  height = 6,
)

# gen errors
#remove outliers as well

graph_ts |>
  filter(!sub %in% outs) |>
  ggplot(aes(x = switch, y = ges_mean)) +
  stat_slab(
    aes(fill = block), width = 0.3, colour = "black",
    data = ~ filter(.x, block == "Singletasking"),
    side = "left", alpha = 0.5, position = position_nudge(x = -0.5)
  ) +
  stat_slab(
    aes(fill = block), width = 0.3, colour = "black",
    data = ~ filter(.x, block == "Multitasking"),
    side = "right", alpha = 0.5, position = position_nudge(x = 0.5),
  ) +
  geom_boxplot(
    aes(fill = block), alpha = 0.5, width = 0.05, colour = "black",
    data = ~ filter(.x, block == "Singletasking"),
    position = position_nudge(x = -0.45), outlier.color = NA
  ) +
  geom_boxplot(
    aes(fill = block), alpha = 0.5, width = 0.05, colour = "black",
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
  scale_fill_manual(values = ge_pal_fill) +
  scale_colour_manual(values = ge_pal_colour) +
  plot_style() +
  theme(
    axis.title = element_text(face = "bold"),
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.title.y = element_text(margin = margin (r = 15)),
    axis.line = element_line(colour = "grey"),
    axis.ticks.x = element_blank(),
    legend.position = "none",
    strip.background = element_rect(fill = "white", color = "white", linewidth = 0.5)
  ) +
  labs(
    y = "Mean Non-Context Errors"
  )

ggsave(
  "ges_dif_ttest_raincloud.png",
  path = ("C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/plots/thesis"),
  width = 4,
  height = 6,
)

