################################################################################
#####         script for pretty plots from t tests comparing            ########
#####                 rt, task jumps and gen errors                     ########
#####                      Sadie Lane, 2026                             ########
################################################################################

rm(list=ls())
library(tidyverse)
library(paletteer)
library(ggsignif)
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
  summarise(
    rt_mean = mean(rt_mean),
    tj_mean = mean(task_jumps_mean),
    ges_mean = mean(general_errors_mean)
  )

# plot --------------------------------------------------------------------
pal <- paletteer_d("RColorBrewer::Paired")

#response time
#palette

graph_ts |>
  ggplot(aes(x = block, y = rt_mean)) +
  geom_violin(aes(colour = block, fill = block), alpha = 0.3) +
  geom_point(
    aes(colour = block, fill = block, shape = block, group = sub)
  ) +
  geom_line(
    aes(group = sub),
    alpha = 0.5,
    colour = "grey"
  ) +
  geom_signif(
    data = graph_ts,
    comparisons = list(c("Singletasking","Multitasking")),
    annotation = "*"
    ) +
  stat_summary(fun = "mean", geom = "point", color = "black", size = 2) +
  scale_color_paletteer_d("RColorBrewer::Paired") +
  scale_fill_paletteer_d("RColorBrewer::Paired") +
  theme_classic() +
  plot_style() +
  theme(
    strip.background = element_rect(fill = "white", color = "white", linewidth = 0.5),
    axis.title.x = element_blank(),
    legend.position = "none"
  ) +
  ylim(c(0, 1)) +
  labs(
    title = "Response Time is significantly\nslower when Multitasking",
    y = "Mean Response Time (ms)",
    colour = "Block",
    fill = "Block"
  )

ggsave(
  "rt_dif_ttest.png",
  path = ("C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/plots/thesis"),
  width = 6,
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

