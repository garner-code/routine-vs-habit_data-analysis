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
pal_fill <- c("#A6CEE390", "#1F78B490", "#B2DF8A90", "#33A02C90", "#FDBF6F90", "#FF7F0090")
pal_colour <- paletteer_d("RColorBrewer::Paired")

rt_pal_fill <- pal_fill[c(1, 2)]
rt_pal_colour <-pal_colour[c(1,2)]

#response time
#palette

graph_ts |>
  ggplot(aes(x = block, y = rt_mean)) +
  stat_summary(fun = "mean", geom = "col", fill = "#899DA4FF", colour = "#899DA4FF") +
  geom_point(
    aes(size = 2.5, stroke = 1.1, colour = block, fill = block, group = sub),
    position = position_dodge(width = 0.5), shape = 21
  ) +
  geom_signif(
    data = graph_ts, comparisons = list(c("Singletasking","Multitasking")),
    annotation = "*", margin_top = 0.1, size = 1.2, textsize = 10, vjust = 0.5
  ) +
  scale_fill_manual(values = rt_pal_fill) +
  scale_colour_manual(values = rt_pal_colour) +
  plot_style() +
  theme(
    axis.title = element_text(face = "bold"),
    axis.title.x = element_blank(),
    axis.title.y = element_text(margin = margin (r = 15)),
    axis.line = element_line(colour = "grey"),
    axis.ticks = element_line(colour = "grey"),
    axis.text.x = element_text(angle = 60, hjust = 1),
    legend.position = "none",
    strip.background = element_rect(fill = "white", color = "white", linewidth = 0.5)
  ) +
  labs(
    y = "Mean RT (ms)"
  )

ggsave(
  "rt_dif_ttest.png",
  path = ("C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/plots/thesis"),
  width = 3,
  height = 6,
)


 ####################

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
  plot_style() +
  theme(
    axis.title = element_text(face = "bold"),
    axis.title.x = element_text(margin = margin (t = 15)),
    axis.title.y = element_text(margin = margin (r = 15)),
    axis.line = element_line(colour = "grey"),
    axis.ticks = element_line(colour = "grey"),
    legend.position = "none"
  ) +
  ylim(c(0, 1)) +
  labs(
    colour = "Block",
    fill = "Block"
  )

ggsave(
  "rt_dif_ttest.png",
  path = ("C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/plots/thesis"),
  width = 6,
  height = 6,
)
