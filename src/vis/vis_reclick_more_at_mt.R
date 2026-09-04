################################################################################
#####         script for pretty plots from t tests comparing            ########
#####              reclicks at mt vs st, TE at mt v st                  ########
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
remove <- c(8, 9, 11, 13, 22, 25, 28, 51, 61, 73, 76, 85)

graph_reclicks <- averages |>
  filter(ses == 4, switch == 1) |>
  group_by(sub, block) |>
  mutate(
    block = factor(block, c("st", "mt"), c("ST", "MT")),
    reclicks_mean = mean(reclicks_mean),
    TE_mean = mean(TE)
  ) |>
  filter(!sub %in% remove)

graph_TE <- averages |>
  filter(ses == 4, switch == 0) |>
  group_by(sub, block) |>
  mutate(
    block = factor(block, c("st", "mt"), c("ST", "MT")),
    reclicks_mean = mean(reclicks_mean),
    TE_mean = mean(TE)
  ) |>
  filter(!sub %in% remove)

pal_fill <- c("#42439590", "#2C176990", "#90D4CC90","#0A9F9D90" )
pal_colour <- c("#424395FF", "#2C1769FF", "#90D4CCFF", "#0A9F9DFF")

reclicks_pal_fill <- pal_fill[c(1, 2)]
reclicks_pal_colour <- pal_colour[c(1, 2)]

TE_pal_fill <- pal_fill [c(3, 4)]
TE_pal_colour <- pal_colour [c(3, 4)]


# graph -------------------------------------------------------------------

#this is a sig result - add in post bc this format doesn't work with it
graph_reclicks |>
  filter(sub != 30) |> #30 is outlier on both mt and st at mean + 2.5*sd
  ggplot(aes(x = switch, y = reclicks_mean)) +
  stat_slab(
    aes(fill = block), width = 0.3, colour = "black",
    data = ~ filter(.x, block == "ST"),
    side = "left", alpha = 0.5, position = position_nudge(x = -0.5)
  ) +
  stat_slab(
    aes(fill = block), width = 0.3, colour = "black",
    data = ~ filter(.x, block == "MT"),
    side = "right", alpha = 0.5, position = position_nudge(x = 0.5),
  ) +
  geom_boxplot(
    aes(fill = block), alpha = 0.5, width = 0.05, colour = "black",
    data = ~ filter(.x, block == "ST"),
    position = position_nudge(x = -0.45), outlier.color = NA
  ) +
  geom_boxplot(
    aes(fill = block), alpha = 0.5, width = 0.05, colour = "black",
    data = ~ filter(.x, block == "MT"),
    position = position_nudge(x = 0.45), outlier.color = NA
  ) +
  geom_point(
    aes(stroke = 1.1, colour = block, fill = block, group = sub),
    data = ~ filter(.x, block == "ST"),
    position = position_dodgenudge(width = 0.2, x = -0.25), shape = 21, size = 3.5
  ) +
  geom_point(
    aes(stroke = 1.1, colour = block, fill = block, group = sub),
    data = ~ filter(.x, block == "MT"),
    position = position_dodgenudge(width = 0.2, x = 0.25), shape = 21, size = 3.5
  ) +
  scale_fill_manual(values = reclicks_pal_fill) +
  scale_colour_manual(values = reclicks_pal_colour) +
  ylim(0, 10) +
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
    y = "Mean Reclicks"
  )

ggsave(
  "reclicks_dif_ttest_raincloud.png",
  path = ("C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/plots/thesis"),
  width = 4,
  height = 6,
)

ggsave(
  "reclicks_dif_ttest_raincloud.svg",
  path = ("C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/plots/thesis"),
  width = 4,
  height = 6,
)

#te time
#te has no outliers.
#add sig in post

graph_TE |>
  ggplot(aes(x = switch, y = TE_mean)) +
  stat_slab(
    aes(fill = block), width = 0.3, colour = "black",
    data = ~ filter(.x, block == "ST"),
    side = "left", alpha = 0.5, position = position_nudge(x = -0.5)
  ) +
  stat_slab(
    aes(fill = block), width = 0.3, colour = "black",
    data = ~ filter(.x, block == "MT"),
    side = "right", alpha = 0.5, position = position_nudge(x = 0.5),
  ) +
  geom_boxplot(
    aes(fill = block), alpha = 0.5, width = 0.05, colour = "black",
    data = ~ filter(.x, block == "ST"),
    position = position_nudge(x = -0.45), outlier.color = NA
  ) +
  geom_boxplot(
    aes(fill = block), alpha = 0.5, width = 0.05, colour = "black",
    data = ~ filter(.x, block == "MT"),
    position = position_nudge(x = 0.45), outlier.color = NA
  ) +
  geom_point(
    aes(stroke = 1.1, colour = block, fill = block, group = sub),
    data = ~ filter(.x, block == "ST"),
    position = position_dodgenudge(width = 0.2, x = -0.25), shape = 21, size = 3.5
  ) +
  geom_point(
    aes(stroke = 1.1, colour = block, fill = block, group = sub),
    data = ~ filter(.x, block == "MT"),
    position = position_dodgenudge(width = 0.2, x = 0.25), shape = 21, size = 3.5
  ) +
  scale_fill_manual(values = TE_pal_fill) +
  scale_colour_manual(values = TE_pal_colour) +
  ylim(0, 1) +
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
    y = "Mean TE"
  )

ggsave(
  "TE_dif_ttest_raincloud.png",
  path = ("C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/plots/thesis"),
  width = 4,
  height = 6,
)

ggsave(
  "TE_dif_ttest_raincloud.svg",
  path = ("C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/plots/thesis"),
  width = 4,
  height = 6,
)
