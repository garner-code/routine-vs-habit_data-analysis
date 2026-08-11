################################################################################
##########                      Sadie Lane 2026                  ###############
##########              checking normalcy of reclicks, TE        ###############
################################################################################

rm(list=ls())
library(tidyverse)
library(paletteer)
setwd("C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/src")
source("plot_style.R")

setwd("C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/res")

#read in data
split_by_block <- read_csv(
  "split_by_block.csv",
  na = c("", "NA")
)


#first lets tidy our data in prep of a qq plot (norm distrib)

reclicks_te <- split_by_block |>
  mutate(
    Reclicks = reclicks_mean
  ) |>
  select(sub, block, Reclicks, TE) |>
  pivot_longer(
    cols = c("Reclicks", "TE"),
    names_to = "dv",
    values_to = "reclicks_or_TE"
  )

# run the analysis of TE corrd with reclicks as is and as sqrtd
reclicks_te |>
  ggplot(aes(sample = reclicks_or_TE, colour = dv)) +
  geom_qq() +
  geom_qq_line() +
  theme_classic() +
  plot_style() +
  facet_wrap(. ~ block) +
  scale_color_paletteer_d("wesanderson::Darjeeling2") +
  labs(
    subtitle = "participant 30 is the outlier"
  )

ggsave(
  "qq_facet_block.png",
  path = ("C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/plots"),
  width = 14,
  height = 8
)

reclicks_te |>
  ggplot(aes(sample = sqrt(reclicks_or_TE), colour = dv)) +
  geom_qq() +
  geom_qq_line() +
  theme_classic() +
  plot_style() +
  facet_wrap(. ~ block) +
  scale_color_paletteer_d("wesanderson::Darjeeling2") +
  labs(
    subtitle = "participant 30 is the outlier"
  )

ggsave(
  "qq_facet_block_sqrt.png",
  path = ("C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/plots"),
  width = 14,
  height = 8
)

# with outlier p30 gone

reclicks_te |>
  filter(sub != 30) |>
  ggplot(aes(sample = reclicks_or_TE, colour = dv)) +
  geom_qq() +
  geom_qq_line() +
  theme_classic() +
  plot_style() +
  facet_wrap(. ~ block) +
  scale_color_paletteer_d("wesanderson::Darjeeling2") +
  labs(
    subtitle = "participant 30 is the outlier"
  )

ggsave(
  "qq_facet_block_no30.png",
  path = ("C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/plots"),
  width = 14,
  height = 8
)

reclicks_te |>
  filter(sub != 30) |>
  ggplot(aes(sample = sqrt(reclicks_or_TE), colour = dv)) +
  geom_qq() +
  geom_qq_line() +
  theme_classic() +
  plot_style() +
  facet_wrap(. ~ block) +
  scale_color_paletteer_d("wesanderson::Darjeeling2") +
  labs(
    subtitle = "participant 30 is the outlier"
  )

ggsave(
  "qq_facet_block_no30_sqrt.png",
  path = ("C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/plots"),
  width = 14,
  height = 8
)


# thesis - st only with sub 30 --------------------------------------------

#no transform
reclicks_te |>
  filter(block == "st") |>
  ggplot(aes(sample = reclicks_or_TE, colour = dv)) +
  geom_qq() +
  geom_qq_line() +
  theme_classic() +
  plot_style() +
  scale_color_paletteer_d("wesanderson::Darjeeling2") +
  labs(
    title = "TE is approximately normally distributed\nwithout transformation",
    subtitle = "N = 85",
    colour = "Dependent Variable"
  )

ggsave(
  "qq_rq1_thesis_sqrt.png",
  path = ("C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/plots"),
  width = 8,
  height = 8
)

#sqrt
reclicks_te |>
  filter(block == "st") |>
  ggplot(aes(sample = sqrt(reclicks_or_TE), colour = dv)) +
  geom_qq() +
  geom_qq_line() +
  theme_classic() +
  plot_style() +
  scale_color_paletteer_d("wesanderson::Darjeeling2") +
  labs(
    title = "Reclicks are normally distributed with a square root transformation",
    subtitle = "N = 85",
    colour = "Dependent Variable"
  )
ggsave(
  "qq_rq1_thesis_notransform.png",
  path = ("C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/plots"),
  width = 8,
  height = 8
)


# and some histograms while we are here -----------------------------------
#histos aren't sqrtd

reclicks_te |>
  filter(dv == "reclicks_mean") |>
  ggplot(aes(x = reclicks_or_TE)) +
  geom_histogram(binwidth = 0.1, colour = "#ECCBAEFF", fill = "#ECCBAEFF") +
  facet_wrap(. ~ block) +
  plot_style() +
  theme_classic() +
  labs(
    title = "reclicks freq distrib",
    subtitle = "sub 30 excluded (15 mean reclicks)",
    x = "mean reclicks"
  ) +
  theme(
    strip.background = element_rect(fill = "white", color = "white", linewidth = 0.5),
  )

ggsave(
  "freq_histo_reclicks_faceted_no30.png",
  path = ("C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/plots"),
  width = 14,
  height = 8
)

reclicks_te |>
  filter(sub != 30, dv == "TE") |>
  ggplot(aes(x = reclicks_or_TE)) +
  geom_histogram(binwidth = 0.01, colour = "#046C9AFF", fill = "#046C9AFF") +
  facet_wrap(. ~ block) +
  plot_style() +
  theme_classic() +
  labs(
    title = "TE freq distrib",
    subtitle = "sub 30 excluded (as in reclicks data)",
    x = "mean TE score"
  ) +
  theme(
    strip.background = element_rect(fill = "white", color = "white", linewidth = 0.5),
  )

ggsave(
  "freq_histo_TE_faceted_no30.png",
  path = ("C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/plots"),
  width = 14,
  height = 8
)

