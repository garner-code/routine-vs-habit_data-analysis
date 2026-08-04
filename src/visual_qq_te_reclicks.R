################################################################################
################        Sadie Lane TE v Reclicks        ########################
################################################################################

library(tidyverse)
library(paletteer)
setwd("C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/src")
source("function_dv_histo.R")
source("function_safe_se.R")
source("plot_style.R")
source("function_safe_se.R")

setwd("C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/res")

#read in data
averages <- read_csv(
  "routine_vs_habit_avg.csv",
  na = c("", "NA")
)

trials <- read_csv(
  "routine_vs_habit_trl.csv",
  na = c("", "NA")
)

#first lets tidy our data in prep of a qq plot (norm distrib)

reclicks_te <- averages |>
  select(sub:switch, reclicks_mean, TE) |>
  filter(ses == 4) |>
  pivot_wider(
    names_from = switch,
    values_from = c("reclicks_mean", "TE")
  ) |>
  mutate(
    reclicks_mean = reclicks_mean_1,
    TE = TE_0
  ) |>
  select(sub, block, reclicks_mean, TE) |>
  pivot_longer(
    cols = c("reclicks_mean", "TE"),
    names_to = "dv",
    values_to = "reclicks_or_TE"
  )

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


# and some histograms while we are here -----------------------------------

reclicks_te |>
  filter(sub != 30, dv == "reclicks_mean") |>
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
