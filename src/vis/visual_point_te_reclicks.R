################################################################################
################      Sadie Lane TE v Reclicks Scatter    ######################
################################################################################

rm(list=ls())
library(tidyverse)
library(paletteer)
library(ggrepel)
setwd("C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/src")
source("plot_style.R")

setwd("C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/res")

#read in data
split_by_block <- read_csv(
  "split_by_block.csv",
  na = c("", "NA")
)


#first lets tidy our data in prep of our scatter plot
#so basically use the split_by_block csv

split_by_block <- split_by_block |>
  select(sub:TE)

#first with labels bc it will be interesting
split_by_block |>
  ggplot(aes(x = reclicks_mean, y = TE, label = sub)) +
  geom_point() +
  geom_text_repel() +
  facet_wrap(. ~ block) +
  theme_classic() +
  plot_style()

ggsave(
  "te_v_reclicks_geomtext.png",
  path = ("C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/plots"),
  width = 14,
  height = 8
)

#next im going to eyeball out some outliers (sub 30)
split_by_block |>
  filter(sub != 30) |>
  ggplot(aes(x = reclicks_mean, y = TE)) +
  geom_point() +
  geom_smooth(method = lm, se = F) +
  facet_wrap(. ~ block) +
  theme_classic() +
  plot_style() +
  labs(
    title = "sub 30 removed bc reclicks = 15",
    subtitle = "pearsons r without sub 30\nmt = -0.555\nst = -0.388"
  )

ggsave(
  "te_v_reclicks_without_sub30.png",
  path = ("C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/plots"),
  width = 14,
  height = 8
)


# st only - for thesis ----------------------------------------------------

split_by_block |>
  filter(block == "st") |>
  ggplot(aes(x = reclicks_mean, y = TE)) +
  geom_point(shape = 21, size = 3, fill = "#899DA4FF", colour = "black") +
  geom_smooth(method = lm, formula = 'y ~ x', se = F, colour = "#C93312FF") +
  theme_classic() +
  annotate(
    geom = "text",
    x = 7,
    y = 0.9,
    label = "Pearson's r = -0.370, p < 0.001   \nSpearman's r = -0.450, p < 0.001"
  ) +
  plot_style() +
  labs(
    title = "Reclicks and TE correlate on Singletasking trials",
    subtitle = "Correlation Tests with and without square root ransformation are same to 3 d.p.",
    x = "Mean Reclicks",
    y = "TE"
  )

ggsave(
  "te_v_reclicks_cor_thesis.png",
  path = ("C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/plots"),
  width = 8,
  height = 8
)



