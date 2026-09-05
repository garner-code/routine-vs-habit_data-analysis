##################              COHS VISUAL              #######################
#sadie lane, 2026

rm(list=ls())
library(tidyverse)
library(paletteer)
library(ggrepel)
setwd("C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/src")
source("plot_style.R")

setwd("C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/res")

#read in data
averages_democohs <- read_csv(
  "averages_democohs.csv",
  na = c("", "NA")
)

df <- read_csv(
  "split_by_block.csv",
  na = c("", "NA")
)

#7th august - SL:
#note to future self - the cors in this script are probably right (histograms
#suggest the data is normally distributed), but to be sanity checked

# freq histograms --------------------------------------------------------------

#auto
averages_democohs |>
  ggplot(aes(x = auto)) +
  geom_histogram(binwidth = 0.1, colour = "#ECCBAEFF", fill = "#ECCBAEFF") +
  plot_style() +
  theme_classic()

ggsave(
  "freq_histo_cohs_auto.png",
  path = ("C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/plots"),
  width = 14,
  height = 8
)

#routine
averages_democohs |>
  ggplot(aes(x = rout)) +
  geom_histogram(binwidth = 0.1, colour = "#046C9AFF", fill = "#046C9AFF") +
  plot_style() +
  theme_classic()

ggsave(
  "freq_histo_cohs_rout.png",
  path = ("C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/plots"),
  width = 14,
  height = 8
)

# correlation between automaticity and routine ----------------------------

averages_democohs |>
  filter(ses == 4, block == "mt", switch == 0) |>
  ggplot(aes(x = auto, y = rout, label = sub)) +
  geom_point() +
  geom_text_repel() +
  geom_smooth(method = lm, se = F) +
  plot_style() +
  theme_classic() +
  labs(
    subtitle = "r = 0.378"
  )

ggsave(
  "geom_text_rout_v_auto.png",
  path = ("C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/plots"),
  width = 14,
  height = 8
)

cor(averages_democohs$auto, averages_democohs$rout, method = "pearson")


# cor cohs v reclicks and te ----------------------------------------------

#make a df for comparison
#reminder that reclicks are necessarily switch, TE is necessarily stay

#for corrs lets split by mt and st
df_mt <- df |>
  filter(block == "mt")

df_st <- df |>
  filter(block == "st")

#reclicks v auto

df |>
  ggplot(aes(x = auto, y = reclicks_mean, label = sub)) +
  geom_point() +
  geom_text_repel() +
  geom_smooth(method = lm, se = F) +
  facet_wrap(. ~ block) +
  plot_style() +
  theme_classic() +
  labs(
    subtitle = "r = -0.024\nr mt = -0.080\nr st = 0.05"
  )

ggsave(
  "geom_text_reclicks_v_auto.png",
  path = ("C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/plots"),
  width = 14,
  height = 8
)

cor(df$reclicks_mean, df$auto, method = "pearson")
cor(df_mt$reclicks_mean, df_mt$auto, method = "pearson")
cor(df_st$reclicks_mean, df_st$auto, method = "pearson")

#reclicks v rout
df |>
  ggplot(aes(x = rout, y = reclicks_mean, label = sub)) +
  geom_point() +
  geom_text_repel() +
  geom_smooth(method = lm, se = F) +
  facet_wrap(. ~ block) +
  plot_style() +
  theme_classic() +
  labs(
    subtitle = "r = 0.008\nr mt = -0.090\nr st = 0.14"
  )

cor(df$reclicks_mean, df$rout, method = "pearson")
cor(df_mt$reclicks_mean, df_mt$rout, method = "pearson")
cor(df_st$reclicks_mean, df_st$rout, method = "pearson")

ggsave(
  "geom_text_reclicks_v_rout.png",
  path = ("C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/plots"),
  width = 14,
  height = 8
)

#TE v auto
df |>
  ggplot(aes(x = auto, y = TE, label = sub)) +
  geom_point() +
  geom_text_repel() +
  geom_smooth(method = lm, se = F) +
  facet_wrap(. ~ block) +
  plot_style() +
  theme_classic() +
  labs(
    subtitle = "r = -0.019\nr mt = 0.011\nr st = -0.051"
  )

cor(df$TE, df$auto, method = "pearson")
cor(df_mt$TE, df_mt$auto, method = "pearson")
cor(df_st$TE, df_st$auto, method = "pearson")

ggsave(
  "geom_text_TE_v_auto.png",
  path = ("C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/plots"),
  width = 14,
  height = 8
)


#TE v rout
df |>
  ggplot(aes(x = rout, y = TE, label = sub)) +
  geom_point() +
  geom_text_repel() +
  geom_smooth(method = lm, se = F) +
  facet_wrap(. ~ block) +
  plot_style() +
  theme_classic() +
  labs(
    subtitle = "r = 0.038\nr mt = 0.071\nr st = 0.003"
  )

cor(df$TE, df$rout, method = "pearson")
cor(df_mt$TE, df_mt$rout, method = "pearson")
cor(df_st$TE, df_st$rout, method = "pearson")

ggsave(
  "geom_text_TE_v_rout.png",
  path = ("C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/plots"),
  width = 14,
  height = 8
)

