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

#i also want a dataset with just reclicks and just TE (at st) to check for outliers

reclicks_only <- reclicks_te |>
  filter(block == "st", dv == "Reclicks")

te_only <- reclicks_te |>
  filter(block == "st", dv == "TE")

#finally with errors, look at all_errors variable

all_errors_st <- split_by_block |>
  filter(block == "st") |>
  select(sub, block, errors_stay)


# normalcy checks ---------------------------------------------------------

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

#all_errors st only normalcy check
all_errors_st |>
  ggplot(aes(sample = errors_stay)) +
  geom_qq() +
  geom_qq_line() +
  theme_classic() +
  plot_style() +
  labs(
    title = "no transform"
  )

#and with some transforms
all_errors_st |>
  ggplot(aes(sample = log(errors_stay + 0.0001))) +
  geom_qq() +
  geom_qq_line() +
  theme_classic() +
  plot_style() +
  labs(
    title = "log transform"
  )

all_errors_st |>
  ggplot(aes(sample = sqrt(errors_stay))) +
  geom_qq() +
  geom_qq_line() +
  theme_classic() +
  plot_style() +
  labs(
    title = "sqrt transform"
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

summary(reclicks_only)
re_iqr <- IQR(reclicks_only$reclicks_or_TE)
re_iqr_out <- 2.625 + 1.5*re_iqr

re_sd <- sd(reclicks_only$reclicks_or_TE)

re_sd_2.5 <- 1.9801 + 2.5*re_sd
re_sd_3 <- 1.9801 + 3*re_sd

reclicks_only |>
  ggplot(aes(x = reclicks_or_TE)) +
  geom_histogram(binwidth = 0.1, colour = "#ECCBAEFF", fill = "#ECCBAEFF") +
  plot_style() +
  theme_classic() +
  geom_vline(xintercept = re_iqr_out, linetype = 3) +
  geom_vline(xintercept = re_sd_2.5, linetype = 3) +
  geom_vline(xintercept = re_sd_3, linetype = 3) +
  labs(
    title = "in order: iqr, 2.5sd and 3sd"
  )


ggsave(
  "reclicks_st_outlier_check.png",
  path = ("C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/plots"),
  width = 8,
  height = 8
)

#now te
summary(te_only)

te_iqr <- IQR(te_only$reclicks_or_TE)
te_iqr_out <- 0.7018 + te_iqr*1.5

te_sd <- sd(te_only$reclicks_or_TE)
te_mean <- mean(te_only$reclicks_or_TE)
te_sd_2.5 <- te_mean + 2.5*te_sd
te_sd_3 <- te_mean + 3*te_sd

te_only |>
  ggplot(aes(x = reclicks_or_TE)) +
  geom_histogram(binwidth = 0.01, colour = "#046C9AFF", fill = "#046C9AFF") +
  plot_style() +
  theme_classic() +
  geom_vline(xintercept = te_iqr_out, linetype = 3) +
  geom_vline(xintercept = te_sd_2.5, linetype = 3) +
  geom_vline(xintercept = te_sd_3, linetype = 3) +
  labs(
    title = "iqr, then 2.5 and 3 sd"
  )
#nothing to be excluded :)

ggsave(
  "TE_st_outlier_check.png",
  path = ("C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/plots"),
  width = 8,
  height = 8
)

#finally, all_errors histo

summary(all_errors_st)

err_iqr <- IQR(all_errors_st$errors_stay)
err_iqr_out <- 0.0476 + err_iqr*1.5

err_sd <- sd(all_errors_st$errors_stay)
err_mean <- mean(all_errors_st$errors_stay)
err_sd_2.5 <- err_mean + 2.5*err_sd
err_sd_3 <- err_mean + 3*err_sd

all_errors_st |>
  ggplot(aes(x = errors_stay)) +
  geom_histogram(binwidth = 0.01) +
  plot_style() +
  theme_classic() +
  geom_vline(xintercept = err_iqr_out, linetype = 3) +
  geom_vline(xintercept = err_sd_2.5, linetype = 3) +
  geom_vline(xintercept = err_sd_3, linetype = 3) +
  labs(
    title = "iqr, then 2.5 and 3 sd"
  )
