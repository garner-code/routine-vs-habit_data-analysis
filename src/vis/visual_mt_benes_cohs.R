################################################################################
############                SL vis benes of rout, auto            ##############
################################################################################

rm(list=ls())
library(tidyverse)
library(paletteer)
setwd("C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/src")
source("plot_style.R")

#change to whatever wd is
setwd("C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/res")

#read in data
averages <- read_csv(
  "routine_vs_habit_avg.csv",
  na = c("", "NA")
)

perform_cohs <- read_csv(
  "perform_cohs.csv",
  na = c("", "NA")
)


# check normalcy of cohs scores -------------------------------------------

perform_cohs |>
  select(sub:rout) |>
  pivot_longer(
    cols = c("auto", "rout"),
    names_to = "cohs_names",
    values_to = "cohs_vals"
  ) |>
  ggplot(aes(sample = cohs_vals, colour = cohs_names)) +
  geom_qq() +
  geom_qq_line() +
  theme_classic() +
  plot_style() +
  paletteer::scale_colour_paletteer_d("wesanderson::Cavalcanti1")

ggsave(
  "qq_cohs.png",
  path = ("C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/plots"),
  width = 8,
  height = 8
)

#auto = #D8B70AFF
#rout = #02401BFF

# now plot benes ----------------------------------------------------------

#auto
perform_cohs |>
  ggplot(aes(x = auto, y = RT_cost)) +
  geom_point(shape = 21, size = 3, fill = "#D8B70AFF", colour = "black") +
  geom_smooth(method = lm, formula = 'y ~ x', se = F, colour = "black") +
  theme_classic() +
  plot_style() +
  labs(
    title = "Auto does not predict rt cost"
  )

perform_cohs |>
  ggplot(aes(x = auto, y = acc_cost)) +
  geom_point(shape = 21, size = 3, fill = "#D8B70AFF", colour = "black") +
  geom_smooth(method = lm, formula = 'y ~ x', se = F, colour = "black") +
  theme_classic() +
  plot_style() +
  labs(
    title = "Auto v acc cost"


#rout

perform_cohs |>
  ggplot(aes(x = rout, y = RT_cost)) +
  geom_point(shape = 21, size = 3, fill = "#02401BFF", colour = "black") +
  geom_smooth(method = lm, formula = 'y ~ x', se = F, colour = "black") +
  theme_classic() +
  plot_style() +
  labs(
    title = "rout v rt cost"
  )

perform_cohs |>
  ggplot(aes(x = rout, y = acc_cost)) +
  geom_point(shape = 21, size = 3, fill = "#02401BFF", colour = "black") +
  geom_smooth(method = lm, formula = 'y ~ x', se = F, colour = "black") +
  theme_classic() +
  plot_style() +
  labs(
    title = "rout v acc"
  )


#both auto and rout

#rt
perform_cohs |>
  pivot_longer(
    cols = c("auto", "rout"),
    names_to = "cohs_names",
    values_to = "cohs_vals"
  ) |>
  ggplot(aes(x = cohs_vals, y = RT_cost, fill = cohs_names, colour = cohs_names)) +
  geom_point(shape = 21, size = 3, colour = "black") +
  geom_smooth(method = lm, formula = 'y ~ x', se = F) +
  theme_classic() +
  plot_style() +
  scale_fill_paletteer_d("wesanderson::Cavalcanti1") +
  scale_colour_paletteer_d("wesanderson::Cavalcanti1")


#acc
perform_cohs |>
  pivot_longer(
    cols = c("auto", "rout"),
    names_to = "cohs_names",
    values_to = "cohs_vals"
  ) |>
  ggplot(aes(x = cohs_vals, y = acc_cost, fill = cohs_names, colour = cohs_names)) +
  geom_point(shape = 21, size = 3, colour = "black") +
  geom_smooth(method = lm, formula = 'y ~ x', se = F) +
  theme_classic() +
  plot_style() +
  scale_fill_paletteer_d("wesanderson::Cavalcanti1") +
  scale_colour_paletteer_d("wesanderson::Cavalcanti1")





