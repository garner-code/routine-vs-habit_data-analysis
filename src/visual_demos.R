#####################              DEMO VIS              #######################
#sadie lane, 2026

library(tidyverse)
library(paletteer)
setwd("C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/src")
source("function_dv_histo.R")
source("function_safe_se.R")
source("plot_style.R")
source("function_safe_se.R")
source("function_violin_box_cat.R")

setwd("C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/res")

#read in data
averages_democohs <- read_csv(
  "averages_democohs.csv",
  na = c("", "NA")
)

averages_democohs <- averages_democohs |>
  mutate(
    sub_gender = factor(sub_gender),
    sub_hand = factor(sub_hand)
  ) |>
  filter(ses == 4)

# Gender impact - RT -----------------------------------------------------------

averages_democohs |>
  filter(sub_gender %in% c("Male", "Female")) |>
  violin_box_two_cat(sub_gender, rt_mean) +
  facet_grid(block ~ switch) +
  plot_style() +
  theme_classic() +
  scale_color_paletteer_d("wesanderson::Darjeeling2") +
  scale_fill_paletteer_d("wesanderson::Darjeeling2")

#likely ns
#there is 1 nb participant and 1 transfemme participant who i have left out
#for vis purposes bc only 1 each.

# Gender impact - accuracy ------------------------------------------------

averages_democohs |>
  filter(sub_gender %in% c("Male", "Female")) |>
  violin_box_two_cat(sub_gender, accuracy_mean) +
  facet_grid(block ~ switch) +
  plot_style() +
  theme_classic() +
  scale_color_paletteer_d("wesanderson::Darjeeling2") +
  scale_fill_paletteer_d("wesanderson::Darjeeling2")


# English as first language impact - RT -----------------------------------

averages_democohs |>
  mutate(sub_english = str_detect(sub_lang, "nglish")) |>
  violin_box_two_cat(sub_english, rt_mean) +
  facet_grid(block ~ switch) +
  plot_style() +
  theme_classic() +
  scale_color_paletteer_d("wesanderson::Darjeeling2") +
  scale_fill_paletteer_d("wesanderson::Darjeeling2")



# English as first language impact - accuracy ----------------------------------------

averages_democohs |>
  mutate(sub_english = str_detect(sub_lang, "nglish")) |>
  violin_box_two_cat(sub_english, accuracy_mean) +
  facet_grid(block ~ switch) +
  plot_style() +
  theme_classic() +
  scale_color_paletteer_d("wesanderson::Darjeeling2") +
  scale_fill_paletteer_d("wesanderson::Darjeeling2")

