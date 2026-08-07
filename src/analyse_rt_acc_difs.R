################################################################################
########    Did our response time manip lead to slower rt in mt con?    ########
########                    Sadie lane, 2026                            ########
################################################################################

library(tidyverse)

setwd("C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/res")

#read in data
averages <- read_csv(
  "averages_democohs.csv",
  na = c("", "NA")
)


# tidy data --------------------------------------------------------------------

averages |>
  filter(ses == 4 & switch == 0) |>
  group_by(block) |>
  summarise(
    M = mean(rt_mean),
    SD = sd(rt_mean),
    accM = mean(accuracy_mean),
    accSD = sd(accuracy_mean)
  )


for_t_tests <- averages |>
  filter(ses == 4 & switch == 0) |>
  select(sub, block, rt_mean, accuracy_mean) |>
  pivot_wider(
    names_from = block,
    values_from = c(rt_mean, accuracy_mean)
  )

# analyse ----------------------------------------------------------------------
# run t tests
# (and summaries)

# do RT with and without log
with(for_t_tests, t.test(log(rt_mean_st), log(rt_mean_mt)))
with(for_t_tests, t.test(accuracy_mean_st, accuracy_mean_mt))
