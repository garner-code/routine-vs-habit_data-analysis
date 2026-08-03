################################################################################
########    Cost of multitasking in habs, routines on rt and acc    ############
########                      Sadie Lane, 2026                      ############
################################################################################

library(tidyverse)
library(paletteer)
setwd("C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/src")
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


# now the specific qs. First lets tidy data --------------------------------

#reclicks
averages |>
  filter(ses == 4, switch == 1) |>
  select(sub, block, accuracy_mean, rt_mean, reclicks_mean) |>
  ggplot(aes(x = ))





#so make a longform df with each person having 16 measurments

#so we want reclick scores and te scores
#then we want one col (factor) showing acc or rt, another col with dif cost.
#and we will eventually separate by switch
mt_cost <- averages |>
  select(sub:switch, accuracy_mean, rt_mean, reclicks_mean, TE) |>
  filter(ses == 4) |>
  pivot_wider(
    names_from = c("block", "switch"),
    values_from = c("accuracy_mean", "rt_mean", "reclicks_mean", "TE")
  ) |>
  mutate(
    mt_reclicks_NA  = reclicks_mean_mt_1,
    st_reclicks_NA  = reclicks_mean_st_1,
    mt_TE_NA        = TE_mt_0,
    st_TE_NA        = TE_st_0,
    mt_acc_stay     = accuracy_mean_mt_0,
    mt_acc_switch   = accuracy_mean_mt_1,
    st_acc_stay     = accuracy_mean_st_0,
    st_acc_switch   = accuracy_mean_st_1,
    acc_cost_stay   = mt_acc_stay - st_acc_stay,
    acc_cost_switch = mt_acc_switch - st_acc_switch,
    mt_rt_stay      = rt_mean_mt_0,
    mt_rt_switch    = rt_mean_mt_1,
    st_rt_stay      = rt_mean_st_0,
    st_rt_switch    = rt_mean_st_1,
    rt_cost_stay    = mt_rt_stay - st_rt_stay,
    rt_cost_switch  = mt_rt_switch - st_rt_switch
  ) |>
  select(
    sub, mt_reclicks_NA:rt_cost_switch
    )

#now make long again
long_mt_cost <- mt_cost |>
  select(sub:st_acc_switch, mt_rt_stay:st_rt_switch) |>
  pivot_longer(
    cols = c("mt_reclicks_NA":"st_rt_switch"),
    names_to = "block",
    values_to = "value"
  ) |>
  separate_wider_delim(
    cols = block,
    delim = "_",
    names = c("block", "re_or_te", "stay_or_switch")
  ) |>
  mutate(
    block = factor(block),
    dv = factor(re_or_te),
    switch = factor(stay_or_switch)
  ) |>
  select(sub, block, dv, switch, value)
long_mt_cost$switch[long_mt_cost$switch == "NA"] <- NA

#and one final widening
#there was definitely a better way to do this...

mt_cost_two <- long_mt_cost |>
  pivot_wider(
    names_from = dv,
    values_from = value
  ) |>
  pivot_longer(
    cols = c("acc", "rt"),
    names_to = "acc_or_rt",
    values_to = "val_acc_rt"
  ) |>
  pivot_longer(
    cols = c("reclicks", "TE"),
    names_to = "reclicks_or_TE",
    values_to = "val_re_TE"
  )

# vis time ----------------------------------------------------------------

mt_cost_2 |>
  filter()
  ggplot(aes(x = dv, y = measure))



#now that difference has been calced, make long again
mt_cost_long <- mt_cost |>
  pivot_longer(
    cols = c("mt_reclicks", "st_reclicks", "mt_TE", "st_TE"),
    names_to = "block",
    values_to = "value"
  ) |>
  separate_wider_delim(
    cols = block,
    delim = "_",
    names = c("block", "re_or_te")
  )

mt_cost_means <- mt_cost_long|>
  pivot_wider(
    names_from = "block",
    values_from = "value"
  ) |>
  mutate(
    mean = mt - st
  )

# now vis -----------------------------------------------------------------

#routines and mt cost
  #rt

mt_cost_long |>
  filter(re_or_te == "TE") |>
  ggplot(aes(x = rt_cost_stay, y = value)) +
  geom_point(colour = "#428C5CFF", size = 2) +
  geom_smooth(method = lm, se = F, colour = "#A7CFF2FF") +
  facet_wrap(. ~ block) +
  plot_style() +
  theme_classic() +
  labs(
    title = "TE x RT cost",
    x = "RT Cost (ms, Mt - ST)",
    y = "TE"
  ) +
  theme(
    strip.background = element_blank()
  )

  #acc

mt_cost_long |>
  filter(re_or_te == "TE") |>
  ggplot(aes(x = acc_cost_stay, y = value)) +
  geom_point(colour = "#428C5CFF", size = 2) +
  geom_smooth(method = lm, se = F, colour = "#A7CFF2FF") +
  facet_wrap(. ~ block) +
  plot_style() +
  theme_classic() +
  labs(
    title = "TE x acc cost",
    x = "Accuracy Cost (MT - ST)",
    y = "TE"
  ) +
  theme(
    strip.background = element_blank()
  )



#habits and mt cost

  #rt
mt_cost_means |>
  filter(re_or_te == "reclicks", sub != 30) |>
  ggplot(aes(x = rt_cost_switch, y = mean)) +
  geom_point(colour = "#F4B5BDFF", size = 2) +
  geom_smooth(method = lm, se = F, colour = "#A7CFF2FF") +
  plot_style() +
  theme_classic() +
  labs(
    title = "Reclicks x RT cost",
    x = "RT Cost (ms, MT - ST)",
    y = "Reclicks"
  )

  #acc

mt_cost_means |>
  filter(re_or_te == "reclicks", sub != 30) |>
  ggplot(aes(x = acc_cost_switch, y = mean)) +
  geom_point(colour = "#F4B5BDFF", size = 2) +
  geom_smooth(method = lm, se = F, colour = "#A7CFF2FF") +
  plot_style() +
  theme_classic() +
  labs(
    title = "Reclicks x RT cost",
    x = "Accuracy Cost (MT - ST)",
    y = "Reclicks"
  )


# take 2  -----------------------------------------------------------------




