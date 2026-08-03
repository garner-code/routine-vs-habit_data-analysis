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
    mt_reclicks     = reclicks_mean_mt_1,
    st_reclicks     = reclicks_mean_st_1,
    mt_TE           = TE_mt_0,
    st_TE           = TE_st_0,
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
    sub, mt_reclicks:st_TE, acc_cost_stay, acc_cost_switch, rt_cost_stay,
    rt_cost_switch
    ) |>
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

# now vis -----------------------------------------------------------------

#routines and mt cost
  #rt

mt_cost_long |>
  ggplot(aes(x = rt_cost_stay, y = TE))

  #acc




#habits and mt cost

  #rt

  #acc
