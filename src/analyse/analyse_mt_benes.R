################################################################################
############    Benefits of habits, routines on multitasking    ################
############                  Sadie lane, 2026                  ################
################################################################################

rm(list=ls())
library(tidyverse)
library(broom)

setwd("C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/res")

#read in data
averages <- read_csv(
  "routine_vs_habit_avg.csv",
  na = c("", "NA")
)

perform_dat <- read_csv(
  "perform_dat_errors.csv",
  na = c("", "NA")
)

perform_dat_mt <- read_csv(
  "perform_dat_mt_errors.csv",
  na = c("", "NA")
)

no_thirty <- perform_dat |>
  filter(sub != 30)

# TE and Reclicks ---------------------------------------------------------

rtcost_trsf_mod <- lm(RT_cost ~ TE + sqrt(reclicks_mean), data = perform_dat)
summary(rtcost_trsf_mod)
#ns

rtcost_mod <- lm(RT_cost ~ TE + reclicks_mean, data = perform_dat)
summary(rtcost_mod)
#ns

tjcost_trsf_mod <- lm(tj_cost ~ TE + sqrt(reclicks_mean), data = perform_dat)
summary(tjcost_trsf_mod)
#ns

tjcost_mod <- lm(tj_cost ~ TE + reclicks_mean, data = perform_dat)
summary(tjcost_mod)
#

# TE, reclicks and general errors ----------------------------------------

rtcost_trsf <- lm(RT_cost ~ TE + sqrt(reclicks_mean) + ge_stay, data = perform_dat)
summary(rtcost_trsf)
#ns

rtcost <- lm(RT_cost ~ TE + reclicks_mean + ge_stay, data = perform_dat)
summary(rtcost)
#ns

tjcost_trsf <- lm(tj_cost ~ TE + sqrt(reclicks_mean) + ge_stay, data = perform_dat)
summary(tjcost_trsf)
#ns

tjcost <- lm(tj_cost ~ TE + reclicks_mean + ge_stay, data = perform_dat)
summary(tjcost)
#ns

# correlations ------------------------------------------------------------

#rt versus

  #reclicks

with(perform_dat, cor(reclicks_mean, RT_cost))

  #te

with(perform_dat, cor(TE, RT_cost))

  #ge stay

with(perform_dat, cor(ge_stay, RT_cost))

#tj versus

  #reclicks

with(perform_dat, cor(reclicks_mean, tj_cost))

  #te

with(perform_dat, cor(TE, tj_cost))

  #ge stay

with(perform_dat, cor(ge_stay, tj_cost))



# sub 30 exclusion criteria -----------------------------------------------

summary(perform_dat$reclicks_mean)

sd(perform_dat$reclicks_mean) * 2.5



# check mt ----------------------------------------------------------------

mt_rtcost_mod <- lm(RT_cost ~ TE + reclicks_mean + ge_stay, data = perform_dat_mt)
summary(mt_rtcost_mod)

mt_tjcost_mod <- lm(tj_cost ~ TE + reclicks_mean + ge_stay, data = perform_dat_mt)
summary(mt_tjcost_mod)
#


# no thirty ---------------------------------------------------------------


nothirty_rtcost <- lm(RT_cost ~ TE + reclicks_mean + ge_stay, data = no_thirty)
summary(nothirty_rtcost)

nothirty_tjcost <- lm(tj_cost ~ TE + reclicks_mean + ge_stay, data = no_thirty)
summary(nothirty_tjcost)



# get residuals and corrs for plotting ------------------------------------

### PREDICT RT

#rt x reclicks, holding TE and gen errors

rReclicks_x_rt <- residuals(lm(reclicks_mean ~ TE + ge_stay,  data = perform_dat))
rRT_x_reclicks <- residuals(lm(RT_cost ~ TE + ge_stay, data = perform_dat))

rt_x_reclicks <- data.frame(rReclicks_x_rt, rRT_x_reclicks)

write_csv(rt_x_reclicks, "C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/res/rt_x_reclicks.csv")

with(rt_x_reclicks, cor(rReclicks_x_rt, rRT_x_reclicks))
#-0.1535

#rt x te holding reclicks and gen errors

rTE_x_rt <- residuals(lm(TE ~ reclicks_mean + ge_stay,  data = perform_dat))
rRT_x_TE <- residuals(lm(RT_cost ~ reclicks_mean + ge_stay, data = perform_dat))

rt_x_TE <- data.frame(rTE_x_rt, rRT_x_TE)

write_csv(rt_x_TE, "C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/res/rt_x_TE.csv")

with(rt_x_TE, cor(rTE_x_rt, rRT_x_TE))
#0.0303


#rt x gen errs, holding reclicks and te

rGen_x_rt <- residuals(lm(ge_stay ~ reclicks_mean + TE,  data = perform_dat))
rRT_x_gen <- residuals(lm(RT_cost ~ reclicks_mean + TE, data = perform_dat))

rt_x_gen <- data.frame(rGen_x_rt, rRT_x_gen)

write_csv(rt_x_gen, "C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/res/rt_x_gen.csv")

with(rt_x_gen, cor(rGen_x_rt, rRT_x_gen))
#-0.0373

### PREDICT TJs

#tjs x reclicks, holding TE and gen errors

rReclicks_x_TJ <- residuals(lm(reclicks_mean ~ TE + ge_stay,  data = perform_dat))
rTJ_x_reclicks <- residuals(lm(tj_cost ~ TE + ge_stay, data = perform_dat))

TJ_x_reclicks <- data.frame(rReclicks_x_TJ, rTJ_x_reclicks)

write_csv(TJ_x_reclicks, "C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/res/TJ_x_reclicks.csv")

with(TJ_x_reclicks, cor(rReclicks_x_TJ, rTJ_x_reclicks))
#-0.0307


#tjs x te holding reclicks and gen errors

rTE_x_tj <- residuals(lm(TE ~ reclicks_mean + ge_stay,  data = perform_dat))
rTJ_x_TE <- residuals(lm(tj_cost ~ reclicks_mean + ge_stay, data = perform_dat))

TJ_x_TE <- data.frame(rTE_x_tj, rTJ_x_TE)

write_csv(TJ_x_TE, "C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/res/TJ_x_TE.csv")

with(TJ_x_TE, cor(rTE_x_tj, rTJ_x_TE))\
#0.12865

#tjs x gen errs, holding reclicks and te

rGen_x_tj <- residuals(lm(ge_stay ~ reclicks_mean + TE,  data = perform_dat))
rTJ_x_gen <- residuals(lm(tj_cost ~ reclicks_mean + TE, data = perform_dat))

TJ_x_gen <- data.frame(rGen_x_tj, rTJ_x_gen)

write_csv(TJ_x_gen, "C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/res/TJ_x_gen.csv")

with(TJ_x_gen, cor(rGen_x_tj, rTJ_x_gen))
#0.23296


