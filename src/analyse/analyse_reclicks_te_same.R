################################################################################
############    To what extent are reclicks and te the same?    ################
############                  Sadie lane, KGG 2026              ################
################################################################################

rm(list=ls())
library(tidyverse)
library(broom)
library(apaTables)

setwd("C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/res")

#read in data
split_by_block <- read_csv(
  "split_by_block_all.csv",
  na = c("", "NA")
)

averages <- read_csv(
  "routine_vs_habit_avg.csv",
  na = c("", "NA")
)

#make just st
single <- split_by_block |>
  filter(block == "st")

#quickly get singletask RT data for join
just_rt <- averages |>
  filter(ses == 4, block == "st", switch == 0) |>
  select(sub, rt_mean)

single_stay <- left_join(single, just_rt)

# correlation -------------------------------------------------------------

pear_cor_trsf <- with(single_stay, cor(sqrt(reclicks_mean), TE, method = "pearson"))

spear_cor_trsf <- with(single_stay, cor(sqrt(reclicks_mean), TE, method = "spearman"))


# Rsquared change -----------------------------------------------------------

trsf_mod1 <- lm(sqrt(reclicks_mean) ~ TE + log(errors_stay + 0.001), data = single_stay)
trsf_mod1 <- summary(trsf_mod1)

trsf_mod2 <- lm(sqrt(reclicks_mean) ~ log(errors_stay + 0.001), data = single_stay)
trsf_mod2 <- summary(trsf_mod2)

#R^2 change is difference between mod 1 R2 and mod2 R2

#R2 change is 0.20300 - 0.03686 = 0.16614

no_trsfmod1 <- lm(reclicks_mean ~ TE + errors_stay, data = single_stay)
summary(no_trsfmod1)

no_trsfmod2 <- lm(reclicks_mean ~ errors_stay, data = single_stay)
summary(no_trsfmod2)

# residuals ---------------------------------------------------------------

#now run analysis
#first lets do no transform

rReclicks <- residuals(lm(reclicks_mean ~ errors_stay,  data = single_stay))
rTE <- residuals(lm(TE ~ errors_stay, data = single_stay))

errors_partialled <- data.frame(rReclicks, rTE)

write_csv(errors_partialled, "C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/res/errors_partialled_reclicks_TE.csv")


#with sqrt transform of reclicks, and with log transform of errors

trsf_rReclicks <- residuals(lm(sqrt(reclicks_mean) ~ log(errors_stay + 0.001),  data = single_stay))
trsf_rTE <- residuals(lm(TE ~ log(errors_stay + 0.001), data = single_stay))

#save a df
trsf_errors_partialled <- data.frame(trsf_rReclicks, trsf_rTE)
write_csv(trsf_errors_partialled, "C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/res/trsf_errors_partialled_reclicks_TE.csv")

#check cor of residuals
pear_cor_err_part <- with(errors_partialled, cor.test(rReclicks, rTE, method = "pearson"))

pear_cor_err_part_trsf <- with(trsf_errors_partialled, cor.test(trsf_rReclicks, trsf_rTE, method = "pearson"))

#reclicks and errors, partialling out TE

r_re_by_te <- residuals(lm(reclicks_mean ~ TE,  data = single_stay))
r_err_by_te <- residuals(lm(errors_stay ~ TE, data = single_stay))

trsf_r_re_by_te <- residuals(lm(sqrt(reclicks_mean) ~ TE,  data = single_stay))
trsf_r_err_by_te <- residuals(lm(log(errors_stay + 0.001) ~ TE, data = single_stay))

#save a df
TE_partialled <- data.frame(r_re_by_te, r_err_by_te)
write_csv(TE_partialled, "C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/res/TE_partialled_reclicks_errors.csv")


trsf_TE_partialled <- data.frame(trsf_r_re_by_te, trsf_r_err_by_te)
write_csv(trsf_TE_partialled, "C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/res/trsf_TE_partialled_reclicks_errors.csv")

#find cor between residuals

pear_cor_te_part <- with(TE_partialled, cor.test(r_re_by_te, r_err_by_te, method = "pearson"))

pear_cor_TE_part_trsf <- with(trsf_TE_partialled, cor.test(trsf_r_re_by_te, trsf_r_err_by_te, method = "pearson"))


# model with RT -----------------------------------------------------------

#quickly check a model with rt

rt_mod1 <- lm(reclicks_mean ~ TE + errors_stay + rt_mean, data = single_stay)
summary(rt_mod1)

#partial out errors and rt

rReclicks <- residuals(lm(reclicks_mean ~ errors_stay + rt_mean,  data = single_stay))
rTE <- residuals(lm(TE ~ errors_stay + rt_mean, data = single_stay))

rt_errors_partialled <- data.frame(rReclicks, rTE)
write_csv(rt_errors_partialled, "C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/res/rt_errors_partialled.csv")

#and get corrs

with(rt_errors_partialled, cor(rReclicks, rTE, method = "pearson"))
#-0.2308


#get residuals for reclicks x errors graph
rReclicks_tertmean <- residuals(lm(reclicks_mean ~ TE + rt_mean,  data = single_stay))
rErrs_tertmean <- residuals(lm(errors_stay ~ TE + rt_mean, data = single_stay))

rtpart_reclicks_x_errors <- data.frame(rReclicks_tertmean, rErrs_tertmean)
write_csv(rtpart_reclicks_x_errors, "C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/res/rtpart_reclicks_x_errors.csv")

with(rtpart_reclicks_x_errors, cor(rReclicks_tertmean, rErrs_tertmean, method = "pearson"))
#-0.1424

#get residuals for reclicks x rt graph

rReclicks_teerrors <- residuals(lm(reclicks_mean ~ TE + errors_stay,  data = single_stay))
rRTs_teerrors <-  residuals(lm(rt_mean ~ TE + errors_stay,  data = single_stay))

rtpart_reclicks_x_rt <- data.frame(rReclicks_teerrors, rRTs_teerrors)
write_csv(rtpart_reclicks_x_rt, "C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/res/rtpart_reclicks_x_rt.csv")

with(rtpart_reclicks_x_rt, cor(rReclicks_teerrors, rRTs_teerrors, method = "pearson"))
#-0.15977
