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
  "split_by_block.csv",
  na = c("", "NA")
)

#make just st
single_stay <- split_by_block |>
  filter(block == "st")

# correlation -------------------------------------------------------------

pear_cor_trsf <- with(single_stay, cor(sqrt(reclicks_mean), TE, method = "pearson"))

spear_cor_trsf <- with(single_stay, cor(sqrt(reclicks_mean), TE, method = "spearman"))


# linear models -----------------------------------------------------------

mod1 <- lm(reclicks_mean ~ TE + errors_stay, data = single_stay)
summary(mod1)

trsf_mod1 <- lm(sqrt(reclicks_mean) ~ TE + log(errors_stay + 0.001), data = single_stay)
trsf_mod1 <- summary(trsf_mod1)

trsf_mod2 <- lm(sqrt(reclicks_mean) ~ TE, data = single_stay)
trsf_mod2 <- summary(trsf_mod2)


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

#check cor of residuals when trsf all errors partialled out

pear_cor_err_part <- with(trsf_errors_partialled, cor(trsf_rReclicks, trsf_rTE, method = "pearson"))


#reclicks and errors, partialling out TE

trsf_r_re_by_te <- residuals(lm(sqrt(reclicks_mean) ~ TE,  data = single_stay))
trsf_r_err_by_te <- residuals(lm(log(errors_stay + 0.001) ~ TE, data = single_stay))

#save a df
trsf_TE_partialled <- data.frame(trsf_r_re_by_te, trsf_r_err_by_te)
write_csv(trsf_TE_partialled, "C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/res/trsf_TE_partialled_reclicks_errors.csv")

#find cor between residuals

pear_cor_TE_part <- with(trsf_TE_partialled, cor(trsf_r_re_by_te, trsf_r_err_by_te, method = "pearson"))


#cor residuals reclicks, te

s

