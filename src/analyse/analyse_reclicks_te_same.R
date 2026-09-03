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

pear_cor_trsf <- with(single_stay, cor.test(sqrt(reclicks_mean), TE, method = "pearson"))

spear_cor_trsf <- with(single_stay, cor.test(sqrt(reclicks_mean), TE, method = "spearman"))


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


#plot relationship (residual x residual) between errors and reclicks
#i.e save a df

errors_stay_vec <- single_stay$errors_stay

reclicks_x_errors <- data.frame(trsf_rReclicks, log(errors_stay_vec + 0.001))

reclicks_x_errors <- reclicks_x_errors |>
  rename(
    log_errors_stay = log.errors_stay_vec...0.001.
  )

write_csv(reclicks_x_errors, "C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/res/trsf_reclicks_x_errors.csv")



