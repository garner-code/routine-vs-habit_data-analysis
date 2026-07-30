##################              DEMO ANALYSIS              #####################
#sadie lane, z5418956

library(tidyverse)
library(paletteer)

#setwd("C:/Users/user/OneDrive - UNSW/2026!/Honours/Data/routine_habit/res")


averages <- read_csv(
  "routine_vs_habit_avg.csv",
  na = c("", "NA")
)

demos <- read_csv(
  "demographics_42ps.csv",
  na = c("", "NA")
)

vangogh <- paletteer::paletteer_d("vangogh::Chaise")
vangogh2 <- paletteer::paletteer_d("vangogh::Cypresses")
picasso <- paletteer::paletteer_d("lisa::PabloPicasso_1")



# Gender impact - RT -----------------------------------------------------------

#training - stay
train_stay <- averages |>
  filter(ses == 2, switch == 0)

train_stay_joint <-
  full_join(train_stay, demos)


train_stay_joint |>
  group_by(sub_gender) |>
  summarise(
    standard_error = safe_se(rt_mean),
    rt_mean = mean(rt_mean),
    ymin = rt_mean - standard_error,
    ymax = rt_mean + standard_error ) |>
  two_cat_compare(sub_gender, rt_mean, ymin, ymax)
  # NON-SIG TRAIN STAY

train_stay_joint |>
  violin_box_two_cat(sub_gender, rt_mean)



#training - switch
train_switch <- averages |>
  filter(ses == 2, switch == 1)

train_switch_joint <-
  full_join(train_switch, demos)

violin_box_two_cat(train_switch_joint, sub_gender, rt_mean)

train_switch_joint |>
  group_by(sub_gender) |>
  summarise(
    standard_error = safe_se(rt_mean),
    rt_mean = mean(rt_mean),
    ymin = rt_mean - standard_error,
    ymax = rt_mean + standard_error ) |>
  two_cat_compare(sub_gender, rt_mean, ymin, ymax)
  # NON-SIG TRAIN SWITCH


#ses 4 st - stay
st_stay <- averages |>
  filter(ses == 4, block == "st", switch == 0)

st_stay_joint <-
  full_join(st_stay, demos)

violin_box_two_cat(st_stay_joint, sub_gender, rt_mean)

st_stay_joint |>
  group_by(sub_gender) |>
  summarise(
    standard_error = safe_se(rt_mean),
    rt_mean = mean(rt_mean),
    ymin = rt_mean - standard_error,
    ymax = rt_mean + standard_error ) |>
  two_cat_compare(sub_gender, rt_mean, ymin, ymax)
  #NON-SIG ST STAY

#ses 4 st - switch
st_switch <- averages |>
  filter(ses == 4, block == "st", switch == 1)

st_switch_joint <-
  full_join(st_switch, demos)

violin_box_two_cat(st_switch_joint, sub_gender, rt_mean)

st_switch_joint |>
  group_by(sub_gender) |>
  summarise(
    standard_error = safe_se(rt_mean),
    rt_mean = mean(rt_mean),
    ymin = rt_mean - standard_error,
    ymax = rt_mean + standard_error ) |>
  two_cat_compare(sub_gender, rt_mean, ymin, ymax)
  #NON-SIG ST SWITCH


#ses 4 mt - stay
mt_stay <- averages |>
  filter(ses == 4, block == "mt", switch == 0)

mt_stay_joint <-
  full_join(mt_stay, demos)

violin_box_two_cat(mt_stay_joint, sub_gender, rt_mean)

mt_stay_joint |>
  group_by(sub_gender) |>
  summarise(
    standard_error = safe_se(rt_mean),
    rt_mean = mean(rt_mean),
    ymin = rt_mean - standard_error,
    ymax = rt_mean + standard_error ) |>
  two_cat_compare(sub_gender, rt_mean, ymin, ymax)
  #NON-SIG mt stay


#ses 4 mt - switch
mt_switch <- averages |>
  filter(ses == 4, block == "mt", switch == 1)

mt_switch_joint <-
  full_join(mt_switch, demos)

violin_box_two_cat(mt_switch_joint, sub_gender, rt_mean)

mt_switch_joint |>
  group_by(sub_gender) |>
  summarise(
    standard_error = safe_se(rt_mean),
    rt_mean = mean(rt_mean),
    ymin = rt_mean - standard_error,
    ymax = rt_mean + standard_error ) |>
  two_cat_compare(sub_gender, rt_mean, ymin, ymax)
  #NON-SIG MT SWITCH





# Gender impact - accuracy ------------------------------------------------

#training - stay
train_stay <- averages |>
  filter(ses == 2, switch == 0)

train_stay_joint <-
  full_join(train_stay, demos)

violin_box_two_cat(train_stay_joint, sub_gender, accuracy_mean)



train_stay_joint |>
  group_by(sub_gender) |>
  summarise(
    standard_error = safe_se(accuracy_mean),
    accuracy_mean = mean(accuracy_mean),
    ymin = accuracy_mean - standard_error,
    ymax = accuracy_mean + standard_error ) |>
  two_cat_compare(sub_gender, accuracy_mean, ymin, ymax)
# NON-SIG TRAIN STAY


#training - switch
train_switch <- averages |>
  filter(ses == 2, switch == 1)

train_switch_joint <-
  full_join(train_switch, demos)

violin_box_two_cat(train_switch_joint, sub_gender, accuracy_mean)

train_switch_joint |>
  group_by(sub_gender) |>
  summarise(
    standard_error = safe_se(accuracy_mean),
    accuracy_mean = mean(accuracy_mean),
    ymin = accuracy_mean - standard_error,
    ymax = accuracy_mean + standard_error ) |>
  two_cat_compare(sub_gender, accuracy_mean, ymin, ymax)
# NON-SIG TRAIN SWITCH - but only barely here (likely due to variance of
#less switch trials, but keep an eye on it as data accumulates)


#ses 4 st - stay
st_stay <- averages |>
  filter(ses == 4, block == "st", switch == 0)

st_stay_joint <-
  full_join(st_stay, demos)

violin_box_two_cat(st_stay_joint, sub_gender, accuracy_mean)

st_stay_joint |>
  group_by(sub_gender) |>
  summarise(
    standard_error = safe_se(accuracy_mean),
    accuracy_mean = mean(accuracy_mean),
    ymin = accuracy_mean - standard_error,
    ymax = accuracy_mean + standard_error ) |>
  two_cat_compare(sub_gender, accuracy_mean, ymin, ymax)
  #NON-SIG ST STAY

#ses 4 st - switch
st_switch <- averages |>
  filter(ses == 4, block == "st", switch == 1)

st_switch_joint <-
  full_join(st_switch, demos)

violin_box_two_cat(st_switch_joint, sub_gender, accuracy_mean)

st_switch_joint |>
  group_by(sub_gender) |>
  summarise(
    standard_error = safe_se(accuracy_mean),
    accuracy_mean = mean(accuracy_mean),
    ymin = accuracy_mean - standard_error,
    ymax = accuracy_mean + standard_error ) |>
  two_cat_compare(sub_gender, accuracy_mean, ymin, ymax)
  #NON-SIG ST SWITCH


#ses 4 mt - stay
mt_stay <- averages |>
  filter(ses == 4, block == "mt", switch == 0)

mt_stay_joint <-
  full_join(mt_stay, demos)

violin_box_two_cat(mt_stay_joint, sub_gender, accuracy_mean)

mt_stay_joint |>
  group_by(sub_gender) |>
  summarise(
    standard_error = safe_se(accuracy_mean),
    accuracy_mean = mean(accuracy_mean),
    ymin = accuracy_mean - standard_error,
    ymax = accuracy_mean + standard_error ) |>
  two_cat_compare(sub_gender, accuracy_mean, ymin, ymax)
#NON-SIG mt stay, but only barely - keep an eye (once again low n is likely the reason)


#ses 4 mt - switch
mt_switch <- averages |>
  filter(ses == 4, block == "mt", switch == 1)

mt_switch_joint <-
  full_join(mt_switch, demos)

violin_box_two_cat(mt_switch_joint, sub_gender, accuracy_mean)

mt_switch_joint |>
  group_by(sub_gender) |>
  summarise(
    standard_error = safe_se(accuracy_mean),
    accuracy_mean = mean(rt_mean),
    ymin = accuracy_mean - standard_error,
    ymax = accuracy_mean + standard_error ) |>
  two_cat_compare(sub_gender, accuracy_mean, ymin, ymax)
  #NON-SIG MT SWITCH






# English as first language impact - RT -----------------------------------

#training - stay
train_stay_joint |>
  mutate(sub_english = str_detect(sub_language, "nglish")) |>
  group_by(sub_english) |>
  summarise(
    standard_error = safe_se(rt_mean),
    rt_mean = mean(rt_mean),
    ymin = rt_mean - standard_error,
    ymax = rt_mean + standard_error ) |>
  two_cat_compare(sub_english, rt_mean, ymin, ymax)
  #ns

#training - switch
train_switch_joint |>
  mutate(sub_english = str_detect(sub_language, "nglish")) |>
  group_by(sub_english) |>
  summarise(
    standard_error = safe_se(rt_mean),
    rt_mean = mean(rt_mean),
    ymin = rt_mean - standard_error,
    ymax = rt_mean + standard_error ) |>
  two_cat_compare(sub_english, rt_mean, ymin, ymax)
#sig, but once again indicative of likely low n rather than anything else
#ggsave("sig_lang_train_switch_rt.png")



#ses 4 st - stay
st_stay_joint |>
  mutate(sub_english = str_detect(sub_language, "nglish")) |>
  group_by(sub_english) |>
  summarise(
    standard_error = safe_se(rt_mean),
    rt_mean = mean(rt_mean),
    ymin = rt_mean - standard_error,
    ymax = rt_mean + standard_error ) |>
  two_cat_compare(sub_english, rt_mean, ymin, ymax)
  #ns


#ses 4 st - switch
st_switch_joint |>
  mutate(sub_english = str_detect(sub_language, "nglish")) |>
  group_by(sub_english) |>
  summarise(
    standard_error = safe_se(rt_mean),
    rt_mean = mean(rt_mean),
    ymin = rt_mean - standard_error,
    ymax = rt_mean + standard_error ) |>
  two_cat_compare(sub_english, rt_mean, ymin, ymax)
 #ns


#ses 4 mt - stay
mt_stay_joint |>
  mutate(sub_english = str_detect(sub_language, "nglish")) |>
  group_by(sub_english) |>
  summarise(
    standard_error = safe_se(rt_mean),
    rt_mean = mean(rt_mean),
    ymin = rt_mean - standard_error,
    ymax = rt_mean + standard_error ) |>
  two_cat_compare(sub_english, rt_mean, ymin, ymax)
 #ns


#ses 4 mt - switch
mt_switch_joint |>
  mutate(sub_english = str_detect(sub_language, "nglish")) |>
  group_by(sub_english) |>
  summarise(
    standard_error = safe_se(rt_mean),
    rt_mean = mean(rt_mean),
    ymin = rt_mean - standard_error,
    ymax = rt_mean + standard_error ) |>
  two_cat_compare(sub_english, rt_mean, ymin, ymax)
 #probs just sig
 #once again likely low n rather than anything systematic
#ggsave("sig_lang_mt_switch_rt.png")


# English as first language impact - accuracy ----------------------------------------
#super plausible that non-english perform worse than english on multitasking
#presumably due to harder mt with a different language


#training - stay
train_stay_joint |>
  mutate(sub_english = str_detect(sub_language, "nglish")) |>
  group_by(sub_english) |>
  summarise(
    standard_error = safe_se(accuracy_mean),
    accuracy_mean = mean(accuracy_mean),
    ymin = accuracy_mean - standard_error,
    ymax = accuracy_mean + standard_error ) |>
  two_cat_compare(sub_english, accuracy_mean, ymin, ymax)
#non sig

#training - switch
train_switch_joint |>
  mutate(sub_english = str_detect(sub_language, "nglish")) |>
  group_by(sub_english) |>
  summarise(
    standard_error = safe_se(accuracy_mean),
    accuracy_mean = mean(accuracy_mean),
    ymin = accuracy_mean - standard_error,
    ymax = accuracy_mean + standard_error ) |>
  two_cat_compare(sub_english, accuracy_mean, ymin, ymax)

train_switch_joint |>
  mutate(sub_english = str_detect(sub_language, "nglish")) |>
  violin_box_two_cat(sub_english, accuracy_mean) +
  labs(
    title = "training switch only"
  )

ggsave(
  "language_sig_train_switch_acc_42ps.png",
  path = ("C:/Users/user/OneDrive - UNSW/2026!/Honours/Data/routine_habit/images_42ps"),
  width = 14,
  height = 8
)

#ses 4 st - stay
st_stay_joint |>
  mutate(sub_english = str_detect(sub_language, "nglish")) |>
  group_by(sub_english) |>
  summarise(
    standard_error = safe_se(accuracy_mean),
    accuracy_mean = mean(accuracy_mean),
    ymin = accuracy_mean - standard_error,
    ymax = accuracy_mean + standard_error ) |>
  two_cat_compare(sub_english, accuracy_mean, ymin, ymax)
  #non sig

st_stay_joint |>
  mutate(sub_english = str_detect(sub_language, "nglish")) |>
  violin_box_two_cat(sub_english, accuracy_mean)

#ses 4 st - switch
st_switch_joint |>
  mutate(sub_english = str_detect(sub_language, "nglish")) |>
  group_by(sub_english) |>
  summarise(
    standard_error = safe_se(accuracy_mean),
    accuracy_mean = mean(accuracy_mean),
    ymin = accuracy_mean - standard_error,
    ymax = accuracy_mean + standard_error ) |>
  two_cat_compare(sub_english, accuracy_mean, ymin, ymax)
  #non sig


#ses 4 mt - stay
mt_stay_joint |>
  mutate(sub_english = str_detect(sub_language, "nglish")) |>
  group_by(sub_english) |>
  summarise(
    standard_error = safe_se(accuracy_mean),
    accuracy_mean = mean(accuracy_mean),
    ymin = accuracy_mean - standard_error,
    ymax = accuracy_mean + standard_error ) |>
  two_cat_compare(sub_english, accuracy_mean, ymin, ymax)
  #sig
  #ggsave("language_sig_mt_stay_acc.png")

mt_stay_joint |>
  mutate(sub_english = str_detect(sub_language, "nglish")) |>
  violin_box_two_cat(sub_english, accuracy_mean)
#seems like an outlier issue



#ses 4 mt - switch
mt_switch_joint |>
mutate(sub_english = str_detect(sub_language, "nglish")) |>
  group_by(sub_english) |>
  summarise(
    standard_error = safe_se(accuracy_mean),
    accuracy_mean = mean(accuracy_mean),
    ymin = accuracy_mean - standard_error,
    ymax = accuracy_mean + standard_error ) |>
  two_cat_compare(sub_english, accuracy_mean, ymin, ymax)
#ns
