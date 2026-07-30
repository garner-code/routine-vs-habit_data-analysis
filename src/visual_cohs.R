
#histo
histogram_cohs(raw_df, sub, auto)
#ggsave("cohs_auto_histo.png")

histogram_cohs(raw_df, sub, rout)
#ggsave("cohs_rout_histo.png")



#linearity/scatter

linearity_check_cohs(raw_df, auto, rout) +
  labs(
    title = "Small pos corr between habit\n and routine on cohs"
  )

ggsave(
  "cohs_routine_by_habit.png",
  path = ("C:/Users/user/OneDrive - UNSW/2026!/Honours/Data/routine_habit/images_42ps"),
  width = 14,
  height = 8,
)


#small pos linear rel where as auto increases, rout increases.


# make df mapped onto averages, check important things --------------------

joint_cohs <- full_join(averages, raw_df, by = 'sub')

joint_cohs_test <- joint_cohs |>
  filter(ses == 4) |>
  mutate(
    switch = factor(switch, levels = c(0, 1), labels = c("Stay", "Switch")),
    block = factor (block, levels = c("st", "mt"), labels = c("Singletask", "Multitask"))
  )

#auto v TE
line_cohs(joint_cohs_test, auto, M_sum_TE)
ggsave(
  "auto_v_te_42ps.png",
  path = ("C:/Users/user/OneDrive - UNSW/2026!/Honours/Data/routine_habit/images_42ps"),
  width = 14,
  height = 8,
)

#auto v reclicks
line_cohs(joint_cohs_test, auto, reclicks_mean)
ggsave(
  "auto_v_reclicks_42ps.png",
  path = ("C:/Users/user/OneDrive - UNSW/2026!/Honours/Data/routine_habit/images_42ps"),
  width = 14,
  height = 8,
)

#with reclicks outlier removed.
joint_cohs_test |>
  filter(sub != 30) |>
  line_cohs(auto, reclicks_mean) +
  labs(
    subtitle = "outlier sub 30 removed"
  )

ggsave(
  "auto_v_reclicks_42ps_no_outlier.png",
  path = ("C:/Users/user/OneDrive - UNSW/2026!/Honours/Data/routine_habit/images_42ps"),
  width = 14,
  height = 8,
)

#rout v TE
line_cohs(joint_cohs_test, rout, M_sum_TE)
ggsave(
  "rout_v_te_42ps.png",
  path = ("C:/Users/user/OneDrive - UNSW/2026!/Honours/Data/routine_habit/images_42ps"),
  width = 14,
  height = 8,
)



#rout v reclicks
line_cohs(joint_cohs_test, rout, reclicks_mean)
ggsave(
  "rout_v_reclicks_42ps.png",
  path = ("C:/Users/user/OneDrive - UNSW/2026!/Honours/Data/routine_habit/images_42ps"),
  width = 14,
  height = 8,
)

#now without outlier sub 30
joint_cohs_test |>
  filter(sub != 30) |>
  line_cohs(rout, reclicks_mean) +
  labs(
    subtitle = "outlier sub 30 removed"
  )

ggsave(
  "rout_v_reclicks_42ps_no_outlier.png",
  path = ("C:/Users/user/OneDrive - UNSW/2026!/Honours/Data/routine_habit/images_42ps"),
  width = 14,
  height = 8,
)
