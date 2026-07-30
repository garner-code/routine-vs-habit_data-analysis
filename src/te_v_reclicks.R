################################################################################
################        Sadie Lane TE v Reclicks        ########################
################################################################################

library(tidyverse)
library(paletteer)
setwd("C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/src")
source("function_dv_histo.R")
source("function_safe_se.R")
source("plot_style.R")
source("function_safe_se.R")

averages |>
  filter(ses == 4, switch == 1) |>
  ggplot(aes(x = reclicks_mean, y = M_sum_TE)) +
  geom_jitter() +
  geom_smooth(method = lm, linewidth = 1.25, se = F) +
  facet_wrap( ~ block) +
  theme_classic(base_size = 26) +
  theme(axis.text.x = element_text(size = 22),
        axis.text.y = element_text(size = 22),
        axis.title.y = element_text(size = 24),
        axis.title.x = element_text(size = 24),
        plot.subtitle = element_text(size = 22)
  )
ggsave(
  "reclicks_te_block_42ps.png",
  path = ("C:/Users/user/OneDrive - UNSW/2026!/Honours/Data/routine_habit/images_42ps"),
  width = 14,
  height = 8,
)

averages |>
  mutate(
    block = factor(
      block,
      levels = c("st", "mt"),
      labels = c("Singletask", "Multitask")
    )
  ) |>
  filter(ses == 4, switch == 1) |>
  filter(reclicks_mean < 8, M_sum_TE < 10) |>
  ggplot(aes(x = reclicks_mean, y = M_sum_TE)) +
  geom_jitter() +
  geom_smooth(method = lm, formula = "y ~ x", linewidth = 1.25, se = F) +
  facet_wrap( ~ block ) +
  theme_classic() +
  theme(axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        axis.title.y = element_text(size = 22),
        axis.title.x = element_text(size = 22),
        plot.title = element_text(size = 24),
        plot.subtitle = element_text(size = 20)
  ) +
  labs(
    title = "Reclicks and Transition Entropy are weakly correlated",
    x = "Mean Reclicks",
    y = "Transition Entropy"
  )
ggsave(
  "reclicks_te_block_42ps_outlierless.png",
  path = ("C:/Users/user/OneDrive - UNSW/2026!/Honours/Data/routine_habit/images_42ps"),
  width = 14,
  height = 8,
)
