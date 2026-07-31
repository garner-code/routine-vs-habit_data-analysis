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


#so on stay trials TE is calced, and on switch trials Reclicks are calculated.
#so now to reorganise the data such that we may compare the two?

#now plot

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
  path = ("C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/plots"),
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
  ggplot(aes(x = reclicks_mean, y = TE)) +
  filter(reclicks_mean < 8, M_sum_TE < 10) |>
  ggplot(aes(x = reclicks_mean, y = M_sum_TE)) +
  ggplot(aes(x = reclicks_mean, y = TE)) +
  filter(reclicks_mean < 8, M_sum_TE < 10) |>
  ggplot(aes(x = reclicks_mean, y = M_sum_TE)) +
  geom_jitter() +
  geom_smooth(method = lm, formula = "y ~ x", linewidth = 1.25, se = F) +
  facet_wrap( ~ block ) +
  theme_classic() +
  plot_style() +
  plot_style() +
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
  "reclicks_te.png",
  path = ("C:/Users/Sadie/Repos/routine-vs-habit_data-analysis/plots"),
  width = 14,
  height = 8,
)
