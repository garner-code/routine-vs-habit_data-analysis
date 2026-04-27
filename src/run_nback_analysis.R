# get the summary data of Nback performance for further analysis
# NOTE: this assumes that you have already run src/get_n_back_data/get_n_back_data.m
# and that the results are saved to the res/ folder

rm(list=ls())
### sources
library(tidyverse)
library(zeallot) #unpack/destructure with %<-%
library(stringr)
library(here)

# settings for reading and saving
exp <- 'data' # data folder
sv_name <- 'routine_vs_habit_nback' # name of the project, for labelling outputs.

file_path <- here()
data_path <- paste(file_path, 'res', sep='/')

# load data
n_back_dat <- read.csv(file.path(data_path, 'routine_vs_habit_n_back_data_block.csv'))
# filter the sessions we want
n_back_dat <- n_back_dat %>% filter(ses != "practice-multi")
n_back_dat <- n_back_dat %>% # expand the data out so can sum across the two mt blocks for each dv
  mutate(block=replace_na(block, 'prac')) %>%
  select(-ses) %>%
  pivot_wider(id_cols = sub, names_from=block,
              values_from = hits:cor_rej) %>%
  mutate(
    across(
      ends_with("_mt1"),
      ~ .x + coalesce( # take the columns ending with mt1 and mt2 and smoosh them together
        get(str_replace(cur_column(), "_mt1$", "_mt2")),
        0
      ),
      .names = "{str_remove(.col, '_mt1$')}_mt" # get the new name
    )
  ) %>%
  select(-ends_with(c("mt1", "mt2")))

# now I want to pivot longer so observations from each type (prac vs mt) are on their own row
# which basically involves going completely long, and then a little wide
n_back_dat <- n_back_dat %>%
  pivot_longer(
    cols = -sub,
    names_to = c("measure", "block"),
    names_pattern = "(.*)_(prac|mt)",
    values_to = "value"
  ) %>%
  pivot_wider(
    names_from = measure,
    values_from = value
  )

# and now I will calculate sensitivity and specificity
n_back_dat <- n_back_dat %>%
  mutate(sens = hits / (hits + miss),
         spec = cor_rej / (cor_rej + fa))

# and now save the data
nb_fn <- file.path(data_path, paste(paste(sv_name, "sum", sep = "_"), ".csv", sep = ""))
write_csv(n_back_dat, nb_fn)

