# now we calculate 'reclicks'. On a switch trial where someone has made no errors
# before jumping away from the previous task,
# how many times did they go around the previous context before jumping to the new
# one? This is a measure of how much they are 'stuck' in the previous context.
get_reclicks <- function(dat){

  # first filter down to switch trials
  tmp <- dat %>% arrange(sub, ses, block, t) %>%
    filter(switch == 1) %>%
    group_by(sub, ses, block, t) %>%
    summarise(
      reclicks = if_else(
        first(door_oc) == 0,
        0L,
        sum(cumprod(door_oc))
      ),
      .groups = "drop"
    ) %>%
    filter(reclicks > 4) %>%
    mutate(reclicks = reclicks - 4) # to get the number over and
   # above the first 4 clicks (which are required to switch)
  dat <- dat %>% left_join(tmp, by = c("sub", "ses", "block", "t")) %>%
    mutate(reclicks = if_else(is.na(reclicks), 0L, reclicks))

  return(dat)
}
