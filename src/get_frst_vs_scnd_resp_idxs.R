index_first_vs_scnd_rsps <- function(dat){

  out <- dat %>%
    group_by(sub, switch, ses, block, t) %>%
    mutate(
      rev_csum = cumsum(rev(door_cc == 0)),
      frst_tsk_resp = as.integer(
        door_cc == 1 &
          rev(rev_csum) == 0 &
          lag(rev(rev_csum), default = 1) > 0
      ),
    ) %>%
    ungroup() %>%
    group_by(sub, switch, ses, block) %>%
    mutate(
      scnd_tsk_resp = lag(frst_tsk_resp, default=0)
    ) %>%
    ungroup() %>%
    select(!rev_csum)

  return(out)
}
