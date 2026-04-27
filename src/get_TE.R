get_TEs_for_all <- function(dat, n_doors = 16){
  # this function applies the below functions to grp_data to get
  # TE scores for all


}


H <- function(x){
  # to be applied over the rows of the matrix of transitions
  -sum(x * log(x), na.rm = TRUE)
}

data_2_counts_matrix <- function(data, n_doors){
  # turn data into a counts matrix of transitions made
  mat <- matrix(rep(0, times=n_doors*n_doors), nrow=n_doors, ncol=n_doors)
  idxs <- matrix(c(data[1:length(data)-1], data[2:length(data)]), nrow=2, byrow=TRUE)
  for(i in 1:ncol(idxs)){
    mat[idxs[1,i],idxs[2,i]] <- mat[idxs[1,i],idxs[2,i]] + 1
  }
  mat
}

p_st1_gs <- function(counts_matrix, n_doors){
  # convert the counts matrix into the row probabilities
  denom <- matrix(rep(rowSums(counts_matrix), n_doors), nrow=n_doors, byrow=FALSE)
  out <- counts_matrix / denom
  out[is.na(out)] = 0
  out
}

get_TE_scores <- function(filt_dat, n_doors = 16) {
  counts <- lapply(
    unique(filt_dat$t),
    function(x)
      data_2_counts_matrix(
        data = filt_dat$door[filt_dat$t == x],
        n_doors = n_doors
      )
  )

  sum_counts <- Reduce(`+`, counts)
  probs <- p_st1_gs(sum_counts, n_doors)
  row_ent <- apply(probs, 1, H)

  tibble(
    sum_TE = sum(row_ent),
    mu_TE  = mean(row_ent)
  )
}

# get_TE_scores <- function(filt_dat, n_doors=16){
#   # enter the rows of the dataframe for which you would like the
#   # TE scores computed
#   # e.g. sub x ses x block x context x switch
#   # first, get a transition counts matrix per trial
#   counts <- lapply(unique(filt_dat$t), function(x) data_2_counts_matrix(data = filt_dat$door[filt_dat$t==x], n_doors=n_doors))
#   sum_counts <- Reduce(`+`, counts)
#   probs <- p_st1_gs(counts_matrix = sum_counts, n_doors = n_doors)
#   row_ent <- apply(probs, 1, H)
#   filt_dat %>% summarise(
#     across(c(sub, ses, context, block, switch),
#            first
#     )
#   ) %>%
#     mutate(sum_TE = sum(row_ent),
#            mu_TE = mean(row_ent))
# }

