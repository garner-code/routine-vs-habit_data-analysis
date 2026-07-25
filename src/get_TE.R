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

get_wMTE <- function(counts_matrix, p_mat){
  # get the weighted mean transition entropy from a probability matrix and counts matrix
  lps <- log(p_mat)
  occupancy_wgt <- counts_matrix/sum(counts_matrix)
  wMTE = -sum(occupancy_wgt * lps, na.rm = TRUE)
  wMTE
}

get_TE_scores <- function(filt_dat, n_doors = 16) {

  sum_counts <-
    data_2_counts_matrix(
      data = filt_dat$door,
      n_doors = n_doors
    )

  probs <- p_st1_gs(sum_counts, n_doors)
  TE <- get_wMTE(sum_counts, probs)

  return(
    tibble(
      TE = TE
    )
  )
}



