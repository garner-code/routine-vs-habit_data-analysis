get_rts <- function(data){
  # this function gets RTs for each event, and adds the variables to the
  # dataframe data

  data <- data %>%
    mutate(start_rt = on - start,
           press_duration = off - on)
}
