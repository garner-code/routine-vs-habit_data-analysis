safe_se <- function(x) {
  # Remove NAs first
  x <- x[!is.na(x)]
  n <- length(x)

  # Return NA if empty, 0 if only one value (0 variance)
  if (n <= 1) return(0)

  # Standard calculation
  sd(x) / sqrt(n)
}
