#' Calculate the Highest Density Interval (HDI)
#'
#' Computes the smallest interval containing the specified probability mass for a given sample.
#'
#' @param sample A numeric vector representing the sample data.
#' @param prob A numeric value between 0 and 1 specifying the desired probability mass. Defaults to 0.95.
#'
#' @return A numeric vector of length 2 indicating the lower and upper bounds of the HDI.
hdi <- function(sample, prob = 0.95) {
  # Sort the sample
  sorted_sample <- sort(sample)

  # Calculate the number of points in the HDI
  n <- length(sorted_sample)
  interval_length <- floor(prob * n)

  # Initialize the variables to store the minimum interval
  min_width <- Inf
  min_interval <- c(0, 0)

  # Loop over all possible intervals and find the smallest one
  for (i in 1:(n - interval_length)) {
    interval_width <- sorted_sample[i + interval_length] - sorted_sample[i] # calculate width of interval
    if (interval_width < min_width) {
      # if width is smaller than current minimum
      min_width <- interval_width # update minimum width
      min_interval <- c(sorted_sample[i], sorted_sample[i + interval_length]) # update minimum interval
    }
  }

  return(min_interval)
}
