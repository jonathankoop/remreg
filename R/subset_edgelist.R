#' Subset Multiple Edgelists
#'
#' This function subsets each edgelist in a list of edgelists to include only
#' the first `m` rows.
#'
#' @param edgelists A list of edgelists. Each edgelist should be a data frame
#'   with at least the following columns:
#'   - `time`: The time of each event.
#'   - `actor1`: The identifier of the first actor involved in the event.
#'   - `actor2`: The identifier of the second actor involved in the event.
#' @param m An integer specifying the number of rows to retain in each edgelist.
#'
#' @return A list of edgelists, where each edgelist has been subset to contain only
#'   the first `m` rows.
#'
#' @details
#' The `subset_edgelists()` function is useful for processing multiple relational event histories (REHs) by truncating them to a specified number of events.
#' This is particularly helpful when comparing or analyzing event histories of different lengths.
#'
#' The function iterates through each edgelist in the input list and subsets it
#' using standard R subsetting (`[1:m, ]`).
#'
#' @note
#' The function assumes that each edgelist in the input list is a data frame
#' with at least the columns `time`, `actor1`, and `actor2`. If an edgelist
#' contains fewer than `m` rows, the entire edgelist is returned.
#'
#' @examples
#' # Example edgelists
#' edgelists <- list(
#'   data.frame(time = 1:10,
#'              actor1 = sample(1:5, 10, replace = TRUE),
#'              actor2 = sample(1:5, 10, replace = TRUE)),
#'   data.frame(time = 1:15,
#'              actor1 = sample(1:5, 15, replace = TRUE),
#'              actor2 = sample(1:5, 15, replace = TRUE))
#' )
#'
#' # Subset each edgelist to the first 5 rows
#' subsetted <- subset_edgelists(edgelists, 5)
#'
#' # View the subsetted edgelists
#' print(subsetted[[1]])
#' print(subsetted[[2]])
#'
#' @seealso
#' Functions that generate or process relational event histories, such as `generate_reh()`.
#'
#' @export
subset_edgelists <- function(edgelists, m) {
  for (i in seq_along(edgelists)) {
    edgelists[[i]] <- edgelists[[i]][1:m,]
  }
  return(edgelists)
}
