#' Evaluate Variable Selection Performance
#'
#' This function evaluates the performance of variable selection methods using
#' the output from `select_variables()`. It computes the True Discovery Rate (TDR)
#' and False Discovery Rate (FDR) for each selection method based on known non-zero
#' and zero parameters in the model.
#'
#' @param selected A list output from `select_variables()`, containing:
#'   - `mle`: Variables selected by the MLE method (if applicable).
#'   - `abr_horseshoe`: Variables selected by the ABR Horseshoe method (if applicable).
#'   - `abr_ridge`: Variables selected by the ABR Ridge method (if applicable).
#'   - `abr_lasso`: Variables selected by the ABR Lasso method (if applicable).
#'   - `parameters`: The original model parameters, where non-zero values indicate true effects.
#'   - `methods`: The methods used for selection.
#'
#' @return A list containing:
#'   - `tdr`: A list of True Discovery Rates (TDR) for each method.
#'   - `fdr`: A list of False Discovery Rates (FDR) for each method.
#'
#' @details
#' The evaluation is performed by comparing the selected variables against the known
#' non-zero and zero parameters:
#' - **True Discovery Rate (TDR)**: The proportion of correctly identified non-zero parameters.
#' - **False Discovery Rate (FDR)**: The proportion of incorrectly identified zero parameters.
#'
#' For each method in `selected$methods`, the function calculates:
#' - `TDR = (number of correctly identified non-zero variables) / (total number of non-zero variables)`
#' - `FDR = (number of incorrectly identified zero variables) / (total number of zero variables)`
#'
#' The results are summarized and printed for user review.
#'
#' @note
#' This function assumes the `parameters` field in `selected` contains the true model
#' parameters, where non-zero values represent true effects and zero values represent
#' noise or irrelevant effects.
#'
#' @examples
#' # Example selected variables from select_variables
#' selected <- list(
#'   mle = c("send_z1", "difference_z2"),
#'   abr_horseshoe = c("send_z1"),
#'   abr_ridge = c("send_z1", "difference_z2", "inertia"),
#'   abr_lasso = c("send_z1"),
#'   parameters = list(
#'     send_z1 = 0.5,
#'     difference_z2 = 0.2,
#'     inertia = 0,
#'     reciprocity = 0
#'   ),
#'   methods = c("mle", "abr_horseshoe", "abr_ridge", "abr_lasso")
#' )
#'
#' # Evaluate selection performance
#' performance <- evaluate_selection(selected)
#'
#' # View results
#' print(performance$tdr)  # True Discovery Rates
#' print(performance$fdr)  # False Discovery Rates
#'
#' @seealso
#' [select_variables()] for selecting variables based on significance criteria.
#'
#' @export
evaluate_selection <- function(selected) {
  # create result list
  result <- list(tdr = list(), fdr = list())

  # get non-zero parameters
  non_zero <- names(selected$parameters)[sapply(selected$parameters, function(x)
    ! is.null(x) && x != 0)]
  # get zero parameters
  zero <- names(selected$parameters)[sapply(selected$parameters, function(x)
    is.null(x) || x == 0)]

  if ("mle" %in% selected$methods) {
    result$tdr$mle <- sum(non_zero %in% selected$mle) / length(non_zero)
    result$fdr$mle <- sum(zero %in% selected$mle) / length(zero)
  }
  if ("abr_horseshoe" %in% selected$methods) {
    result$tdr$abr_horseshoe <- sum(non_zero %in% selected$abr_horseshoe) / length(non_zero)
    result$fdr$abr_horseshoe <- sum(zero %in% selected$abr_horseshoe) / length(zero)
  }
  if ("abr_ridge" %in% selected$methods) {
    result$tdr$abr_ridge <- sum(non_zero %in% selected$abr_ridge) / length(non_zero)
    result$fdr$abr_ridge <- sum(zero %in% selected$abr_ridge) / length(zero)
  }
  if ("abr_lasso" %in% selected$methods) {
    result$tdr$abr_lasso <- sum(non_zero %in% selected$abr_lasso) / length(non_zero)
    result$fdr$abr_lasso <- sum(zero %in% selected$abr_lasso) / length(zero)
  }

  cat("Selection Performance:\n", if ("mle" %in% names(result$tdr)) {
    paste(
      "  MLE:\n",
      "    TDR: ",
      paste(result$tdr$mle, collapse = ", "),
      "\n",
      "    FDR: ",
      paste(result$fdr$mle, collapse = ", "),
      "\n"
    )
  }, if ("abr_horseshoe" %in% names(result$tdr)) {
    paste(
      "  ABR Horseshoe:\n",
      "    TDR: ",
      paste(result$tdr$abr_horseshoe, collapse = ", "),
      "\n",
      "    FDR: ",
      paste(result$fdr$abr_horseshoe, collapse = ", "),
      "\n"
    )
  }, if ("abr_ridge" %in% names(result$tdr)) {
    paste(
      "  ABR Ridge:\n",
      "    TDR: ",
      paste(result$tdr$abr_ridge, collapse = ", "),
      "\n",
      "    FDR: ",
      paste(result$fdr$abr_ridge, collapse = ", "),
      "\n"
    )
  }, if ("abr_lasso" %in% names(result$tdr)) {
    paste(
      "  ABR Lasso:\n",
      "    TDR: ",
      paste(result$tdr$abr_lasso, collapse = ", "),
      "\n",
      "    FDR: ",
      paste(result$fdr$abr_lasso, collapse = ", "),
      "\n"
    )
  })

  return(result)
}
