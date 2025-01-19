#' Select Significant Variables from REM Estimates
#'
#' This function selects variables that meet specified significance criteria from the results of
#' Relational Event Models (REMs) estimated using `estimate_rems()`. It supports significance
#' selection for both Maximum Likelihood Estimation (MLE) and Approximate Bayesian Regularization
#' (ABR) methods with various priors.
#'
#' @param estimates A list output from `estimate_rems()`, containing REM results, including:
#'   - `coefs_mle`: Coefficients from MLE (if computed).
#'   - `coefs_abr_horseshoe`: ABR results with Horseshoe prior (if computed).
#'   - `coefs_abr_ridge`: ABR results with Ridge prior (if computed).
#'   - `coefs_abr_lasso`: ABR results with Lasso prior (if computed).
#'   - Other metadata such as `parameters`, `covar`, `methods`, `directed`, and `seed`.
#' @param criterion_mle A character vector specifying the criterion for selecting variables
#'   from MLE results. Options include:
#'   - `"p<0.05"`: Select variables with p-values less than 0.05.
#'   - `"p<0.01"`: Select variables with p-values less than 0.01.
#'   - `"p<0.001"`: Select variables with p-values less than 0.001.
#'   Must specify exactly one criterion.
#' @param criterion_abr A character vector specifying the criterion for selecting variables
#'   from ABR results. Options include:
#'   - `"95% hdi"`: Select variables whose 95% highest density interval (HDI) does not include 0.
#'   - `"99% hdi"`: Select variables whose 99% HDI does not include 0.
#'   - `"mean>0.1"`: Select variables whose absolute shrunk mean exceeds 0.1.
#'   - `"median>0.1"`: Select variables whose absolute shrunk median exceeds 0.1.
#'   - `"mode>0.1"`: Select variables whose absolute shrunk mode exceeds 0.1.
#'   Must specify exactly one criterion.
#'
#' @return A list containing:
#'   - `mle`: Variables selected from MLE results (if applicable).
#'   - `abr_horseshoe`: Variables selected from ABR results with Horseshoe prior (if applicable).
#'   - `abr_ridge`: Variables selected from ABR results with Ridge prior (if applicable).
#'   - `abr_lasso`: Variables selected from ABR results with Lasso prior (if applicable).
#'   - Original metadata from the `estimates` input: `parameters`, `covar`, `methods`, `directed`, and `seed`.
#'
#' @details
#' The function performs the following steps:
#'
#' 1. **MLE Selection**: Variables are selected based on the p-values in `coefs_mle` for the specified
#'    criterion (`p<0.05`, `p<0.01`, or `p<0.001`).
#' 2. **ABR Selection**: For each ABR method (`abr_horseshoe`, `abr_ridge`, `abr_lasso`) in the input:
#'    - HDI-based criteria (`95% hdi`, `99% hdi`) select variables whose highest density interval
#'      does not include 0.
#'    - Mean, median, or mode-based criteria (`mean>0.1`, `median>0.1`, `mode>0.1`) select variables
#'      with absolute values of the respective statistic exceeding 0.1.
#' 3. The selected variables are combined into the output list and printed for user review.
#'
#' @note
#' Only one criterion can be specified for `criterion_mle` and `criterion_abr`. If multiple criteria
#' are provided for either, the function will lead to an error.
#'
#' @examples
#' # Example input from estimate_rems
#' estimates <- estimate_rems(
#'   edgelist = list(
#'     edgelist = data.frame(time = 1:10,
#'                           actor1 = sample(1:3, 10, replace = TRUE),
#'                           actor2 = sample(1:3, 10, replace = TRUE)),
#'     parameters = list(baseline = -5, send_z1 = 0.5, inertia = 0.3),
#'     covar = data.frame(id = 1:3, z1 = rnorm(3)),
#'     directed = TRUE
#'   ),
#'   methods = c("mle", "abr_horseshoe")
#' )
#'
#' # Select variables using p<0.05 for MLE and 95% HDI for ABR Horseshoe
#' selected <- select_variables(estimates, criterion_mle = "p<0.05", criterion_abr = "95% hdi")
#'
#' # View selected variables
#' print(selected$mle)                # Variables from MLE
#' print(selected$abr_horseshoe)      # Variables from ABR Horseshoe
#'
#' @seealso
#' [estimate_rems()] for estimating relational event models.
#' [shrinkem::shrinkem()] for Approximate Bayesian Regularization.
#'
#' @export
select_variables <- function(estimates,
                             criterion_mle = c("p<0.05", "p<0.01", "p<0.001"),
                             criterion_abr = c("95% hdi", "99% hdi", "mean>0.1", "median>0.1", "mode>0.1")) {
  if (length(criterion_mle) > 1 || length(criterion_abr) > 1) {
    stop("One criterion has to be selected for mle and abr each.")
  }

  result <- list()

  if ("mle" %in% estimates$methods) {
    coefs_mle <- estimates$coefs_mle
    for (criterion in criterion_mle) {
      if (criterion == "p<0.05") {
        result$mle <- union(result$mle, rownames(coefs_mle[coefs_mle[, 5] < 0.05, ]))
      }
      if (criterion == "p<0.01") {
        result$mle <- union(result$mle, rownames(coefs_mle[coefs_mle[, 5] < 0.01, ]))
      }
      if (criterion == "p<0.001") {
        result$mle <- union(result$mle, rownames(coefs_mle[coefs_mle[, 5] < 0.001, ]))
      }
    }
  }

  if ("abr_horseshoe" %in% estimates$methods) {
    samples <- estimates$abr$abr_horseshoe$draws$beta
    for (criterion in criterion_abr) {
      if (criterion == "95% hdi") {
        hdis <- apply(samples, 2, hdi, prob = 0.95)
        result$abr_horseshoe <- union(result$abr_horseshoe, colnames(samples)[apply(hdis, 2, function(x)
          x[1] > 0 | x[2] < 0)])
      }
      if (criterion == "99% hdi") {
        hdis <- apply(samples, 2, hdi, prob = 0.99)
        result$abr_horseshoe <- union(result$abr_horseshoe, colnames(samples)[apply(hdis, 2, function(x)
          x[1] > 0 | x[2] < 0)])
      }
      if (criterion == "mean>0.1") {
        coefs <- estimates$coefs_abr_horseshoe
        result$abr_horseshoe <- union(result$abr_horseshoe, rownames(coefs[abs(coefs$shrunk.mean) > 0.1, ]))
      }
      if (criterion == "median>0.1") {
        coefs <- estimates$coefs_abr_horseshoe
        result$abr_horseshoe <- union(result$abr_horseshoe, rownames(coefs[abs(coefs$shrunk.median) > 0.1, ]))
      }
      if (criterion == "mode>0.1") {
        coefs <- estimates$coefs_abr_horseshoe
        result$abr_horseshoe <- union(result$abr_horseshoe, rownames(coefs[abs(coefs$shrunk.mode) > 0.1, ]))
      }
    }
  }

  if ("abr_ridge" %in% estimates$methods) {
    samples <- estimates$abr$abr_ridge$draws$beta
    for (criterion in criterion_abr) {
      if (criterion == "95% hdi") {
        hdis <- apply(samples, 2, hdi, prob = 0.95)
        result$abr_ridge <- union(result$abr_ridge, colnames(samples)[apply(hdis, 2, function(x)
          x[1] > 0 | x[2] < 0)])
      }
      if (criterion == "99% hdi") {
        hdis <- apply(samples, 2, hdi, prob = 0.99)
        result$abr_ridge <- union(result$abr_ridge, colnames(samples)[apply(hdis, 2, function(x)
          x[1] > 0 | x[2] < 0)])
      }
      if (criterion == "mean>0.1") {
        coefs <- estimates$coefs_abr_ridge
        result$abr_ridge <- union(result$abr_ridge, rownames(coefs[abs(coefs$shrunk.mean) > 0.1, ]))
      }
      if (criterion == "median>0.1") {
        coefs <- estimates$coefs_abr_ridge
        result$abr_ridge <- union(result$abr_ridge, rownames(coefs[abs(coefs$shrunk.median) > 0.1, ]))
      }
      if (criterion == "mode>0.1") {
        coefs <- estimates$coefs_abr_ridge
        result$abr_ridge <- union(result$abr_ridge, rownames(coefs[abs(coefs$shrunk.mode) > 0.1, ]))
      }
    }
  }

  if ("abr_lasso" %in% estimates$methods) {
    samples <- estimates$abr$abr_lasso$draws$beta
    for (criterion in criterion_abr) {
      if (criterion == "95% hdi") {
        hdis <- apply(samples, 2, hdi, prob = 0.95)
        result$abr_lasso <- union(result$abr_lasso, colnames(samples)[apply(hdis, 2, function(x)
          x[1] > 0 | x[2] < 0)])
      }
      if (criterion == "99% hdi") {
        hdis <- apply(samples, 2, hdi, prob = 0.99)
        result$abr_lasso <- union(result$abr_lasso, colnames(samples)[apply(hdis, 2, function(x)
          x[1] > 0 | x[2] < 0)])
      }
      if (criterion == "mean>0.1") {
        coefs <- estimates$coefs_abr_lasso
        result$abr_lasso <- union(result$abr_lasso, rownames(coefs[abs(coefs$shrunk.mean) > 0.1, ]))
      }
      if (criterion == "median>0.1") {
        coefs <- estimates$coefs_abr_lasso
        result$abr_lasso <- union(result$abr_lasso, rownames(coefs[abs(coefs$shrunk.median) > 0.1, ]))
      }
      if (criterion == "mode>0.1") {
        coefs <- estimates$coefs_abr_lasso
        result$abr_lasso <- union(result$abr_lasso, rownames(coefs[abs(coefs$shrunk.mode) > 0.1, ]))
      }
    }
  }

  result$parameters <- estimates$parameters
  result$covar <- estimates$covar
  result$methods <- estimates$methods
  result$directed <- estimates$directed
  result$seed <- estimates$seed

  cat("Selected variables:\n", # if results$mle exists print the selected variables
      if ("mle" %in% names(result)) {
        paste("MLE:", paste(result$mle, collapse = ", "), "\n")
      }, # if results$abr_horseshoe exists print the selected variables
      if ("abr_horseshoe" %in% names(result)) {
        paste("ABR Horseshoe:",
              paste(result$abr_horseshoe, collapse = ", "),
              "\n")
      }, # if results$abr_ridge exists print the selected variables
      if ("abr_ridge" %in% names(result)) {
        paste("ABR Ridge:", paste(result$abr_ridge, collapse = ", "), "\n")
      }, # if results$abr_lasso exists print the selected variables
      if ("abr_lasso" %in% names(result)) {
        paste("ABR Lasso:", paste(result$abr_lasso, collapse = ", "), "\n")
      })

  return(result)

}
