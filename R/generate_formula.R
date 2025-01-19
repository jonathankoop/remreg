#' Generate a Formula for `tie_effects` in `remstats`
#'
#' This function creates a formula for the `tie_effects` argument in the
#' `remstats` package based on a list of parameters and their corresponding values.
#' It translates parameter names into valid `remstats` effect terms
#' and combines them into a formula.
#'
#' @param parameters A named list of parameters and their assigned values.
#'   Parameter names must follow a specific naming convention to be recognized
#'   as valid `remstats` effects. For example, endogenous names could `inertia` or
#'  `psABBA`. Exogenous statistics need to have the respective statistic name from
#'  `remstats::tie_effects(endogenous = FALSE)` followed by `_[variable]`, e.g.
#'  `send_z1`. Unrecognized names will be ignored.
#'
#' @return A formula object of class `"formula"`, suitable for the `tie_effects`
#'   argument in the `remstats::remstats` function. The formula specifies the
#'   effects in the tie-oriented model for which statistics are computed.
#'
#' @details
#' The `generate_formula()` function translates parameter names into effect terms
#' using specific prefixes or direct matches. The following naming conventions
#' are supported:
#'
#' - **Prefixes**: Certain prefixes indicate exogenous effects:
#'   - `difference_`: Generates a `difference('%s', scaling = 'std')` term.
#'   - `same_`: Generates a `same('%s')` term.
#'   - `send_`: Generates a `send('%s', scaling = 'std')` term.
#'   - `receive_`: Generates a `receive('%s', scaling = 'std')` term.
#'   - Additional prefixes like `tie_`, `average_`, `minimum_`, `maximum_`,
#'     and `event_` are also supported.
#'
#' - **Specific Terms**: Endogenous effects are recognized directly by their names, such as:
#'   - `"inertia"`, `"indegreeSender"`, `"reciprocity"`, `"sp"`, etc.
#'   - These names map to their respective terms in `remstats`.
#'
#' - **Standardization**: All effects that support the `scaling` argument
#'   are automatically standardized by default (i.e., `scaling = 'std'` is applied).
#'   This ensures that the effects are computed consistently within the model.
#'
#' Any parameter names that do not match a prefix or specific term will be ignored.
#'
#' The formula always includes an intercept (`1`) by default.
#'
#' @examples
#' # Define parameters
#' parameters <- list(
#'   baseline = -5,
#'   send_z1 = 0.5,
#'   difference_z2 = 0.2,
#'   inertia = 0.3,
#'   receive_z1 = 0,
#'   unknown_param = 1  # This will be ignored
#' )
#'
#' # Generate formula
#' formula <- generate_formula(parameters)
#' print(formula)
#' # Output:
#' # ~ 1 + send('z1', scaling = 'std') +
#' #   difference('z2', scaling = 'std') +
#' #   inertia(scaling = 'std') +
#' #   receive('z1', scaling = 'std')
#'
#' # Use the formula in remstats
#' # remstats::remstats(reh, tie_effects = formula, attr_actors = covar)
#'
#' @seealso [remstats::remstats()] for computing statistics based on the generated formula.
#'
#' @export
generate_formula <- function(parameters) {
  terms <- c("1") # Start with intercept

  # Process each parameter
  for (name in names(parameters)) {
    term <- create_term(name, "difference_", "difference('%s', scaling = 'std')") %||%
      create_term(name, "same_", "same('%s')") %||%
      create_term(name, "send_", "send('%s', scaling = 'std')") %||%
      create_term(name, "receive_", "receive('%s', scaling = 'std')") %||%
      create_term(name, "tie_", "tie('%s', scaling = 'std')") %||%
      create_term(name, "average_", "average('%s', scaling = 'std')") %||%
      create_term(name, "minimum_", "minimum('%s', scaling = 'std')") %||%
      create_term(name, "maximum_", "maximum('%s', scaling = 'std')") %||%
      create_term(name, "event_", "event('%s')") %||%
      create_term(name, "userStat_", "userStat('%s')")

    # Handle specific terms directly
    if (is.null(term)) {
      term <- switch(
        name,
        "inertia" = "inertia(scaling = 'std')",
        "indegreeSender" = "indegreeSender(scaling = 'std')",
        "indegreeReceiver" = "indegreeReceiver(scaling = 'std')",
        "outdegreeSender" = "outdegreeSender(scaling = 'std')",
        "outdegreeReceiver" = "outdegreeReceiver(scaling = 'std')",
        "totaldegreeDyad" = "totaldegreeDyad(scaling = 'std')",
        "totaldegreeSender" = "totaldegreeSender(scaling = 'std')",
        "totaldegreeReceiver" = "totaldegreeReceiver(scaling = 'std')",
        "degreeMin" = "degreeMin(scaling = 'std')",
        "degreeMax" = "degreeMax(scaling = 'std')",
        "degreeDiff" = "degreeDiff(scaling = 'std')",
        "sp" = "sp(scaling = 'std')",
        "reciprocity" = "reciprocity(scaling = 'std')",
        "otp" = "otp(scaling = 'std')",
        "itp" = "itp(scaling = 'std')",
        "osp" = "osp(scaling = 'std')",
        "isp" = "isp(scaling = 'std')",
        "psABBA" = "psABBA()",
        "psABBY" = "psABBY()",
        "psABXA" = "psABXA()",
        "psABXB" = "psABXB()",
        "psABXY" = "psABXY()",
        "psABAY" = "psABAY()",
        "psABAB" = "psABAB()",
        "rrankSend" = "rrankSend()",
        "rrankReceive" = "rrankReceive()",
        "recencySendSender" = "recencySendSender()",
        "recencySendReceiver" = "recencySendReceiver()",
        "recencyReceiveSender" = "recencyReceiveSender()",
        "recencyReceiveReceiver" = "recencyReceiveReceiver()",
        "recencyContinue" = "recencyContinue()",
        "FEtype" = "FEtype()",
        NULL
      )
    }

    if (!is.null(term))
      terms <- c(terms, term)
  }

  # Combine terms into formula
  formula_text <- paste("~", paste(terms, collapse = " + "))
  formula <- stats::as.formula(formula_text)

  return(formula)
}
