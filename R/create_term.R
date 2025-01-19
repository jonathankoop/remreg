#' Create Term for `remstats` Effects
#'
#' Extracts a variable from a prefixed string and formats it as a `remstats` effect term.
#'
#' @param name A string to process.
#' @param prefix The prefix indicating the effect type.
#' @param template A template string, where `%s` represents the extracted variable.
#'
#' @return A formatted string if `name` matches `prefix`; otherwise, `NULL`.
create_term <- function(name, prefix, template) {
  if (grepl(paste0("^", prefix), name)) {
    var <- sub(paste0("^", prefix), "", name)
    return(sprintf(template, var))
  }
  return(NULL)
}
