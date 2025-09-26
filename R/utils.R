#' Get a value that might be reactive
#'
#' @param x The value, which might be a reactive expression.
#' @returns The value, or the result of evaluating the reactive expression.
#' @keywords internal
.get_value_maybe_reactive <- function(x) {
  if (is.reactive(x)) {
    x <- x()
  }
  x
}
