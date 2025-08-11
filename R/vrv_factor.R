#' Create a validated reactive factor
#'
#' A wrapper around [validated_reactive_val()] to help manage factors,
#' especially in a cascading / dependent manner. This is a convenience function
#' that constructs the `validation_expr` for you.
#'
#' @param levels (`expression`) A reactive expression that returns a `character`
#'   vector of valid levels.
#' @param default (`expression`) A reactive expression that returns a
#'   length-1 `character` vector to use as the default value when the current
#'   value is not in `levels`.
#' @inheritParams validated_reactive_val
#'
#' @returns A `validated_reactive_val` object.
#' @export
vrv_factor <- function(
  levels,
  default,
  value = NULL,
  label = NULL,
  env = rlang::caller_env()
) {
  levels_quo <- rlang::enquo(levels)
  default_quo <- rlang::enquo(default)
  new_env <- rlang::env(
    levels_quo = levels_quo,
    default_quo = default_quo,
    env
  )

  validation_expr <- rlang::quo({
    if (isTruthy(.vrv() %in% !!levels_quo)) {
      .vrv()
    } else {
      !!default_quo
    }
  })
  rlang::quo_set_env(validation_expr, new_env)

  validated_reactive_val(
    validation_expr = !!validation_expr,
    value = value,
    label = label,
    env = new_env
  )
}
