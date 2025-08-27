#' Create a validated reactive factor-like expression
#'
#' A wrapper around [validated_reactive_val()] to help manage factor-like
#' values, especially in a cascading / dependent manner. This is a convenience
#' function that constructs the `validation_expr` for you by wrapping
#' [stbl::stabilize_fct()]. Note that the objects returned by the resulting
#' `vrv` are `character` vectors, not `factor`s, to allow values to remain
#' unchanged when the allowed levels change but the value is still valid.
#'
#' @param levels (`expression`) An expression that returns a `character` vector
#'   of valid levels. Can be a reactive expression.
#' @inheritParams shared-params
#' @inheritParams stbl::stabilize_fct
#'
#' @returns A `vrv` object which returns a factor-like `character` vector.
#' @export
vrv_factor <- function(
  levels,
  value = NULL,
  default = NULL,
  allow_null = TRUE,
  allow_na = TRUE,
  min_size = NULL,
  max_size = NULL,
  to_na = character(),
  label = NULL,
  env = rlang::caller_env()
) {
  levels_quo <- rlang::enquo(levels)
  new_env <- rlang::env(
    levels_quo = levels_quo,
    allow_null = allow_null,
    allow_na = allow_na,
    min_size = min_size,
    max_size = max_size,
    to_na = to_na,
    env
  )

  validation_expr <- rlang::quo({
    as.character(
      stbl::stabilize_fct(
        .vrv(),
        levels = !!levels_quo,
        allow_null = !!allow_null,
        allow_na = !!allow_na,
        min_size = !!min_size,
        max_size = !!max_size,
        to_na = !!to_na
      )
    )
  })
  rlang::quo_set_env(validation_expr, new_env)

  validated_reactive_val(
    validation_expr = !!validation_expr,
    value = value,
    default = {{ default }},
    label = label,
    env = new_env
  )
}
