#' Create a validated reactive factor-like character vector
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
#' @param to_na (`expression`) Values to coerce to `NA`. Can be a reactive
#'   expression.
#' @inheritParams shared-params
#'
#' @returns A `vrv` object which returns a factor-like `character` vector.
#' @export
vrv_fct <- function(
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
  vrv_from_function(
    validation_fn = function(...) {
      as.character(stbl::stabilize_fct(...))
    },
    value = {{ value }},
    default = {{ default }},
    label = label,
    env = env,
    levels = {{ levels }},
    allow_null = {{ allow_null }},
    allow_na = {{ allow_na }},
    min_size = {{ min_size }},
    max_size = {{ max_size }},
    to_na = {{ to_na }}
  )
}

#' @export
#' @rdname vrv_fct
#' @param allow_zero_length (`expression`) Is a zero-length vector acceptable?
#'   Can be a reactive expression.
vrv_fct_scalar <- function(
  levels,
  value = NULL,
  default = NULL,
  allow_null = TRUE,
  allow_zero_length = TRUE,
  allow_na = TRUE,
  to_na = character(),
  label = NULL,
  env = rlang::caller_env()
) {
  vrv_from_function(
    validation_fn = function(...) {
      as.character(stbl::stabilize_fct_scalar(...))
    },
    value = {{ value }},
    default = {{ default }},
    label = label,
    env = env,
    levels = {{ levels }},
    allow_null = {{ allow_null }},
    allow_zero_length = {{ allow_zero_length }},
    allow_na = {{ allow_na }},
    to_na = {{ to_na }}
  )
}
