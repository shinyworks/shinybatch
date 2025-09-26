#' Create a validated reactive integer vector
#'
#' A wrapper around [validated_reactive_val()] that uses [stbl::stabilize_int()]
#' to validate and coerce its value. This is a convenience function that
#' constructs the `validation_expr` for you.
#'
#' @param coerce_character (length-1 `logical`, `expression`, or `reactive
#'   expression`) Should character vectors such as `"1"` and `"2.0"` be coerced
#'   to integer?
#' @param coerce_factor (length-1 `logical`, `expression`, or `reactive
#'   expression`) Should factors with values such as `"1"` and `"2.0"` be
#'   coerced to integer? Note that this function uses the character value from
#'   the factor, while `as.integer()` uses the integer index of the factor.
#' @param min_value (length-1 `integer`, `expression`, or `reactive expression`,
#'   or `NULL`) The lowest allowed value for `x`. If `NULL` (default) values are
#'   not checked.
#' @param max_value (length-1 `integer`, `expression`, or `reactive expression`,
#'   or `NULL`) The highest allowed value for `x`. If `NULL` (default) values
#'   are not checked.
#' @inheritParams shared-params
#'
#' @returns A `vrv` object which returns a validated integer vector.
#' @export
vrv_integer <- function(
  value = NULL,
  default = NULL,
  allow_null = TRUE,
  allow_na = TRUE,
  min_size = NULL,
  max_size = NULL,
  coerce_character = TRUE,
  coerce_factor = TRUE,
  min_value = NULL,
  max_value = NULL,
  label = NULL,
  env = rlang::caller_env()
) {
  vrv_from_function(
    validation_fn = stbl::stabilize_int,
    value = {{ value }},
    default = {{ default }},
    label = label,
    env = env,
    allow_null = {{ allow_null }},
    allow_na = {{ allow_na }},
    min_size = {{ min_size }},
    max_size = {{ max_size }},
    coerce_character = {{ coerce_character }},
    coerce_factor = {{ coerce_factor }},
    min_value = {{ min_value }},
    max_value = {{ max_value }}
  )
}

#' @export
#' @rdname vrv_integer
vrv_integer_scalar <- function(
  value = NULL,
  default = NULL,
  label = NULL,
  allow_null = TRUE,
  allow_zero_length = TRUE,
  allow_na = TRUE,
  coerce_character = TRUE,
  coerce_factor = TRUE,
  min_value = NULL,
  max_value = NULL,
  env = rlang::caller_env()
) {
  vrv_from_function(
    validation_fn = stbl::stabilize_int_scalar,
    value = {{ value }},
    default = {{ default }},
    label = label,
    env = env,
    allow_null = {{ allow_null }},
    allow_zero_length = {{ allow_zero_length }},
    allow_na = {{ allow_na }},
    coerce_character = {{ coerce_character }},
    coerce_factor = {{ coerce_factor }},
    min_value = {{ min_value }},
    max_value = {{ max_value }}
  )
}
