#' Create a validated reactive logical vector
#'
#' A wrapper around [validated_reactive_val()] that uses [stbl::stabilize_lgl()]
#' to validate and coerce its value. This is a convenience function that
#' constructs the `validation_expr` for you.
#'
#' @inheritParams shared-params
#'
#' @returns A `vrv` object which returns a validated logical vector.
#' @export
vrv_lgl <- function(
  value = NULL,
  default = NULL,
  allow_null = TRUE,
  allow_na = TRUE,
  min_size = NULL,
  max_size = NULL,
  label = NULL,
  env = rlang::caller_env()
) {
  vrv_from_function(
    validation_fn = stbl::stabilize_lgl,
    value = {{ value }},
    default = {{ default }},
    label = label,
    env = env,
    allow_null = {{ allow_null }},
    allow_na = {{ allow_na }},
    min_size = {{ min_size }},
    max_size = {{ max_size }}
  )
}

#' @export
#' @rdname vrv_lgl
vrv_lgl_scalar <- function(
  value = NULL,
  default = NULL,
  label = NULL,
  allow_null = TRUE,
  allow_zero_length = TRUE,
  allow_na = TRUE,
  env = rlang::caller_env()
) {
  vrv_from_function(
    validation_fn = stbl::stabilize_lgl_scalar,
    value = {{ value }},
    default = {{ default }},
    label = label,
    env = env,
    allow_null = {{ allow_null }},
    allow_zero_length = {{ allow_zero_length }},
    allow_na = {{ allow_na }}
  )
}
