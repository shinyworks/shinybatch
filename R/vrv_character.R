#' Create a validated reactive character vector
#'
#' A wrapper around [validated_reactive_val()] that uses [stbl::stabilize_chr()]
#' to validate and coerce its value. This is a convenience function that
#' constructs the `validation_expr` for you.
#'
#' @inheritParams shared-params
#' @inheritParams stbl::stabilize_chr
#'
#' @returns A `vrv` object which returns a validated character vector.
#' @export
vrv_character <- function(
  value = NULL,
  default = NULL,
  allow_null = TRUE,
  allow_na = TRUE,
  min_size = NULL,
  max_size = NULL,
  regex = NULL,
  label = NULL,
  env = rlang::caller_env()
) {
  new_env <- rlang::env(
    allow_null = allow_null,
    allow_na = allow_na,
    min_size = min_size,
    max_size = max_size,
    regex = regex,
    env
  )

  validation_expr <- rlang::quo({
    stbl::stabilize_chr(
      .vrv(),
      allow_null = !!allow_null,
      allow_na = !!allow_na,
      min_size = !!min_size,
      max_size = !!max_size,
      regex = !!regex
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

#' @export
#' @rdname vrv_character
vrv_character_scalar <- function(
  value = NULL,
  default = NULL,
  label = NULL,
  allow_null = TRUE,
  allow_zero_length = TRUE,
  allow_na = TRUE,
  regex = NULL,
  env = rlang::caller_env()
) {
  new_env <- rlang::env(
    allow_null = allow_null,
    allow_zero_length = allow_zero_length,
    allow_na = allow_na,
    regex = regex,
    env
  )

  validation_expr <- rlang::quo({
    stbl::stabilize_chr_scalar(
      .vrv(),
      allow_null = !!allow_null,
      allow_zero_length = !!allow_zero_length,
      allow_na = !!allow_na,
      regex = !!regex
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
