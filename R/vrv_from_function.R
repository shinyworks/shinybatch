#' Create a validated reactive value from a validation function
#'
#' A wrapper around [validated_reactive_val()] that uses a provided function
#' to validate and coerce its value. This is a convenience function that
#' constructs the `validation_expr` for you.
#'
#' @param validation_fn (`function`) A function to use for validation.
#' @param ... Quosures of arguments to pass to `validation_fn`.
#' @inheritParams shared-params
#'
#' @returns A `vrv` object.
#' @keywords internal
vrv_from_function <- function(
  validation_fn,
  ...,
  value = NULL,
  default = NULL,
  label = NULL,
  env = rlang::caller_env()
) {
  quos <- rlang::enquos(..., .named = TRUE)
  new_env <- rlang::env(
    !!!quos,
    validation_fn = validation_fn,
    env
  )

  validation_expr <- rlang::quo({
    rlang::inject(
      validation_fn(.vrv(), !!!quos)
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
