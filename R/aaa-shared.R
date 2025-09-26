#' Parameters used in multiple functions
#'
#' Reused parameter definitions are gathered here for easier editing. R6 methods
#' can't `inheritParams`, so you still need to copy/paste to the individual
#' methods.
#'
#' @param allow_na (length-1 `logical`, `expression`, or `reactive expression`)
#'   Are `NA` values okay?
#' @param allow_null (length-1 `logical`, `expression`, or `reactive
#'   expression`) Is `NULL` an acceptable value?
#' @param allow_zero_length (length-1 `logical`, `expression`, or `reactive
#'   expression`) Is a zero-length vector acceptable?
#' @param call (`environment`) The execution environment to mention as the
#'   source of error messages.
#' @param default (various, including `expression` or `reactive expression`) A
#'   value to use when the current value is not valid according to the defined
#'   rules. Defaults to `NULL`.
#' @param env (`environment`) The environment in which to evaluate the
#'   `validation_expr`.
#' @param label (length-1 `character` or `NULL`) An optional label for the
#'   `validated_reactive_value`, used for debugging.
#' @param max_size (length-1 `integer`, `expression`, or `reactive expression`)
#'   The maximum size of the vector.
#' @param message_env (`environment`) The execution environment to use to
#'   evaluate variables in error messages.
#' @param min_size (length-1 `integer`, `expression`, or `reactive expression`)
#'   The minimum size of the vector.
#' @param validation_expr (`expression`) A reactive expression that defines the
#'   authoritative state of this `validated_reactive_value`, This expression can
#'   access the `validated_reactive_value`'s own current value via the `.vrv()`
#'   pronoun to reconcile it with upstream dependencies.
#' @param validation_rctv (`reactive`) The [shiny::reactive()] that performs the
#'   validation.
#' @param value (various) The initial value. This value will be coerced via the
#'   validation expression when accessed. If this value is reactive, an observer
#'   with `priority = Inf` will be created to attempt to keep the validated
#'   value in sync with that reactive.
#' @param value_rv (`reactiveVal`) The [shiny::reactiveVal()] that holds the
#'   current value.
#' @param vrv_fun (`vrv`, `function`) A function to get and set validated reactive
#'   values.
#'
#' @name shared-params
#' @keywords internal
NULL
