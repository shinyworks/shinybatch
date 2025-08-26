#' Parameters used in multiple functions
#'
#' Reused parameter definitions are gathered here for easier editing. R6 methods
#' can't `inheritParams`, so you still need to copy/paste to the individual
#' methods.
#'
#' @param call (`environment`) The execution environment to mention as the
#'   source of error messages.
#' @param default A value to use when the current value is not valid according
#'   to the rules. Can be a reactive expression. Defaults to `NULL`.
#' @param env (`environment`) The environment in which to evaluate the
#'   `validation_expr`.
#' @param label (length-1 `character` or `NULL`) An optional label for the
#'   `validated_reactive_value`, used for debugging.
#' @param message_env (`environment`) The execution environment to use to
#'   evaluate variables in error messages.
#' @param state_rv (`reactiveVal`) The [shiny::reactiveVal()] that holds the
#'   state.
#' @param validation_expr (`expression`) A reactive expression that defines the
#'   authoritative state of this `validated_reactive_value`, This expression can
#'   access the `validated_reactive_value`'s own current value via the `.vrv()`
#'   pronoun to reconcile it with upstream dependencies.
#' @param validation_rctv (`reactive`) The [shiny::reactive()] that performs the
#'   validation.
#' @param value (various) The initial value.
#' @param value_rv (`reactiveVal`) The [shiny::reactiveVal()] that holds the
#'   current value.
#'
#' @name shared-params
#' @keywords internal
NULL
