#' Parameters used in multiple functions
#'
#' Reused parameter definitions are gathered here for easier editing. R6 methods
#' can't `inheritParams`, so you still need to copy/paste to the individual
#' methods.
#'
#' @param validation_expr (`expression`) A reactive expression that defines the
#'   authoritative state of this `validated_reactive_value`, This expression can
#'   access the `validated_reactive_value`'s own current value via a `self()`
#'   [shiny::reactiveVal()] to reconcile it with upstream dependencies.
#' @param value (various) The initial value.
#' @param label (length-1 `character` or `NULL`) An optional label for the
#'   `validated_reactive_value`, used for debugging.
#' @param env (`environment`) The environment in which to evaluate the
#'   `validation_expr`.
#' @param state_rv (`reactiveVal`) The [shiny::reactiveVal()] that holds the
#'   state.
#' @param validation_rctv (`reactive`) The [shiny::reactive()] that performs the
#'   validation.
#' @param call (`environment`) The execution environment to mention as the
#'   source of error messages.
#' @param message_env (`environment`) The execution environment to use to
#'   evaluate variables in error messages.
#'
#' @name shared-params
#' @keywords internal
NULL
