#' Parameters used in multiple functions
#'
#' Reused parameter definitions are gathered here for easier editing. R6 methods
#' can't `inheritParams`, so you still need to copy/paste to the individual
#' methods.
#'
#' @param call (`environment`) The execution environment to mention as the
#'   source of error messages.
#' @param message_env (`environment`) The execution environment to use to
#'   evaluate variables in error messages.
#' @param timeout (length-1 `integer`) The maximum number of seconds to wait for
#'   a transaction to complete.
#'
#' @name shared-params
#' @keywords internal
NULL
