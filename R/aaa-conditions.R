#' Wrap an expression with error handling
#'
#' A wrapper around [rlang::try_fetch()] that allows for cleanup code to be run
#' before re-throwing an error with [shinybatch_abort()].
#'
#' @inheritParams shinybatch_abort
#' @param expr (`expression`) The expression to evaluate.
#' @param before_error (`expression` or `NULL`) An expression to run before
#'   aborting, typically for cleanup.
#'
#' @returns The result of `expr` if successful.
#' @keywords internal
with_error_handling <- function(expr,
                                message,
                                subclass,
                                before_error = NULL,
                                call = rlang::caller_env(),
                                message_env = call) {
  rlang::try_fetch(
    expr,
    error = function(cnd) {
      rlang::inject(before_error)
      shinybatch_abort(
        message,
        parent = cnd,
        subclass = subclass,
        call = call,
        .envir = message_env
      )
    }
  )
}

#' Signal an error with standards applied
#'
#' A wrapper around [cli::cli_abort()] to throw classed errors.
#'
#' @param message (`character`) The message for the new error. Messages will be
#'   formatted with [cli::cli_bullets()].
#' @param subclass (`character`) Class(es) to assign to the error. Will be
#'   prefixed by "shinybatch-error-".
#' @param call (`environment`) The execution environment to mention as the
#'   source of error messages.
#' @param message_env (`environment`) The execution environment to use to
#'   evaluate variables in error messages.
#' @param parent A parent condition, as you might create during a
#'   [rlang::try_fetch()]. See [rlang::abort()] for additional information.
#' @param ... Additional parameters passed to [cli::cli_abort()] and on to
#'   [rlang::abort()].
#'
#' @keywords internal
shinybatch_abort <- function(message,
                             subclass,
                             call = rlang::caller_env(),
                             message_env = call,
                             parent = NULL,
                             ...) {
  cli::cli_abort(
    message,
    class = c(
      "shinybatch-condition",
      "shinybatch-error",
      paste("shinybatch-error", subclass, sep = "-")
    ),
    call = call,
    .envir = message_env,
    parent = parent
  )
}
