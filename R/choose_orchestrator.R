#' Choose an orchestrator
#'
#' @param orchestrator (`BatchOrchestrator` or `batch_reactive_val`) A
#'   `BatchOrchestrator` object, or a `batch_reactive_val` that should be
#'   included in the same batch.
#' @param call (`environment`) The execution environment to use for error
#'   messages.
#'
#' @returns A `BatchOrchestrator` object.
#' @keywords internal
choose_orchestrator <- function(orchestrator, call = rlang::caller_env()) {
  UseMethod("choose_orchestrator")
}

#' @export
choose_orchestrator.default <- function(orchestrator, call = rlang::caller_env()) {
  shinybatch_abort(
    "{.arg orchestrator} must be a {.cls BatchOrchestrator} object or another {.cls batch_reactive_val} object.",
    subclass = "bad_orchestrator",
    call = call
  )
}

#' @export
choose_orchestrator.BatchOrchestrator <- function(orchestrator, call = rlang::caller_env()) {
  orchestrator
}

#' @export
choose_orchestrator.batch_reactive_val <- function(orchestrator, call = rlang::caller_env()) {
  attr(orchestrator, ".impl", exact = TRUE)$get_orchestrator()
}
