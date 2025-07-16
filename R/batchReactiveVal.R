#' BatchReactiveVal R6 class
#'
#' @description An R6 class that encapsulates a standard `reactiveVal`. It
#'   serves as the backend for [batch_reactive_val()] and is managed by a
#'   [BatchOrchestrator] object.
#'
#' @keywords internal
BatchReactiveVal <- R6::R6Class(
  "BatchReactiveVal",
  private = list(
    .name = NULL,
    .rv = NULL,
    .orchestrator = NULL
  ),
  public = list(
    #' @description Initialize the object.
    #' @param name (length-1 `character`) The unique name for this reactive
    #'   value.
    #' @param value (various) The initial value.
    #' @param orchestrator (`BatchOrchestrator` or `batch_reactive_val`) A
    #'   `BatchOrchestrator` object, or a `batch_reactive_val` that should be
    #'   included in the same batch. Defaults to creating a new
    #'   [batch_orchestrator()].
    #' @param validation_fun (`function` or `NULL`) The validation function for
    #'   this value.
    #' @param call (`environment`) The execution environment to use for error
    #'   messages.
    initialize = function(name,
                          value = NULL,
                          orchestrator = batch_orchestrator(),
                          validation_fun = NULL,
                          call = rlang::caller_env()) {
      private$.name <- name
      private$.rv <- reactiveVal(value)
      private$.orchestrator <- choose_orchestrator(orchestrator, call = call)
      private$.orchestrator$add_val(self, validation_fun)
    },

    #' @description Get the current value, blocking if a transaction is active.
    #' @return The current, consistent value stored in the internal `reactiveVal`.
    get = function() {
      private$.orchestrator$wait_for_transaction()
      private$.rv()
    },

    #' @description Get the current value of the underlying [reactiveVal()].
    #' @return The current value stored in the internal `reactiveVal`,
    #'   regardless of whether the value is in a consistent state.
    get_rv_value = function() {
      private$.rv()
    },

    #' @description Set a new value.
    #' @param value (various) The new value to set.
    set = function(value) {
      private$.rv(value)
    },

    #' @description Get the name of the reactive value.
    #' @return (length-1 `character`) The name of the reactive value.
    get_name = function() private$.name,

    #' @description Get the managing [BatchOrchestrator] instance.
    #' @return (R6 `BatchOrchestrator`) The [BatchOrchestrator] object.
    get_orchestrator = function() private$.orchestrator
  )
)

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
  cli::cli_abort(
    "{.arg orchestrator} must be a {.cls BatchOrchestrator} object or another {.cls batch_reactive_val} object.",
    class = "shinybatch-error-bad_orchestrator",
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

#' Create a batch reactive value
#'
#' Creates a "smart" reactive value that coordinates with other reactive values
#' in a batch to ensure atomic, validated updates. This is useful for managing
#' interdependent inputs, like a set of cascading filters, where updating one
#' value should immediately cause others to be re-validated.
#'
#' @param name (length-1 `character`) A unique character string to identify this
#'   reactive value within its batch.
#' @param value (various) The initial value.
#' @param orchestrator (`BatchOrchestrator` or `batch_reactive_val`) A
#'   `BatchOrchestrator` object created by [batch_orchestrator()], or another
#'   `batch_reactive_val` that is already part of the desired batch. Defaults to
#'   creating a new, empty orchestrator.
#' @param validation_fun (`function` or `NULL`) An optional function that
#'   defines the validation logic for this value. It must accept two arguments:
#'   its own current or proposed value, and a named list of all proposed values
#'   in the batch. The function must return the final, validated value based on
#'   the other values (usually either the input value or a default value).
#'
#' @returns A function that behaves like a `reactiveVal` but with transactional
#'   guarantees, with class `batch_reactive_val`.
#' @export
batch_reactive_val <- function(name,
                               value = NULL,
                               orchestrator = batch_orchestrator(),
                               validation_fun = NULL) {
  impl <- BatchReactiveVal$new(
    name = name,
    value = value,
    orchestrator = orchestrator,
    validation_fun = validation_fun,
    call = rlang::current_env()
  )

  the_function <- function(x) {
    if (missing(x)) {
      return(impl$get())
    }
    # If setting a value, request an update from the orchestrator.
    impl$get_orchestrator()$request_update(name, x)
  }
  structure(
    the_function,
    .impl = impl,
    class = c("batch_reactive_val", "reactiveVal", "reactive", "function")
  )
}
