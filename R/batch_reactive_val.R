# BatchReactiveVal R6 class ----

#' BatchReactiveVal R6 class
#'
#' @description An R6 class that encapsulates a standard `reactiveVal`. It
#'   serves as the backend for [batch_reactive_val()] and is managed by a
#'   [BatchOrchestrator] object.
#'
#' @keywords internal
BatchReactiveVal <- R6::R6Class(
  "BatchReactiveVal",
  ## Private ----
  private = list(
    .name = NULL,
    .rv = NULL,
    .orchestrator = NULL
  ),
  ## Public ----
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
    #' @param call (`environment`) The execution environment to use for error
    #'   messages.
    #' @return The current, consistent value stored in the internal
    #'   `reactiveVal`.
    get = function(call = rlang::caller_env()) {
      private$.orchestrator$wait_for_transaction(call = call)
      private$.rv()
    },

    #' @description Get the raw value of the internal reactiveVal.
    #' @details This is an unsafe getter intended for internal use by the
    #'   orchestrator only. It does not wait for transactions to complete.
    #' @return The current value stored in the internal `reactiveVal`.
    get_rv_value = function() {
      private$.rv()
    },

    #' @description Request a new value for this reactive.
    #' @details This method initiates a transaction with the orchestrator.
    #' @param value (various) The new value to set.
    #' @param call (`environment`) The execution environment to use for error
    #'   messages.
    set = function(value, call = rlang::caller_env()) {
      private$.orchestrator$request_update(
        private$.name,
        value,
        call = call
      )
    },

    #' @description Set the raw value of the internal reactiveVal.
    #' @details This is an unsafe setter intended for internal use by the
    #'   orchestrator only. It does not trigger a validation cascade.
    #' @param value (various) The new value to set.
    set_rv_value = function(value) {
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

# batch_reactive_val ----

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
    impl$set(x, call = rlang::current_env())
  }
  structure(
    the_function,
    .impl = impl,
    class = c("batch_reactive_val", "reactiveVal", "reactive", "function")
  )
}
