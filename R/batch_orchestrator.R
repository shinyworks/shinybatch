#' BatchOrchestrator R6 class
#'
#' @description Manages a group of [batch_reactive_val()] objects, orchestrating
#'   validation and ensuring that their values are updated together in a single
#'   atomic transaction.
#'
#' @keywords internal
BatchOrchestrator <- R6::R6Class(
  "BatchOrchestrator",
  private = list(
    .managed_vals = list(),
    .validation_flow = list(),
    .target_state = NULL,
    .is_in_transaction = FALSE
  ),
  public = list(
    #' @description Initialize the transaction manager.
    #' @param validation_flow (named `list` of `function`s) Each function takes
    #'   two arguments: `my_value` (the current or proposed value for the
    #'   reactive it is validating) and `all_values` (a named list of all other
    #'   values in the transaction), and returns a valid value. The order of
    #'   this list defines the order of the validation cascade.
    initialize = function(validation_flow = list()) {
      private$.validation_flow <- validation_flow
    },

    #' @description Add a reactive value to be managed by this coordinator.
    #' @param val (`BatchReactiveVal`) The [BatchReactiveVal] object to add.
    #' @param validation_fun (`function` or `NULL`) The validation function for
    #'   this value.
    add_val = function(val, validation_fun = NULL) {
      name <- val$get_name()
      private$.managed_vals[[name]] <- val
      if (!is.null(validation_fun)) {
        private$.validation_flow[[name]] <- validation_fun
      }
    },

    #' @description Check if a transaction is currently active.
    #' @return (length-1 `logical`) `TRUE` if a transaction is in progress.
    is_in_transaction = function() {
      private$.is_in_transaction
    },

    #' @description Block until the current transaction is complete.
    wait_for_transaction = function() {
      # Ideally this should probably use {later}.
      if (self$is_in_transaction()) {
        target_state <- private$.target_state
        if (length(target_state)) {
          current_values <- isolate(
            lapply(private$.managed_vals, function(v) v$get_rv_value())
          )
          if (identical(current_values, target_state)) {
            private$.is_in_transaction <- FALSE
            return(NULL)
          }
        }
        Sys.sleep(0.01)
        self$wait_for_transaction()
      }
    },

    #' @description The main entry point for starting a state change.
    #' @param trigger_name (length-1 `character`) The name of the reactive value
    #'   that initiated the change.
    #' @param proposed_value (various) The new value proposed for the trigger.
    request_update = function(trigger_name, proposed_value) {
      # Run this *before* we set .is_in_transaction; otherwise we can't get
      # these until we resolve them. But using get() here also protects us from
      # starting a new update before the previous update resolves.
      new_values <- shiny::isolate(
        lapply(private$.managed_vals, function(v) v$get())
      )
      private$.is_in_transaction <- TRUE
      new_values[[trigger_name]] <- proposed_value

      for (name in names(private$.validation_flow)) {
        validation_fun <- private$.validation_flow[[name]]
        validated_value <- validation_fun(new_values[[name]], new_values)
        new_values[[name]] <- validated_value
      }

      private$.target_state <- new_values

      for (name in names(private$.managed_vals)) {
        private$.managed_vals[[name]]$set(new_values[[name]])
      }
    }
  )
)

#' Create a manager for a batch of reactive values
#'
#' Creates an orchestrator to manage a group of related [batch_reactive_val()]s.
#' This is an advanced function; in most cases, an orchestrator will be created
#' automatically by the first call to `batch_reactive_val()` in a batch.
#'
#' @param validation_flow (named `list` of `function`s) A named list that
#'   defines the validation logic for the batch. The name of each element should
#'   correspond to the `name` of a [batch_reactive_val()] which will be included
#'   in the batch. Each function must accept two arguments: its own proposed
#'   value, and a named list of all proposed values in the batch. The function
#'   should return the final, validated value. The order of this list is
#'   important, as it determines the execution order of the validation cascade.
#'   By default, the orchestrator is initialized with an empty list, and
#'   validation functions are added as values are added to this batch.
#'
#' @returns An object of class `BatchOrchestrator` to be passed to the
#'   `orchestrator` argument of [batch_reactive_val()].
#' @export
batch_orchestrator <- function(validation_flow = list()) {
  BatchOrchestrator$new(validation_flow)
}
