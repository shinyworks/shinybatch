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
    .is_in_transaction = FALSE,
    .timeout = 5L,

    # Check if the current state matches the target state.
    .is_resolved = function() {
      if (!length(private$.target_state)) {
        # covr doesn't see us hit this but of course we do, or we'd time out.
        return(TRUE) # nocov
      }

      current_values <- isolate(
        lapply(private$.managed_vals, function(v) v$get_rv_value())
      )

      return(identical(current_values, private$.target_state))
    },

    # Check if the transaction has exceeded its timeout.
    .check_timeout = function(start_time, timeout, call) {
      wait <- difftime(Sys.time(), start_time, units = "secs")
      if (wait > timeout) {
        shinybatch_abort(
          c(
            "Transaction must resolve within {timeout} seconds.",
            i = "Check your validation functions for infinite loops or long-running operations."
          ),
          subclass = "timeout",
          call = call,
          message_env = rlang::current_env()
        )
      }
    },

    # Block until the transaction is resolved, then clean up.
    .resolve_transaction = function(timeout, call) {
      start_time <- Sys.time()
      while (!private$.is_resolved()) {
        private$.check_timeout(start_time, timeout, call)
        Sys.sleep(0.01)
      }
      private$.is_in_transaction <- FALSE
      private$.target_state <- NULL
    },

    # Apply the validation rules to a set of proposed values.
    .apply_validation_rules = function(new_values, managed_flows) {
      for (name in managed_flows) {
        validation_fun <- private$.validation_flow[[name]]
        validated_value <- validation_fun(new_values[[name]], new_values)
        new_values[[name]] <- validated_value
      }
      new_values
    },

    # Run the validation cascade over a set of proposed values.
    .cascade_validation = function(state, call) {
      managed_flows <- intersect(
        names(private$.validation_flow),
        names(state)
      )
      private$.try_validation(state, managed_flows, call)
    },

    # Try to apply validation rules, catching errors and rolling back the
    # transaction on failure
    .try_validation = function(new_values, managed_flows, call) {
      with_error_handling(
        private$.apply_validation_rules(new_values, managed_flows),
        before_error = {
          private$.is_in_transaction <- FALSE
          private$.target_state <- NULL
        },
        message = "Validation failed during batch update.",
        subclass = "validation",
        call = call
      )
    },

    # Apply a new state to the managed reactive values.
    .apply_state = function(state) {
      private$.target_state <- state
      for (name in names(private$.managed_vals)) {
        private$.managed_vals[[name]]$set(state[[name]])
      }
    },

    # Get current state and set transaction flag.
    .start_transaction = function(trigger_name, proposed_value) {
      # We must request the values *before* setting `.is_in_transaction`, or we
      # get stuck. But use the `get()` methods rather than a workaround, because
      # that way we're forced to wait for any previous changes.
      new_values <- shiny::isolate(
        lapply(private$.managed_vals, function(v) v$get())
      )
      private$.is_in_transaction <- TRUE
      new_values[[trigger_name]] <- proposed_value
      new_values
    }
  ),
  public = list(
    #' @description Initialize the transaction manager.
    #' @param validation_flow (named `list` of `function`s) Each function takes
    #'   two arguments: `my_value` (the current or proposed value for the
    #'   reactive it is validating) and `all_values` (a named list of all other
    #'   values in the transaction), and returns a valid value. The order of
    #'   this list defines the order of the validation cascade.
    #' @param timeout (length-1 `integer`) The maximum number of seconds to wait
    #'   for a transaction to complete.
    initialize = function(validation_flow = list(), timeout = 5L) {
      private$.validation_flow <- validation_flow
      private$.timeout <- timeout
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

    #' @description Get this orchestrator's timeout setting.
    #' @return (length-1 `integer`) The maximum number of seconds to wait for a
    #'   transaction to complete.
    get_timeout = function() {
      private$.timeout %||% 5L
    },

    #' @description Block until the current transaction is complete.
    #' @param timeout (length-1 `integer`) The maximum number of seconds to wait
    #'   for a transaction to complete.
    #' @param call (`environment`) The execution environment to mention as the
    #'   source of error messages.
    wait_for_transaction = function(timeout = self$get_timeout(),
                                    call = rlang::caller_env()) {
      if (!self$is_in_transaction()) {
        # covr doesn't see us hit this but of course we do, or we'd time out.
        return(invisible(NULL)) # nocov
      }
      private$.resolve_transaction(timeout, call)
      return(invisible(NULL))
    },

    #' @description Attempt to update managed values in a transaction.
    #' @param trigger_name (length-1 `character`) The name of the reactive value
    #'   that initiated the change.
    #' @param proposed_value (various) The new value proposed for the trigger.
    #' @param call (`environment`) The execution environment to mention as the
    #'   source of error messages.
    request_update = function(trigger_name,
                              proposed_value,
                              call = rlang::caller_env()) {
      state <- private$.start_transaction(trigger_name, proposed_value)
      state <- private$.cascade_validation(state, call)
      private$.apply_state(state)
    }
  )
)

#' Create a manager for a batch of reactive values
#'
#' Creates an orchestrator to manage a group of related [batch_reactive_val()]s.
#' This is an advanced function; in most cases, an orchestrator will be created
#' automatically by the first call to [batch_reactive_val()] in a batch.
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
#' @param timeout (length-1 `integer`) The maximum number of seconds to wait
#'   for a transaction to complete.
#'
#' @returns An object of class `BatchOrchestrator` to be passed to the
#'   `orchestrator` argument of [batch_reactive_val()].
#' @export
batch_orchestrator <- function(validation_flow = list(), timeout = 5L) {
  BatchOrchestrator$new(validation_flow, timeout)
}
