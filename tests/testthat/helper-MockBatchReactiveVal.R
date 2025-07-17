# This mock class mimics the interface that BatchOrchestrator expects,
# but without any Shiny reactivity, allowing for isolated testing.
MockBatchReactiveVal <- R6::R6Class(
  "MockBatchReactiveVal",
  private = list(
    .name = NULL,
    .value = NULL
  ),
  public = list(
    initialize = function(name, value) {
      private$.name <- name
      private$.value <- value
    },
    get_name = function() private$.name,
    get = function() private$.value,
    get_rv_value = function() private$.value, # Same as get for the mock
    set = function(value) private$.value <- value
  )
)
