test_that("BatchOrchestrator initializes", {
  orchestrator <- batch_orchestrator()
  expect_s3_class(orchestrator, "BatchOrchestrator")
})

test_that("Orchestrator handles invalidated first value", {
  orchestrator <- batch_orchestrator()
  val_a <- MockBatchReactiveVal$new("a", 10)
  val_b <- MockBatchReactiveVal$new("b", 20)
  orchestrator$add_val(
    val_a,
    validation_fun = function(my_val, all_vals) {
      if (my_val > 0) my_val else 1
    }
  )
  orchestrator$add_val(
    val_b,
    validation_fun = function(my_val, all_vals) {
      all_vals$a + 1
    }
  )
  orchestrator$request_update("a", -5)
  # This doesn't actually do anything important here, but I'm running it since
  # it would run in normal usage.
  orchestrator$wait_for_transaction()
  expect_equal(val_a$get(), 1)
  expect_equal(val_b$get(), 2)
})

test_that("Orchestrator handles invalidated second value", {
  orchestrator <- batch_orchestrator()
  val_a <- MockBatchReactiveVal$new("a", 10)
  val_b <- MockBatchReactiveVal$new("b", 20)
  orchestrator$add_val(
    val_a,
    validation_fun = function(my_val, all_vals) {
      if (my_val > 0) my_val else 1
    }
  )
  orchestrator$add_val(
    val_b,
    validation_fun = function(my_val, all_vals) {
      if (my_val > all_vals$a) my_val else all_vals$a + 1
    }
  )
  orchestrator$request_update("b", 5)
  expect_equal(val_a$get(), 10)
  expect_equal(val_b$get(), 11)
})

test_that("Orchestrator errors if validation depends on unmanaged value", {
  orchestrator <- batch_orchestrator()
  val_b <- MockBatchReactiveVal$new("b", 20)
  orchestrator$add_val(
    val_b,
    validation_fun = function(my_val, all_vals) {
      # This will fail because `all_vals$a` is NULL.
      if (my_val > all_vals$a) my_val else all_vals$a + 1
    }
  )
  expect_error(
    orchestrator$request_update("b", 5),
    "argument is of length zero"
  )
})

test_that("Orchestrator created with validation_flow handles invalid first value", {
  validation_logic <- list(
    a = function(my_val, all_vals) if (my_val > 0) my_val else 1,
    b = function(my_val, all_vals) all_vals$a + 1
  )
  orchestrator <- batch_orchestrator(validation_flow = validation_logic)
  val_a <- MockBatchReactiveVal$new("a", 10)
  val_b <- MockBatchReactiveVal$new("b", 20)
  orchestrator$add_val(val_a)
  orchestrator$add_val(val_b)

  orchestrator$request_update("a", -5)
  expect_equal(val_a$get(), 1)
  expect_equal(val_b$get(), 2)
})

test_that("Orchestrator created with larger-than-needed validation_flow handles update", {
  validation_logic <- list(
    a = function(my_val, all_vals) if (my_val > 0) my_val else 1,
    b = function(my_val, all_vals) all_vals$a + 1
  )
  orchestrator <- batch_orchestrator(validation_flow = validation_logic)
  val_a <- MockBatchReactiveVal$new("a", 10)
  orchestrator$add_val(val_a)
  orchestrator$request_update("a", -5)
  orchestrator$wait_for_transaction()
  expect_equal(val_a$get(), 1)
})

test_that("Orchestrator errors informatively if wait exceeds timeout.", {
  orchestrator <- batch_orchestrator(timeout = 0.01)
  val_a <- MockBatchReactiveVal$new("a", 1)
  # "Break" val_a so it never resolves.
  rlang::env_binding_unlock(val_a, "get_rv_value")
  val_a$get_rv_value <- function() 1000

  orchestrator$add_val(val_a, validation_fun = function(my_val, all_vals) my_val)
  orchestrator$request_update("a", 2)
  expect_error(
    {
      orchestrator$wait_for_transaction()
    },
    "Transaction must resolve within 0.01 seconds.",
    class = "shinybatch-error-timeout"
  )
})

test_that("Orchestrator catches and re-throws validation errors", {
  orchestrator <- batch_orchestrator()
  val_a <- MockBatchReactiveVal$new("a", 1)
  orchestrator$add_val(
    val_a,
    validation_fun = function(my_val, all_vals) {
      stop("Do not update")
    }
  )
  val_a_updater <- function(x) {
    orchestrator$request_update("a", x)
  }
  expect_error(
    {
      val_a_updater(2)
    },
    class = "shinybatch-error-validation"
  )
  expect_false(orchestrator$is_in_transaction())
})
