test_that("choose_orchestrator works with a BatchOrchestrator object", {
  orchestrator <- batch_orchestrator()
  expect_identical(choose_orchestrator(orchestrator), orchestrator)
})

test_that("choose_orchestrator works with a batch_reactive_val object", {
  orchestrator <- batch_orchestrator()
  brv <- batch_reactive_val("test", orchestrator = orchestrator)
  expect_identical(choose_orchestrator(brv), orchestrator)
})

test_that("choose_orchestrator errors with other object types", {
  expect_error(
    choose_orchestrator(list()),
    class = "shinybatch-error-bad_orchestrator"
  )
  expect_error(
    choose_orchestrator(123),
    class = "shinybatch-error-bad_orchestrator"
  )
  expect_error(
    choose_orchestrator("a string"),
    class = "shinybatch-error-bad_orchestrator"
  )
})
