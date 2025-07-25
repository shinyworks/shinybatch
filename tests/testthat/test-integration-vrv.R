test_that("vrv doesn't enter unexpected state (#13)", {
  skip_if_not_installed("shinytest2")
  app <- AppDriver$new(
    app_dir = system.file(
      "apps/cascading_filters/shinybatch",
      package = "shinybatch"
    )
  )
  app$wait_for_idle()
  app$set_inputs(level = "B")
  app$wait_for_idle()
  logs <- app$get_logs()
  error_msgs <- logs$message[logs$level == "error"]
  expect_length(error_msgs, 0)
  app$stop()
})
