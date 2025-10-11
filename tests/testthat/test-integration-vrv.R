skip_if_not_installed("shinytest2")
skip_on_covr()
skip_on_cran()
skip_if(is_checking())

test_that("vrv doesn't enter unexpected state (#13)", {
  app <- AppDriver$new(
    app_dir = test_path("apps/cascading_filters/chains")
  )
  app$wait_for_idle()
  app$set_inputs(level = "B")
  app$wait_for_idle()
  logs <- app$get_logs()
  error_msgs <- logs$message[logs$level == "error"]
  expect_length(error_msgs, 0)
  app$stop()
})
