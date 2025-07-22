test_that(".vrv() successfully finds and executes the pronoun", {
  # Use a test environment so we don't have a conflict between the exported
  # pronoun and the local definition.
  test_env <- rlang::env(
    .vrv = function() "success"
  )
  expect_equal(.vrv(env = test_env), "success")
})

test_that(".vrv() errors if the pronoun is not found", {
  test_env <- rlang::env()
  expect_error(
    {
      .vrv(env = test_env)
    },
    class = "shinybatch-error-pronoun-not-found"
  )
  expect_snapshot(
    {
      .vrv(env = test_env)
    },
    error = TRUE
  )
})
