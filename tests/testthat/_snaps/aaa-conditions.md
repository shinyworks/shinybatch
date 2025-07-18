# shinybatch_abort message formatting is correct

    Code
      shinybatch_abort("This is a test message.", subclass = "snapshot")
    Condition
      Error:
      ! This is a test message.

---

    Code
      bad_variable <- "bad"
      shinybatch_abort(c("This is a test message with a variable: {bad_variable}", i = "This is some additional info."),
      subclass = "snapshot_complex")
    Condition
      Error:
      ! This is a test message with a variable: bad
      i This is some additional info.

# shinybatch_abort messages mention the parent function

    Code
      wrapper()
    Condition
      Error in `wrapper()`:
      ! This is a test message.

# with_error_handling throws an error that looks how we expect

    Code
      with_error_handling(expr = {
        stop("Original error")
      }, message = "New message", subclass = "failure")
    Condition
      Error:
      ! New message
      Caused by error in `withCallingHandlers()`:
      ! Original error

