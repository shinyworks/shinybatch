# .with_error_handling throws an error that looks how we expect

    Code
      .with_error_handling(expr = {
        stop("Original error")
      }, message = "New message", subclass = "failure")
    Condition
      Error:
      ! New message
      Caused by error in `withCallingHandlers()`:
      ! Original error

