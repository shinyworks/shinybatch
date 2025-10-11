# vrv_lgl() sets value to default when invalid (size)

    Code
      signalCondition(error)
    Condition
      Error:
      ! `.vrv()` must have size <= 1.
      x 2 is too big.

# vrv_lgl() handles NULL initialization

    Code
      signalCondition(error)
    Condition
      Error:
      ! `.vrv()` must not be <NULL>.

# vrv_lgl() handles being set to NULL

    Code
      signalCondition(error)
    Condition
      Error:
      ! `.vrv()` must not be <NULL>.

# vrv_lgl_scalar() sets value to default when invalid (size)

    Code
      signalCondition(error)
    Condition
      Error:
      ! `.vrv()` must be a single <logical>.
      x `.vrv()` has 2 values.

# vrv_lgl_scalar() handles NULL initialization

    Code
      signalCondition(error)
    Condition
      Error:
      ! `.vrv()` must not be <NULL>.

# vrv_lgl_scalar() handles zero-length logical vector

    Code
      signalCondition(error)
    Condition
      Error:
      ! `.vrv()` must be a single <logical (non-empty)>.
      x `.vrv()` has no values.

