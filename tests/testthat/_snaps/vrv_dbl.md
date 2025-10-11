# vrv_dbl() sets value to default when invalid (size)

    Code
      signalCondition(error)
    Condition
      Error:
      ! `.vrv()` must have size <= 1.
      x 2 is too big.

# vrv_dbl() sets value to default when invalid (min_value)

    Code
      signalCondition(error)
    Condition
      Error:
      ! Values of `.vrv()` must be >= 5.
      x Values are too low at locations 1.

# vrv_dbl() sets value to default when invalid (max_value)

    Code
      signalCondition(error)
    Condition
      Error:
      ! Values of `.vrv()` must be <= 15.
      x Values are too high at locations 1.

# vrv_dbl() handles NULL initialization

    Code
      signalCondition(error)
    Condition
      Error:
      ! `.vrv()` must not be <NULL>.

# vrv_dbl() handles being set to NULL

    Code
      signalCondition(error)
    Condition
      Error:
      ! `.vrv()` must not be <NULL>.

# vrv_dbl_scalar() sets value to default when invalid (size)

    Code
      signalCondition(error)
    Condition
      Error:
      ! `.vrv()` must be a single <numeric>.
      x `.vrv()` has 2 values.

# vrv_dbl_scalar() sets value to default when invalid (max_value)

    Code
      signalCondition(error)
    Condition
      Error:
      ! Values of `.vrv()` must be <= 15.
      x Values are too high at locations 1.

# vrv_dbl_scalar() handles NULL initialization

    Code
      signalCondition(error)
    Condition
      Error:
      ! `.vrv()` must not be <NULL>.

# vrv_dbl_scalar() handles zero-length double vector

    Code
      signalCondition(error)
    Condition
      Error:
      ! `.vrv()` must be a single <numeric (non-empty)>.
      x `.vrv()` has no values.

