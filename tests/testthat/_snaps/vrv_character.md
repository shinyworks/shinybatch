# vrv_character() sets value to default when invalid (size)

    Code
      signalCondition(error)
    Condition
      Error:
      ! `.vrv()` must have size <= 1.
      x 2 is too big.

# vrv_character() sets value to default when invalid (regex)

    Code
      signalCondition(error)
    Condition
      Error:
      ! `.vrv()` must match the regex pattern "^[a-z]+$"
      x "123" fails the check.

# vrv_character() handles NULL initialization

    Code
      signalCondition(error)
    Condition
      Error:
      ! `.vrv()` must not be <NULL>.

# vrv_character() handles being set to NULL

    Code
      signalCondition(error)
    Condition
      Error:
      ! `.vrv()` must not be <NULL>.

# vrv_character_scalar() sets value to default when invalid (size)

    Code
      signalCondition(error)
    Condition
      Error:
      ! `.vrv()` must be a single <character>.
      x `.vrv()` has 2 values.

# vrv_character_scalar() sets value to default when invalid (regex)

    Code
      signalCondition(error)
    Condition
      Error:
      ! `.vrv()` must match the regex pattern "^[a-z]+$"
      x "123" fails the check.

# vrv_character_scalar() handles NULL initialization

    Code
      signalCondition(error)
    Condition
      Error:
      ! `.vrv()` must not be <NULL>.

# vrv_character_scalar() handles zero-length character vector

    Code
      signalCondition(error)
    Condition
      Error:
      ! `.vrv()` must be a single <character (non-empty)>.
      x `.vrv()` has no values.

