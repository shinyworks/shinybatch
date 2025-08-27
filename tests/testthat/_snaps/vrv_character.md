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
      x "123" does not match.

# vrv_character() handles NULL initialization

    Code
      signalCondition(error)
    Condition
      Error:
      ! `.vrv()` must match the regex pattern [34m"^[a-z]+$"[39m
      x [34m"123"[39m does not match.

# vrv_character() handles being set to NULL

    Code
      signalCondition(error)
    Condition
      Error:
      ! `.vrv()` must match the regex pattern [34m"^[a-z]+$"[39m
      x [34m"123"[39m does not match.

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
      x "123" does not match.

# vrv_character_scalar() handles NULL initialization

    Code
      signalCondition(error)
    Condition
      Error:
      ! `.vrv()` must match the regex pattern [34m"^[a-z]+$"[39m
      x [34m"123"[39m does not match.

# vrv_character_scalar() handles being set to NULL

    Code
      signalCondition(error)
    Condition
      Error:
      ! `.vrv()` must match the regex pattern [34m"^[a-z]+$"[39m
      x [34m"123"[39m does not match.

# vrv_character_scalar() handles zero-length character vector

    Code
      signalCondition(error)
    Condition
      Error:
      ! `.vrv()` must match the regex pattern [34m"^[a-z]+$"[39m
      x [34m"123"[39m does not match.

