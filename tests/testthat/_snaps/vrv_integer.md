# vrv_integer() sets value to default when invalid (size)

    Code
      signalCondition(error)
    Condition
      Error:
      ! `.vrv()` must have size <= 1.
      x 2 is too big.

---

    Code
      signalCondition(error)
    Condition
      Error:
      ! `.vrv()` must have size <= 1.
      x 2 is too big.

# vrv_integer() sets value to default when invalid (min_value)

    Code
      signalCondition(error)
    Condition
      Error:
      ! Values of `.vrv()` must be >= 5.
      x Values are too low at locations 1.

---

    Code
      signalCondition(error)
    Condition
      Error:
      ! Values of `.vrv()` must be >= 5.
      x Values are too low at locations 1.

# vrv_integer() sets value to default when invalid (max_value)

    Code
      signalCondition(error)
    Condition
      Error:
      ! Values of `.vrv()` must be <= 15.
      x Values are too high at locations 1.

---

    Code
      signalCondition(error)
    Condition
      Error:
      ! Values of `.vrv()` must be <= 15.
      x Values are too high at locations 1.

# vrv_integer() handles NULL initialization

    Code
      signalCondition(error)
    Condition
      Error:
      ! `.vrv()` must not be <NULL>.

---

    Code
      signalCondition(error)
    Condition
      Error:
      ! `.vrv()` must not be <NULL>.

# vrv_integer() handles being set to NULL

    Code
      signalCondition(error)
    Condition
      Error:
      ! `.vrv()` must not be <NULL>.

---

    Code
      signalCondition(error)
    Condition
      Error:
      ! `.vrv()` must not be <NULL>.

# vrv_integer_scalar() sets value to default when invalid (size)

    Code
      signalCondition(error)
    Condition
      Error:
      ! `.vrv()` must be a single <integer>.
      x `.vrv()` has 2 values.

---

    Code
      signalCondition(error)
    Condition
      Error:
      ! `.vrv()` must be a single <integer>.
      x `.vrv()` has 2 values.

# vrv_integer_scalar() sets value to default when invalid (max_value)

    Code
      signalCondition(error)
    Condition
      Error:
      ! Values of `.vrv()` must be <= 15.
      x Values are too high at locations 1.

---

    Code
      signalCondition(error)
    Condition
      Error:
      ! Values of `.vrv()` must be <= 15.
      x Values are too high at locations 1.

# vrv_integer_scalar() handles NULL initialization

    Code
      signalCondition(error)
    Condition
      Error:
      ! `.vrv()` must not be <NULL>.

---

    Code
      signalCondition(error)
    Condition
      Error:
      ! `.vrv()` must not be <NULL>.

# vrv_integer_scalar() handles zero-length integer vector

    Code
      signalCondition(error)
    Condition
      Error:
      ! `.vrv()` must be a single <integer (non-empty)>.
      x `.vrv()` has no values.

---

    Code
      signalCondition(error)
    Condition
      Error:
      ! `.vrv()` must be a single <integer (non-empty)>.
      x `.vrv()` has no values.

