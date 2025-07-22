
<!-- README.md is generated from README.Rmd. Please edit that file -->

# shinybatch

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/shinybatch)](https://CRAN.R-project.org/package=shinybatch)
[![Codecov test
coverage](https://codecov.io/gh/shinyworks/shinybatch/graph/badge.svg)](https://app.codecov.io/gh/shinyworks/shinybatch)
[![R-CMD-check](https://github.com/shinyworks/shinybatch/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/shinyworks/shinybatch/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

{shinybatch} provides `validated_reactive_val()`, a hybrid reactive
object that acts like a `shiny::reactiveVal()` but with a validation
expression like a `shiny::reactive()`. This allows for the creation of
self-validating reactive values, useful for managing complex,
interdependent state in ‘shiny’ applications.

## Motivation

In a standard Shiny app, you might have two inputs that depend on each
other, like a “category” and “sub-category” selector. When the user
changes the category, you need an `observeEvent()` to update the
sub-category’s value to something valid. `validated_reactive_val()`
simplifies this pattern by building the validation logic directly into
the reactive object itself. We also ensure that the changes happen in
the correct order, and prevent observers from “seeing” the inconsistent
state.

``` r
# `group_val` depends on `input$level`.
group_val <- validated_reactive_val(
  value = "A1",
  validation_expr = {
    # If the current value is not valid for the new level, reset it.
    valid_groups <- if (input$level == "A") {
      c("A1", "A2")
    } else {
      c("B1", "B2")
    }
    if (.vrv() %in% valid_groups) {
      .vrv()
    } else {
      valid_groups[[1]]
    }
  }
)
```

Now, any time `input$level` changes, `group_val()` will automatically
re-validate its own state. When you read `group_val()`, you are
guaranteed to get a value that is consistent with its dependencies.

## Installation

You can install the development version of shinybatch from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("shinyworks/shinybatch")
```

## Code of Conduct

Please note that the shinybatch project is released with a [Contributor
Code of
Conduct](https://shinyworks.github.io/shinybatch/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
