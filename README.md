

<!-- README.md is generated from README.qmd. Please edit that file -->

# chains

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/chains.png)](https://CRAN.R-project.org/package=chains)
[![R-universe
version](https://shinyworks.r-universe.dev/chains/badges/version.png)](https://shinyworks.r-universe.dev/chains)
[![R-CMD-check](https://github.com/shinyworks/chains/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/shinyworks/chains/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/shinyworks/chains/graph/badge.svg)](https://app.codecov.io/gh/shinyworks/chains)
<!-- badges: end -->

{chains} provides `validated_reactive_val()`, a hybrid reactive object
that acts like a `shiny::reactiveVal()` but with a validation expression
like a `shiny::reactive()`. This allows for the creation of
self-validating reactive values, useful for managing complex,
interdependent state in ‘shiny’ applications.

## Motivation

In a standard Shiny app, you might have two inputs that depend on each
other, like a “category” and “sub-category” selector. When the user
changes the category, you need an `observeEvent()` to update the
sub-category’s value to something valid. `validated_reactive_val()` and
its wrappers like `vrv_factor_scalar()` simplify this pattern by
building the validation logic directly into the reactive object itself.
We also ensure that the changes happen in the correct order, and prevent
observers from “seeing” the inconsistent state.

``` r
# The allowed groups depend on the selected "level".
allowed_groups <- shiny::reactive({
  if (shiny::req(input$level) == "A") {
    c("A1", "A2")
  } else {
    c("B1", "B2")
  }
})

# The group_val is validated to make sure it is always one of the allowed groups,
# returning "bad group" if not. It is also synchronized with the input$group.
group_val <- vrv_factor_scalar(
  levels = allowed_groups(),
  value = shiny::reactive(input$group),
  default = "bad group"
)
```

Now, any time `input$level` changes, `group_val()` will automatically
re-validate its own state. When you read `group_val()`, you are
guaranteed to get a value that is consistent with its dependencies.
Also, if you set `group_val` to a bad value (such as `group_val("C1")`),
it will return `"bad group"` anywhere that it is used.

## Installation

You can install the development version of chains from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("shinyworks/chains")
```

## Code of Conduct

Please note that the chains project is released with a [Contributor Code
of Conduct](https://shinyworks.github.io/chains/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
