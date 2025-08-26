#' Create a validated reactive value
#'
#' Create a reactive value that can be updated imperatively (like a
#' [shiny::reactiveVal()]) but whose state is ultimately governed by a reactive
#' validation expression. This ensures that its value is always consistent with
#' its reactive dependencies before any downstream dependents are re-evaluated.
#'
#' @inheritParams shared-params
#' @param default The value to use when the `validation_expr` throws an error.
#'   Can be a reactive expression. Defaults to `NULL`.
#' @returns A `vrv` object, which is a function with a custom class. Call it
#'   with no arguments to (reactively) read the validated value. Call it with a
#'   single argument to imperatively set the value; it will be automatically
#'   validated on the next read. You can access the most recent validation error
#'   (if any) with `my_vrv$error()` and check if the current value is the default
#'   with `my_vrv$is_default()`.
#' @export
#' @examplesIf interactive()
#'
#'   library(shiny)
#'
#'   ui <- fluidPage( selectInput("level", "Level", choices = c("A", "B")),
#'   uiOutput("group_ui"), textOutput("current_group") )
#'
#'   server <- function(input, output, session) {
#'     # `group_val` depends on `input$level` and can also be set by `input$group`.
#'     group_val <- validated_reactive_val(
#'       value = "A1",
#'       validation_expr = {
#'         # If the current value is not valid for the new level, reset it.
#'         valid_groups <- if (input$level == "A") {
#'           c("A1", "A2")
#'         } else {
#'           c("B1", "B2")
#'         }
#'         if (.vrv() %in% valid_groups) {
#'           .vrv()
#'         } else {
#'           valid_groups[[1]]
#'         }
#'       }
#'     )
#'
#'     # When the user changes the group input, imperatively update the vrv.
#'     observeEvent(input$group, {
#'       group_val(input$group)
#'     })
#'
#'     # The UI for the group dropdown is dynamic.
#'     output$group_ui <- renderUI({
#'       choices <- if (input$level == "A") c("A1", "A2") else c("B1", "B2")
#'       selectInput("group", "Group", choices = choices, selected = group_val())
#'     })
#'
#'     output$current_group <- renderText({
#'       paste("Current Validated Group:", group_val())
#'     })
#'   }
#'
#'   shinyApp(ui, server)
validated_reactive_val <- function(
  validation_expr,
  value = NULL,
  default = NULL,
  label = NULL,
  env = rlang::caller_env()
) {
  rvs <- .initialize_rvs(value, label)
  validation_rctv <- .create_validation_reactive(
    {{ validation_expr }},
    rvs$value,
    label,
    env
  )
  .create_vrv_function(rvs, validation_rctv, rlang::enquo(default))
}

#' @export
#' @rdname validated_reactive_val
`$.vrv` <- function(x, name) {
  env <- rlang::fn_env(x)
  switch(
    name,
    "error" = env$rvs$error,
    "is_default" = env$rvs$is_default,
    NULL
  )
}

#' Create and Label the Internal `reactiveVal`s
#'
#' Create the internal [shiny::reactiveVal()]s that hold the state for a
#' `validated_reactive_val()`.
#'
#' @inheritParams shared-params
#' @returns A list of [shiny::reactiveVal()]s.
#' @keywords internal
.initialize_rvs <- function(value = NULL, label = NULL) {
  list(
    value = reactiveVal(
      value,
      label = .paste_if_defined(label, "value", sep = "-")
    ),
    error = reactiveVal(
      NULL,
      label = .paste_if_defined(label, "error", sep = "-")
    ),
    is_default = reactiveVal(
      FALSE,
      label = .paste_if_defined(label, "is_default", sep = "-")
    )
  )
}

#' Create the Core Validation Reactive
#'
#' Create the core [shiny::reactive()] expression that runs the user-provided
#' validation logic, and catch any errors introduce by that logic.
#'
#' @inheritParams shared-params
#' @returns A [shiny::reactive()].
#' @keywords internal
.create_validation_reactive <- function(validation_expr, value_rv, label, env) {
  validation_label <- .paste_if_defined(label, "validation", sep = "-")
  validation_expr_quo <- .enquo_validation_expr(
    {{ validation_expr }},
    value_rv,
    env
  )
  reactive(
    {
      .catch_vrv_error(rlang::eval_tidy(validation_expr_quo))
    },
    label = validation_label
  )
}

#' Create the User-Facing Getter/Setter Function
#'
#' Create the user-facing function. Without an argument, the resulting function
#' triggers the validation reactive. With an argument, the resulting function
#' imperatively updates the internal state.
#'
#' @inheritParams shared-params
#' @returns A function that acts as a getter and setter.
#' @keywords internal
.create_vrv_function <- function(rvs, validation_rctv, default_quo) {
  force(rvs)
  force(validation_rctv)
  force(default_quo)

  vrv_fun <- function(value) {
    if (missing(value)) {
      # Getter: validate the current value and return the result.
      result <- validation_rctv()
      .handle_validation_result(result, rvs, default_quo)
    } else {
      # Setter: imperatively update the internal value. The value will be
      # automatically validated on the next read.
      rvs$value(value)
    }
  }

  structure(vrv_fun, class = c("vrv", "function"))
}

#' Prepare the validation quosure
#'
#' Capture the user-provided validation expression and set its environment to a
#' new child environment containing the `.vrv` pronoun.
#'
#' @inheritParams shared-params
#'
#' @returns The `validation_expr` enclosed in an environment that is a child of
#'   `env`, with a `.vrv()` pronoun function.
#' @keywords internal
.enquo_validation_expr <- function(validation_expr, value_rv, env) {
  validation_expr_quo <- rlang::enquo(validation_expr)
  new_env <- rlang::env(
    .vrv = function() value_rv(),
    env
  )
  rlang::quo_set_env(validation_expr_quo, new_env)
}

#' Capture a vrv error without re-throwing it
#'
#' @param expr The expression to evaluate.
#' @returns The result of the expression, or the "defused" condition if an
#'   error is thrown.
#' @keywords internal
.catch_vrv_error <- function(expr) {
  rlang::try_fetch(
    expr,
    error = function(cnd) {
      class(cnd) <- paste("captured", class(cnd), sep = "-")
      cnd
    }
  )
}

#' Handle the result of a validation expression
#'
#' Check if the validation result is an error, update the internal state
#' `reactiveVal`s accordingly, and return the final value (either the validated
#' result or the default).
#'
#' @param result The value returned from the validation reactive.
#' @param rvs A list of the internal `reactiveVal`s.
#' @param default_quo The quosure for the default value.
#' @returns The validated value or the default value.
#' @keywords internal
.handle_validation_result <- function(result, rvs, default_quo) {
  if (inherits(result, "captured-error")) {
    return(.handle_validation_error(result, rvs, default_quo))
  }
  return(.handle_validation_success(result, rvs))
}

#' Handle a validation error
#'
#' Sets the internal state `reactiveVal`s to reflect a validation failure.
#'
#' @param result The captured error condition.
#' @param rvs A list of the internal `reactiveVal`s.
#' @param default_quo The quosure for the default value.
#' @returns The evaluated default value.
#' @keywords internal
.handle_validation_error <- function(result, rvs, default_quo) {
  default_val <- rlang::eval_tidy(default_quo)
  rvs$value(default_val)
  rvs$error(result)
  rvs$is_default(TRUE)
  return(default_val)
}

#' Handle a validation success
#'
#' Sets the internal state `reactiveVal`s to reflect a validation success.
#'
#' @param result The validated value.
#' @param rvs A list of the internal `reactiveVal`s.
#' @returns The validated value.
#' @keywords internal
.handle_validation_success <- function(result, rvs) {
  rvs$value(result)
  rvs$error(NULL)
  rvs$is_default(FALSE)
  return(result)
}
