#' Create a validated reactive value
#'
#' Create a reactive value that can be updated imperatively (like a
#' [shiny::reactiveVal()]) but whose state is ultimately governed by a reactive
#' validation expression. This ensures that its value is always consistent with
#' its reactive dependencies before any downstream dependents are re-evaluated.
#'
#' @inheritParams shared-params
#' @returns A `vrv` object, which is a function with a custom class. Call it
#'   with no arguments to (reactively) read the validated value. Call it with a
#'   single argument to imperatively set the value; it will be automatically
#'   validated on the next read. You can also access the most recent validation
#'   error with `my_vrv$error()` and check if the current value is the default
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
  value_rv <- .initialize_value_rv(value, label)
  validation_rctv <- .create_validation_reactive(
    {{ validation_expr }},
    value_rv,
    rlang::enquo(default),
    label,
    env
  )
  vrv_fun <- .create_vrv_function(value_rv, validation_rctv)
  structure(vrv_fun, class = c("vrv", "function"))
}

#' @export
#' @param x (`vrv`) A `vrv` object.
#' @param name (length-1 `character`) The name of the helper function to access.
#'   Expects one of `value`, `error`, or `is_default`. Other values return
#'   `NULL`.
#' @rdname validated_reactive_val
`$.vrv` <- function(x, name) {
  force(name)
  if (!name %in% c("value", "error", "is_default")) {
    return(NULL)
  }
  function() {
    x(get = name)
  }
}

#' Create and Label the Internal `reactiveVal`
#'
#' Create the internal [shiny::reactiveVal()] that holds the state for a
#' `validated_reactive_val()`.
#'
#' @inheritParams shared-params
#' @returns A [shiny::reactiveVal()].
#' @keywords internal
.initialize_value_rv <- function(value = NULL, label = NULL) {
  reactiveVal(
    value,
    label = .paste_if_defined(label, "value", sep = "-")
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
.create_validation_reactive <- function(
  validation_expr,
  value_rv,
  default_quo,
  label,
  env
) {
  validation_label <- .paste_if_defined(label, "validation", sep = "-")
  validation_expr_quo <- .enquo_validation_expr(
    {{ validation_expr }},
    value_rv,
    env
  )
  reactive(
    {
      result <- .catch_vrv_error(rlang::eval_tidy(validation_expr_quo))
      .handle_validation_result(result, default_quo)
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
.create_vrv_function <- function(value_rv, validation_rctv) {
  force(value_rv)
  force(validation_rctv)

  function(value, get = "value") {
    if (missing(value)) {
      # Getter: return the requested piece of the validation result.
      result <- validation_rctv()
      result[[get]]
    } else {
      # Setter: imperatively update the internal value. The value will be
      # automatically validated on the next read.
      value_rv(value)
    }
  }
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
#' Check if the validation result is an error, and return a list containing the
#' final value and its status.
#'
#' @param result The value returned from the validation reactive.
#' @param default_quo The quosure for the default value.
#' @returns A list with elements `value`, `error`, and `is_default`.
#' @keywords internal
.handle_validation_result <- function(result, default_quo) {
  if (inherits(result, "captured-error")) {
    return(.handle_validation_error(result, default_quo))
  }
  return(.handle_validation_success(result))
}

#' Handle a validation error
#'
#' Constructs the result list for a validation failure.
#'
#' @param result The captured error condition.
#' @param default_quo The quosure for the default value.
#' @returns A list with elements `value`, `error`, and `is_default`.
#' @keywords internal
.handle_validation_error <- function(result, default_quo) {
  list(
    value = rlang::eval_tidy(default_quo),
    error = result,
    is_default = TRUE
  )
}

#' Handle a validation success
#'
#' Constructs the result list for a validation success.
#'
#' @param result The validated value.
#' @returns A list with elements `value`, `error`, and `is_default`.
#' @keywords internal
.handle_validation_success <- function(result) {
  list(
    value = result,
    error = NULL,
    is_default = FALSE
  )
}
