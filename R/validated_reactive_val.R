#' Create a validated reactive value
#'
#' Create a reactive value that can be updated imperatively (like a
#' [shiny::reactiveVal()]) but whose state is ultimately governed by a reactive
#' validation expression. This ensures that its value is always consistent with
#' its reactive dependencies before any downstream dependents are re-evaluated.
#'
#' @inheritParams shared-params
#' @returns A function. Call with no arguments to (reactively) read the
#'   validated value. Call with a single argument to imperatively set the value;
#'   it will be automatically validated on the next read.
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
  label = NULL,
  env = rlang::caller_env()
) {
  state_rv <- .initialize_state_rv(value, label)
  validation_rctv <- .create_validation_reactive(
    {{ validation_expr }},
    state_rv,
    label,
    env
  )
  .create_vrv_function(state_rv, validation_rctv)
}

#' Create and Label the Internal `reactiveVal`
#'
#' Create the internal [shiny::reactiveVal()] that holds the state for a
#' `validated_reactive_val()`.
#'
#' @inheritParams shared-params
#' @returns A [shiny::reactiveVal()].
#' @keywords internal
.initialize_state_rv <- function(value = NULL, label = NULL) {
  state_label <- .paste_if_defined(label, "state", sep = "-")
  reactiveVal(value, label = state_label)
}

#' Create the Core Validation Reactive
#'
#' Create the core [shiny::reactive()] expression that runs the user-provided
#' validation logic, and catch any errors introduce by that logic.
#'
#' @inheritParams shared-params
#' @returns A [shiny::reactive()].
#' @keywords internal
.create_validation_reactive <- function(validation_expr, state_rv, label, env) {
  validation_label <- .paste_if_defined(label, "validation", sep = "-")
  validation_expr_quo <- .enquo_validation_expr(
    {{ validation_expr }},
    state_rv,
    env
  )
  reactive(
    {
      .with_error_handling(
        rlang::eval_tidy(validation_expr_quo),
        message = c(
          "Validation of {.fn shinybatch::validated_reactive_val} failed.",
          x = "Could not evaluate {.arg validation_expr}."
        ),
        subclass = "validation"
      )
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
.create_vrv_function <- function(state_rv, validation_rctv) {
  force(state_rv)
  force(validation_rctv)
  function(value) {
    if (missing(value)) {
      validation_rctv()
    } else {
      # The setter imperatively updates the internal value. The value will be
      # automatically validated on the next read.
      state_rv(value)
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
.enquo_validation_expr <- function(validation_expr, state_rv, env) {
  validation_expr_quo <- rlang::enquo(validation_expr)
  new_env <- rlang::env(
    .vrv = function() state_rv(),
    env
  )
  rlang::quo_set_env(validation_expr_quo, new_env)
}
