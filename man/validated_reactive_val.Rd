% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/validated_reactive_val.R
\name{validated_reactive_val}
\alias{validated_reactive_val}
\title{Create a validated reactive value}
\usage{
validated_reactive_val(
  validation_expr,
  value = NULL,
  label = NULL,
  env = rlang::caller_env()
)
}
\arguments{
\item{validation_expr}{(\code{expression}) A reactive expression that defines the
authoritative state of this \code{validated_reactive_value}, This expression can
access the \code{validated_reactive_value}'s own current value via the \code{.vrv()}
pronoun to reconcile it with upstream dependencies.}

\item{value}{(various) The initial value.}

\item{label}{(length-1 \code{character} or \code{NULL}) An optional label for the
\code{validated_reactive_value}, used for debugging.}

\item{env}{(\code{environment}) The environment in which to evaluate the
\code{validation_expr}.}
}
\value{
A function. Call with no arguments to (reactively) read the
validated value. Call with a single argument to imperatively set the value;
it will be automatically validated on the next read.
}
\description{
Create a reactive value that can be updated imperatively (like a
\code{\link[shiny:reactiveVal]{shiny::reactiveVal()}}) but whose state is ultimately governed by a reactive
validation expression. This ensures that its value is always consistent with
its reactive dependencies before any downstream dependents are re-evaluated.
}
\examples{
\dontshow{if (interactive()) (if (getRversion() >= "3.4") withAutoprint else force)(\{ # examplesIf}

  library(shiny)

  ui <- fluidPage( selectInput("level", "Level", choices = c("A", "B")),
  uiOutput("group_ui"), textOutput("current_group") )

  server <- function(input, output, session) {
    # `group_val` depends on `input$level` and can also be set by `input$group`.
    group_val <- validated_reactive_val(
      value = "A1",
      validation_expr = {
        # If the current value is not valid for the new level, reset it.
        valid_groups <- if (input$level == "A") {
          c("A1", "A2")
        } else {
          c("B1", "B2")
        }
        if (.vrv() \%in\% valid_groups) {
          .vrv()
        } else {
          valid_groups[[1]]
        }
      }
    )

    # When the user changes the group input, imperatively update the vrv.
    observeEvent(input$group, {
      group_val(input$group)
    })

    # The UI for the group dropdown is dynamic.
    output$group_ui <- renderUI({
      choices <- if (input$level == "A") c("A1", "A2") else c("B1", "B2")
      selectInput("group", "Group", choices = choices, selected = group_val())
    })

    output$current_group <- renderText({
      paste("Current Validated Group:", group_val())
    })
  }

  shinyApp(ui, server)
\dontshow{\}) # examplesIf}
}
