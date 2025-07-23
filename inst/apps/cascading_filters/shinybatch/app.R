# shiny App to demonstrate the effect of validated_reactive_val()

library(shiny)
library(shinybatch)

all_data <- data.frame(
  level = c("A", "A", "A", "A", "B", "B", "B", "B"),
  group = c("A1", "A1", "A2", "A2", "B1", "B1", "B2", "B2"),
  member = c("A1a", "A1b", "A2a", "A2b", "B1a", "B1b", "B2a", "B2b")
)

ui <- fluidPage(
  titlePanel("With validated_reactive_val()"),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        "level",
        "Level (controls Group)",
        choices = unique(all_data$level)
      ),
      selectInput(
        "group",
        "Group",
        choices = NULL
      )
    ),
    mainPanel(
      h3("Members"),
      verbatimTextOutput("members_out")
    )
  )
)

server <- function(input, output, session) {
  # Keep the list of possible group values synced with the level.
  valid_groups <- reactive({
    unique(all_data$group[all_data$level == input$level])
  })

  # This is the core change: validated_reactive_val() ensures that its value
  # is always valid. The validation_expr uses the .vrv() pronoun to refer
  # to selected_group's current value (before any update).
  selected_group <- shinybatch::validated_reactive_val({
    groups <- valid_groups()
    current_group <- .vrv()
    if (isTRUE(current_group %in% groups)) current_group
  })

  # Keep the validated_reactive_val in sync with the input.
  observe({
    selected_group(input$group)
  })

  # Update the input when the validated_reactive_val or the valid values change.
  observe({
    updateSelectInput(
      session,
      "group",
      choices = valid_groups(),
      selected = selected_group()
    )
  })

  # Display the members of the selected group. We pause the operation when it
  # encounters an invalid state to emphasize the potential issue.
  output$members_out <- renderPrint({
    req(input$level, selected_group())

    filtered_data <- all_data[
      all_data$level == input$level & all_data$group == selected_group(),
    ]
    if (nrow(filtered_data)) {
      return(filtered_data)
    }

    # This block should now be unreachable, as req(selected_group()) will
    # prevent execution when the inputs are inconsistent.
    showModal(modalDialog(
      title = "Error",
      p("This modal should not appear!"),
      easyClose = FALSE,
      footer = NULL
    ))
    # Simulate a long-running process
    Sys.sleep(5)
    removeModal()
  })
}

shinyApp(ui, server)
