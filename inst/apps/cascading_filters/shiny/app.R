# shiny App to demonstrate the need for validated_reactive_val()

library(shiny)


all_data <- data.frame(
  level = c("A", "A", "A", "A", "B", "B", "B", "B"),
  group = c("A1", "A1", "A2", "A2", "B1", "B1", "B2", "B2"),
  member = c("A1a", "A1b", "A2a", "A2b", "B1a", "B1b", "B2a", "B2b")
)

ui <- fluidPage(
  titlePanel("Motivating Example"),
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

  # Track the current selection via a reactiveVal, which can also be updated by
  # other modules, etc.
  selected_group <- reactiveVal(NULL)

  # Keep the reactiveVal in sync with the input.
  observe({
    selected_group(input$group)
  })

  # Update the input when the reactiveVal or the valid values change.
  observe({
    groups <- valid_groups()
    current_group <- selected_group()

    selected <- if (isTRUE(current_group %in% groups)) current_group

    updateSelectInput(
      session,
      "group",
      choices = groups,
      selected = selected
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

    # Make the problem obvious with a modal + timer
    showModal(modalDialog(
      title = "Error",
      p("Triggered a slow operation with bad data!"),
      p("This dialog will auto-close after 5 seconds."),
      easyClose = FALSE,
      footer = NULL
    ))
    # Simulate a long-running process
    Sys.sleep(5)
    removeModal()
  })
}

shinyApp(ui, server)
