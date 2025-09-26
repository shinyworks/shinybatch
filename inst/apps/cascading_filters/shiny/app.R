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

  # When the level changes, the selected group may no longer be valid.
  selected_group <- reactive({
    if (isTRUE(input$group %in% valid_groups())) {
      input$group
    } # Returns NULL if invalid
  })

  # Update the input when the reactiveVal or the valid values change.
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
    req(input$level, input$group)

    filtered_data <- all_data[
      all_data$level == input$level & all_data$group == input$group,
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
