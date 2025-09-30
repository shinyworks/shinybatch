# shiny App to demonstrate vrv_factor_scalar()

library(shiny)
pkgload::load_all(export_all = FALSE)

all_data <- data.frame(
  level = c("A", "A", "A", "A", "B", "B", "B", "B"),
  group = c("A1", "A1", "A2", "A2", "B1", "B1", "B2", "B2"),
  member = c("A1a", "A1b", "A2a", "A2b", "B1a", "B1b", "B2a", "B2b")
)

ui <- fluidPage(
  titlePanel("With vrv_factor_scalar()"),
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

  # This is the core change: vrv_factor_scalar() ensures that its value is
  # always one of the valid_groups.
  selected_group <- vrv_factor_scalar(
    levels = valid_groups(),
    value = reactive(input$group)
  )

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

    # This block is now unreachable, since we returned filtered_data and we will
    # never execute this render with an invalid selected group.
    showModal(modalDialog(
      title = "Error",
      p("This modal will not appear!"),
      easyClose = FALSE,
      footer = NULL
    ))
    # Simulate a long-running process
    Sys.sleep(5)
    removeModal()
  })
}

shinyApp(ui, server)
