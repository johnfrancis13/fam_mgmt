library(shiny)
library(dplyr)

# Define UI for Chores Module
choresUI <- function(id) {
  ns <- NS(id) # Namespace for unique IDs
  sidebarLayout(
    sidebarPanel(
      textInput(ns("choresInput"), "Enter a New Chore:", placeholder = "E.g., Wash dishes"),
      selectInput(ns("assignTo"), "Assign to:", choices = c("John", "Jesse", "GB")),
      dateInput(ns("dueDate"), "Due Date:", value = Sys.Date()), # Add due date field
      selectInput(ns("recurrence"), "Recurrence:", choices = c("None", "Daily", "Weekly", "Monthly", "Yearly")),
      actionButton(ns("addChore"), "Add Chore")
    ),
    mainPanel(
      h3("Chores Dashboard"),
      tableOutput(ns("choresTable")), # Display chores list
      h4("Click to Mark as Completed"),
      uiOutput(ns("choresList")) # Dynamically render buttons for marking chores as completed
    )
  )
}

# Define Server Logic for Chores Module
choresServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns # Namespace for handling IDs
    
    # Reactive values to store chores
    chores <- reactiveValues(data = data.frame(
      Chore = character(),
      AssignedTo = character(),
      DueDate = as.Date(character()),
      Recurrence = character(),
      Status = character(),
      stringsAsFactors = FALSE
    ))
    
    # Add a new chore
    observeEvent(input$addChore, {
      newChore <- data.frame(
        Chore = input$choresInput,
        AssignedTo = input$assignTo,
        DueDate = input$dueDate,
        Recurrence = input$recurrence,
        Status = "Pending",
        stringsAsFactors = FALSE
      )
      chores$data <- rbind(chores$data, newChore)
      updateTextInput(session, "choresInput", value = "") # Clear chore input
    })
    
    # Render chores table
    output$choresTable <- renderTable({
      chores$data
    })
    
    # Dynamically generate buttons for marking chores as completed
    output$choresList <- renderUI({
      if (nrow(chores$data) > 0) {
        lapply(1:nrow(chores$data), function(i) {
          if (chores$data$Status[i] == "Pending") {
            div(
              p(strong("Chore:"), chores$data$Chore[i]),
              p("Assigned To:", chores$data$AssignedTo[i]),
              p("Due Date:", chores$data$DueDate[i]),
              actionButton(ns(paste0("complete_", i)), "Mark as Completed"),
              tags$hr()
            )
          }
        }) %>% do.call(tagList, .)
      } else {
        h4("No chores assigned yet!")
      }
    })
    
    # Mark a chore as completed and handle recurrence
    observe({
      lapply(1:nrow(chores$data), function(i) {
        observeEvent(input[[paste0("complete_", i)]], {
          # If the chore is recurring, update its due date and status
          if (chores$data$Recurrence[i] != "None") {
            # Calculate the new due date based on the recurrence frequency
            newDueDate <- switch(
              chores$data$Recurrence[i],
              "Daily" = chores$data$DueDate[i] + 1,
              "Weekly" = chores$data$DueDate[i] + 7,
              "Monthly" = seq(chores$data$DueDate[i], length = 2, by = "1 month")[2],
              "Yearly" = seq(chores$data$DueDate[i], length = 2, by = "1 year")[2]
            )
            # Update the chore's due date and status
            chores$data$DueDate[i] <- newDueDate
            chores$data$Status[i] <- "Pending"
          } else {
            # Mark the chore as completed if it's not recurring
            chores$data$Status[i] <- "Completed"
          }
        })
      })
    })
  })
}