library(shiny)
library(dplyr)
library(lubridate)

# Define UI for Chores Module
choresUI <- function(id) {
  ns <- NS(id) # Namespace for unique IDs
  sidebarLayout(
    sidebarPanel(
      h4("Add a new chore"),
      textInput(ns("choresInput"), "Enter a New Chore:", placeholder = "E.g., Wash dishes"),
      selectInput(ns("assignTo"), "Assign to:", choices = c("John", "Jesse", "GB")),
      dateInput(ns("dueDate"), "Due Date:", value = Sys.Date()), # Add due date field
      selectInput(ns("recurrence"), "Recurrence:", choices = c("None", "Daily", "Weekly", "Monthly", "Yearly")),
      actionButton(ns("addChore"), "Add Chore"),
      actionButton(ns("clearfinishedChores"), "Clear Completed Chores"),
      actionButton(ns("clearChores"), "Clear All Chores"),
      
      # New section: Dropdown and Mark as Completed button
      h4("Complete a chore"),
      selectInput(ns("choreDropdown"), "Select a Chore to Complete:", choices = NULL), # Dynamically populated
      actionButton(ns("markDropdownComplete"), "Mark as Completed") # Button to complete chore
    ),
    mainPanel(
      h3("Chores Dashboard"),
      tableOutput(ns("choresTable")), # Display chores list
    )
  )
}


# Define Server Logic for Chores Module
choresServer <- function(id, db) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns # Namespace for handling IDs

    # Live-update fitness data using `reactivePoll`
    refresh_data <- reactivePoll(
      intervalMillis = 2000,  # Check for updates every 2 seconds
      session = session,
      checkFunc = function() {dbGetQuery(db, "SELECT COUNT(*), MAX(LastUpdated) FROM chores_db")},  # Detect table changes
      valueFunc = function() { dbReadTable(db, "chores_db") }  # Load updated data
    )

    # Populate dropdown with Pending chores
    observe({
      chores_data <- refresh_data()
      pending_chores <- chores_data %>%
        filter(Status == "Pending")
      
      updateSelectInput(session, "choreDropdown", choices = pending_chores$Chore)
    })
    
    # Handle Mark as Completed button click
    observeEvent(input$markDropdownComplete, {
      req(input$choreDropdown) # Ensure a chore is selected
      
      chores_data <- refresh_data() %>% filter(Chore %in% input$choreDropdown)
      chores_data$DueDate <- ymd(chores_data$DueDate) # ensure values are dates in R
      
      if (chores_data$Recurrence[1] != "None") {
        # Calculate the new due date
        newDueDate <- case_when(
          chores_data$Recurrence[1] == "Daily" ~ chores_data$DueDate[1] + 1,
          chores_data$Recurrence[1] == "Weekly" ~ chores_data$DueDate[1] + 7,
          chores_data$Recurrence[1] == "Monthly" ~ as.Date(chores_data$DueDate[1]) %m+% months(1),
          chores_data$Recurrence[1] == "Yearly" ~ as.Date(chores_data$DueDate[1]) %m+% years(1)
        )
        
        # Update the database with the new due date and status
        dbExecute(
          db,
          "UPDATE chores_db SET DueDate = ?, LastUpdated = ? WHERE Chore = ?",
          params = list(format(newDueDate, "%Y-%m-%d"), format(as.POSIXct(Sys.time())), chores_data$Chore[1])
        )
      } else {
        # Delete chore if not recurring
        dbExecute(
          db,
          "UPDATE chores_db SET Status = ?, LastUpdated = ? WHERE Chore = ?",
          params = list("Completed", format(as.POSIXct(Sys.time())), chores_data$Chore[1])
        )
      }
      
    })
    
    # Add new chore
    observeEvent(input$addChore, {
      dbExecute(db, "INSERT INTO chores_db (Chore, AssignedTo, DueDate, Recurrence, Status, LastUpdated) VALUES (?, ?, ?, ?, ?,?)",
                params = list(input$choresInput, input$assignTo, format(as.POSIXct(input$dueDate), "%Y-%m-%d"), input$recurrence, "Pending",format(as.POSIXct(Sys.time()))))
      updateTextInput(session, "choresInput", value = "") # Clear chore input
      refresh_data()
    })

    # Clear all completed chores
    observeEvent(input$clearfinishedChores, {
      dbExecute(db, "DELETE FROM chores_db WHERE Status = ?", params = list("Completed"))
    })

    # Clear all chores
    observeEvent(input$clearChores, {
      dbExecute(db, "DELETE FROM chores_db")
    })

    # Render chores table
    output$choresTable <- renderTable({
      refresh_data() %>% select(-LastUpdated)  # Table now auto-refreshes!
    })


  })
}