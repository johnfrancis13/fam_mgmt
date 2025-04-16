fitnessUI <- function(id) {
  ns <- NS(id)  # Namespace for unique IDs within the module
  
  sidebarLayout(
    sidebarPanel(
      textInput(ns("name"), "Family Member Name:", placeholder = "E.g., John"),
      selectInput(ns("goal"), "Fitness Goal:", choices = c("Lose Weight", "Gain Muscle", "Improve Stamina", "General Health")),
      textAreaInput(ns("plan"), "Workout Plan:", placeholder = "Describe your workout routine"),
      textInput(ns("milestone"), "Milestone:", placeholder = "E.g., Lose 5kg"),
      dateInput(ns("deadline"), "Deadline:", value = Sys.Date() + 30),  # Default to 30 days later
      actionButton(ns("addEntry"), "Save Plan")
    ),
    mainPanel(
      h3("Fitness Goals & Plans"),
      tableOutput(ns("fitnessTable")),  # Live-updating fitness goals & milestones
      uiOutput(ns("checkboxList")),  # Dynamically render checkboxes
      actionButton(ns("clearSelected"), "Clear Selected Items"),  # Clear selected items
      actionButton(ns("clearEntries"), "Clear All Plans", class = "btn-danger")  # Option to clear the entire list
      
    )
  )
}


fitnessServer <- function(id, db) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns  # Namespace for unique IDs
    
    # Live-update fitness data using `reactivePoll`
    refresh_data <- reactivePoll(
      intervalMillis = 2000,  # Check for updates every 2 seconds
      session = session,
      checkFunc = function() { dbGetQuery(db, "SELECT COUNT(*) FROM fitness_db") },  # Detect table changes
      valueFunc = function() { dbReadTable(db, "fitness_db") }  # Load updated data
    )
    
    # Render the fitness table with milestones & deadlines
    output$fitnessTable <- renderTable({
      refresh_data()  # Table now auto-refreshes!
    })
    
    # Add new fitness plan entry with milestone & deadline
    observeEvent(input$addEntry, {
      dbExecute(db, "INSERT INTO fitness_db (Name, Goal, Plan, Milestone, Deadline) VALUES (?, ?, ?, ?, ?)", 
                params = list(input$name, input$goal, input$plan, input$milestone, format(as.POSIXct(input$deadline))))
      
      updateTextInput(session, "name", value = "")  # Clear name field
      updateTextAreaInput(session, "plan", value = "")  # Clear plan field
      updateTextInput(session, "milestone", value = "")  # Clear milestone field
      updateDateInput(session, "deadline", value = Sys.Date() + 30)  # Reset deadline to 30 days later
    })
    
    # Clear all fitness plans from the database
    observeEvent(input$clearEntries, {
      dbExecute(db, "DELETE FROM fitness_db")
    })
    
    # Dynamically generate checkboxes for each shopping list item
    output$checkboxList <- renderUI({
      fitness_data <- refresh_data()
      if (nrow(fitness_data) > 0) {
        checkboxGroupInput(ns("selectedItems"), "Select Plans to Remove:",
                           choices = fitness_data$Milestone)
      } else {
        h4("No fitness plans.")
      }
    })
    
    # Clear selected items from the shopping list
    observeEvent(input$clearSelected, {
      req(input$selectedItems)  # Ensure at least one item is selected
      
      # Remove selected items from the database
      dbExecute(
        db,
        paste0("DELETE FROM fitness_db WHERE Milestone IN (", 
               paste(rep("?", length(input$selectedItems)), collapse = ","), ")"),
        params = input$selectedItems
      )
      
    })
    
  })
}

