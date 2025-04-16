mealPlanUI <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      #textInput(ns("mealName"), "Meal Name:", placeholder = "E.g., Spaghetti Bolognese"),
      # NEW: dynamic select input + conditional "other" input
      selectInput(ns("mealNameSelect"), "Meal Name:", choices = c("Loading..." = "")),
      conditionalPanel(
        condition = sprintf("input['%s'] == 'other'", ns("mealNameSelect")),
        textInput(ns("customMealName"), "Enter Custom Meal Name:")
      ),
      
      dateInput(ns("mealDate"), "Date:", value = Sys.Date()),
      selectInput(ns("needIngred"), "Need Ingredients?", choices = c("Yes", "No")),
      actionButton(ns("addMeal"), "Add Meal"),
      
      # New section: Dropdown and Mark as Completed button
      h4("Remove a meal"),
      selectInput(ns("mealDropdown"), "Select a Meal:", choices = NULL), # Dynamically populated
      dateInput(ns("mealDateremoval"), "Date:", value = Sys.Date()),
      actionButton(ns("markDropdownComplete"), "Remove Meal"), # Button to complete chore
      actionButton(ns("clearMeals"), "Clear All Meals")
      
    ),
    mainPanel(
      h3("Meal Plan"),
      tableOutput(ns("mealTable")) # Display meal plan
    )
  )
}

mealPlanServer <- function(id,db) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Live-update fitness data using `reactivePoll`
    refresh_data <- reactivePoll(
      intervalMillis = 2000,  # Check for updates every 2 seconds
      session = session,
      checkFunc = function() {dbGetQuery(db, "SELECT COUNT(*) FROM meal_planning_db")},  # Detect table changes
      valueFunc = function() { dbReadTable(db, "meal_planning_db") }  # Load updated data
    )
    
    # Populate dropdown 
    observe({
      meal_data <- refresh_data()
      
      updateSelectInput(session, "mealDropdown", choices = meal_data$Item)
    })
    # Populate mealNameSelect
    observe({
      meal_data <- load_data(db, "meal_lookup_table") %>% select(mealName) %>% distinct()
      recipe_choices <- c(meal_data$mealName, "Other (please specify)" = "other")
      updateSelectInput(session, "mealNameSelect", choices = recipe_choices)
    })
    
    
    # Add a new income
    observeEvent(input$addMeal, {
      mealName <- if (input$mealNameSelect == "other") input$customMealName else input$mealNameSelect
      dbExecute(db, "INSERT INTO meal_planning_db (Item ,Date, NeedIngred) VALUES (?, ?, ?)", 
                params = list(mealName, format(as.POSIXct(input$mealDate)),input$needIngred))
      
      updateSelectInput(session, "mealNameSelect", selected = "")
      updateTextInput(session, "customMealName", value = "")
      #updateTextInput(session, "mealName", value = "") # Clear input field
      updateDateInput(session, "mealDate", value = Sys.Date()) # Reset amount field
      updateSelectInput(session, "needIngred") # Reset amount field
    })
    
    # Render the meal plan table
    output$mealTable <- renderTable({
      refresh_data() %>% arrange(Date)
    })
    
    
    # Add ingredients to the shopping list list
    observeEvent(input$addMeal, {
      # load in the lookup table (long df of mealName, Item, Quantity, Unit)
      meal_data <- load_data(db, "meal_lookup_table")
      
      tempmealName <- if (input$mealNameSelect == "other") input$customMealName else input$mealNameSelect
      print(tempmealName)
      if (tempmealName %in% meal_data$mealName & input$needIngred=="Yes") {
        print("Item in lookup table, adding to shopping list.")
        meal_data <- meal_data %>% filter(mealName==tempmealName) %>% select(Item,Quantity,Unit )
        dbWriteTable(db, "shopping_list_db", meal_data, append = TRUE)
      } else if(input$needIngred=="No"){
        # dont do anything
      } else{
        shinyalert::shinyalert("The meal added is not in the lookup table. Ingredients have not been auto added to the shopping list.", type = "info")
      }
      
    })
    
    # Clear all meals
    observeEvent(input$clearMeals, {
      dbExecute(db, "DELETE FROM meal_planning_db")
    })
    

    
    # Handle Mark as Completed button click
    observeEvent(input$markDropdownComplete, {
      req(input$mealDropdown) # Ensure a meal is selected
    
      # Remove selected items from the database
      dbExecute(
        db,
        "DELETE FROM meal_planning_db WHERE Item = ? AND Date = ?",
        params = list(input$mealDropdown,format(as.POSIXct(input$mealDateremoval)))
      )
       
      
    })
    
  })
}
