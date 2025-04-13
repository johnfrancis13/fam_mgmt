# Define UI for shopping_list Module
shopping_listUI <- function(id) {
  ns <- NS(id)  # Namespace for unique IDs within the module
  
  sidebarLayout(
    sidebarPanel(
      textInput(ns("shopping_listItem"), "Shopping list Item:", placeholder = "E.g., Milk"),
      numericInput(ns("shopping_listQty"), "Quantity:", value = 1, min = 1),
      textInput(ns("shopping_listUnit"), "Unit:", placeholder = "E.g., litres"),
      actionButton(ns("addshopping_list"), "Add Item"),
    ),
    mainPanel(
      h3("Shopping List"),
      tableOutput(ns("shopping_listTable")),  # Display shopping list
      uiOutput(ns("checkboxList")),  # Dynamically render checkboxes
      actionButton(ns("clearSelected"), "Clear Selected Items"),  # Clear selected items
      actionButton(ns("clearList"), "Clear Entire List", class = "btn-danger")  # Option to clear the entire list
    )
  )
}


# Define Server Logic for shopping_list Module
shopping_listServer <- function(id, db) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns  # Namespace for handling IDs
    
    # Live-update fitness data using `reactivePoll`
    refresh_data <- reactivePoll(
      intervalMillis = 2000,  # Check for updates every 2 seconds
      session = session,
      checkFunc = function() { dbGetQuery(db, "SELECT COUNT(*) FROM shopping_list_db") },  # Detect table changes
      valueFunc = function() { dbReadTable(db, "shopping_list_db") }  # Load updated data
    )
    
    # Render shopping list table
    output$shopping_listTable <- renderTable({
     refresh_data()
    })
    
    # Add a new item to the shopping list (insert into database)
    observeEvent(input$addshopping_list, {
      dbExecute(db, "INSERT INTO shopping_list_db (Item, Quantity, Unit) VALUES (?, ?, ?)", 
                params = list(input$shopping_listItem, input$shopping_listQty,input$shopping_listUnit))
      
      updateTextInput(session, "shopping_listItem", value = "")  # Clear input field
      updateNumericInput(session, "shopping_listQty", value = 1)  # Reset quantity field
      updateTextInput(session, "shopping_listUnit", value = "")  # Reset unit field
    })
    
    # Clear the shopping list (delete all records)
    observeEvent(input$clearList, {
      dbExecute(db, "DELETE FROM shopping_list_db")
    })
    
    # Dynamically generate checkboxes for each shopping list item
    output$checkboxList <- renderUI({
      shopping_data <- refresh_data()
      if (nrow(shopping_data) > 0) {
        checkboxGroupInput(ns("selectedItems"), "Select Items to Remove:",
                           choices = shopping_data$Item)
      } else {
        h4("No items in the shopping list.")
      }
    })
    
    # Clear selected items from the shopping list
    observeEvent(input$clearSelected, {
      req(input$selectedItems)  # Ensure at least one item is selected
      
      # Remove selected items from the database
      dbExecute(
        db,
        "DELETE FROM shopping_list_db WHERE Item IN (?)",
        params = list(paste(input$selectedItems, collapse = "','"))
      )
    })
    
  })
}

