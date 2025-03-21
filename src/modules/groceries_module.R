# Define UI for Groceries Module
groceriesUI <- function(id) {
  ns <- NS(id) # Namespace for unique IDs within the module
  sidebarLayout(
    sidebarPanel(
      textInput(ns("groceryItem"), "Grocery Item:", placeholder = "E.g., Milk"),
      numericInput(ns("groceryQty"), "Quantity:", value = 1, min = 1),
      actionButton(ns("addGrocery"), "Add Item")
    ),
    mainPanel(
      h3("Grocery List"),
      tableOutput(ns("groceryTable")), # Display grocery list
      actionButton(ns("clearList"), "Clear List") # Option to clear the entire list
    )
  )
}

# Define Server Logic for Groceries Module
groceriesServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns # Namespace for handling IDs
    
    # Reactive values to store groceries
    groceries <- reactiveValues(data = data.frame(
      Item = character(),
      Quantity = numeric(),
      stringsAsFactors = FALSE
    ))
    
    # Add a new item to the list
    observeEvent(input$addGrocery, {
      newGrocery <- data.frame(
        Item = input$groceryItem,
        Quantity = input$groceryQty,
        stringsAsFactors = FALSE
      )
      groceries$data <- rbind(groceries$data, newGrocery)
      updateTextInput(session, "groceryItem", value = "") # Clear item field
      updateNumericInput(session, "groceryQty", value = 1) # Reset quantity field
    })
    
    # Render the grocery list table
    output$groceryTable <- renderTable({
      groceries$data
    })
    
    # Clear the grocery list
    observeEvent(input$clearList, {
      groceries$data <- data.frame(
        Item = character(),
        Quantity = numeric(),
        stringsAsFactors = FALSE
      )
    })
  })
}
