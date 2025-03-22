# Define UI for shopping_list Module
shopping_listUI <- function(id) {
  ns <- NS(id) # Namespace for unique IDs within the module
  sidebarLayout(
    sidebarPanel(
      textInput(ns("shopping_listItem"), "Shopping list Item:", placeholder = "E.g., Milk"),
      numericInput(ns("shopping_listQty"), "Quantity:", value = 1, min = 1),
      actionButton(ns("addshopping_list"), "Add Item")
    ),
    mainPanel(
      h3("Shopping List"),
      tableOutput(ns("shopping_listTable")), # Display shopping_list list
      actionButton(ns("clearList"), "Clear List") # Option to clear the entire list
    )
  )
}

# Define Server Logic for shopping_list Module
shopping_listServer <- function(id,shopping_list) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns # Namespace for handling IDs
    
    # Add a new item to the list
    observeEvent(input$addshopping_list, {
      newshopping_list <- data.frame(
        Item = input$shopping_listItem,
        Quantity = input$shopping_listQty,
        stringsAsFactors = FALSE
      )
      shopping_list$data <- rbind(shopping_list$data, newshopping_list)
      updateTextInput(session, "shopping_listItem", value = "") # Clear item field
      updateNumericInput(session, "shopping_listQty", value = 1) # Reset quantity field
    })
    
    # Render the shopping_list list table
    output$shopping_listTable <- renderTable({
      shopping_list$data
    })
    
    # Clear the shopping_list list
    observeEvent(input$clearList, {
      shopping_list$data <- data.frame(
        Item = character(),
        Quantity = numeric(),
        stringsAsFactors = FALSE
      )
    })
  })
}
