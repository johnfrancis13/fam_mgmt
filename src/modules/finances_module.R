# Define UI for Finances Module
financesUI <- function(id) {
  ns <- NS(id) # Namespace for unique IDs within the module
  sidebarLayout(
    sidebarPanel(
      numericInput(ns("expenseAmount"), "Expense Amount:", value = 0, min = 0, step = 1),
      textInput(ns("expenseDesc"), "Description:", placeholder = "E.g., Monthly rent"),
      actionButton(ns("addExpense"), "Add Expense")
    ),
    mainPanel(
      h3("Expense Tracker"),
      tableOutput(ns("expenseTable")), # Display expense data
      plotOutput(ns("expenseChart"))   # Visualization of expenses
    )
  )
}

# Define Server Logic for Finances Module
financesServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns # Namespace for handling IDs
    
    # Reactive values to store expenses
    expenses <- reactiveValues(data = data.frame(
      Description = character(),
      Amount = numeric(),
      stringsAsFactors = FALSE
    ))
    
    # Add a new expense
    observeEvent(input$addExpense, {
      newExpense <- data.frame(
        Description = input$expenseDesc,
        Amount = input$expenseAmount,
        stringsAsFactors = FALSE
      )
      expenses$data <- rbind(expenses$data, newExpense)
      updateTextInput(session, "expenseDesc", value = "") # Clear input field
      updateNumericInput(session, "expenseAmount", value = 0) # Reset amount field
    })
    
    # Render the expense table
    output$expenseTable <- renderTable({
      expenses$data
    })
    
    # Render the expense chart
    output$expenseChart <- renderPlot({
      if (nrow(expenses$data) > 0) {
        barplot(
          expenses$data$Amount,
          names.arg = expenses$data$Description,
          col = "blue",
          main = "Expense Breakdown",
          xlab = "Description",
          ylab = "Amount"
        )
      }
    })
  })
}
