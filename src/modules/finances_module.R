library(ggplot2)
# Define UI for Finances Module
financesUI <- function(id) {
  ns <- NS(id) # Namespace for unique IDs within the module
  sidebarLayout(
    sidebarPanel(
      numericInput(ns("expenseAmount"), "Expense Amount:", value = 0, min = 0, step = 1),
      textInput(ns("expenseDesc"), "Description:", placeholder = "E.g., Monthly rent"),
      actionButton(ns("addExpense"), "Add Monthly Expense"),
      
      numericInput(ns("incAmount"), "Income Amount:", value = 0, min = 0, step = 1),
      textInput(ns("incDesc"), "Description:", placeholder = "E.g., Dad's salary"),
      actionButton(ns("addIncome"), "Add Monthly Income"),
      
      # New section: Dropdown and Mark as Completed button
      h4("Remove an expense/income"),
      selectInput(ns("finrowDropdown"), "Select a row to remove:", choices =  c("Waiting for data..." = ""), selected = NULL), # Dynamically populated
      actionButton(ns("editDropdown"), "Remove row"), # Button to remove an item
      actionButton(ns("clearAll"), "Remove all rows")
    ),
    mainPanel(
      #h3("Expense Tracker"),
      fluidRow(
        column(6, plotOutput(ns("expenseChart"))),
        column(6, plotOutput(ns("incomeBudgetPlot")))  # New plot for income vs. budget difference
      ),
      fluidRow(
        column(6, tableOutput(ns("expenseTable"))),
        column(6, tableOutput(ns("incTable"))) 
      ),

      
    )
  )
}

# Define Server Logic for Finances Module
financesServer <- function(id, db) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns # Namespace for handling IDs
    
    # Live-update data using `reactivePoll`
    refresh_data <- reactivePoll(
      intervalMillis = 2000,  # Check for updates every 2 seconds
      session = session,
      checkFunc = function() { dbGetQuery(db, "SELECT COUNT(*) FROM finances_db") },  # Detect table changes
      valueFunc = function() { dbReadTable(db, "finances_db") }  # Load updated data
    )
    
    # Render table
    output$expenseTable <- renderTable({
      refresh_data() %>% filter(Type %in% "Expense")
    })
    output$incTable <- renderTable({
      refresh_data()%>% filter(Type %in% "Income")
    })
    
    
    # Add a new expense
    observeEvent(input$addExpense, {
      dbExecute(db, "INSERT INTO finances_db (Type ,Item, Quantity) VALUES (?, ?, ?)", 
                params = list("Expense",input$expenseDesc, input$expenseAmount))
  
      updateTextInput(session, "expenseDesc", value = "") # Clear input field
      updateNumericInput(session, "expenseAmount", value = 0) # Reset amount field
    })
    
    # Add a new income
    observeEvent(input$addIncome, {
      dbExecute(db, "INSERT INTO finances_db (Type ,Item, Quantity) VALUES (?, ?, ?)", 
                params = list("Income",input$incDesc, input$incAmount))
      
      updateTextInput(session, "incDesc", value = "") # Clear input field
      updateNumericInput(session, "incAmount", value = 0) # Reset amount field
    })
    
    # Remove rows
    observeEvent(input$editDropdown, {
      req(input$finrowDropdown) # Ensure a row is selected
      
      dbExecute(db, "DELETE FROM finances_db WHERE Item IN (?)", 
                params = list(paste(input$finrowDropdown, collapse = "','")))
      
      
    })
  
    observe({
      finance_data <- refresh_data()
      finance_items <- finance_data %>%
        pull(Item) # Get the chore names

      print("Here")
      print(finance_items)
      updateSelectInput(session, "finrowDropdown", choices = finance_items)
    })

    
    # Clear all rows
    observeEvent(input$clearAll, {
      dbExecute(db, "DELETE FROM finances_db")
    })
    
    # Budget plot
    output$incomeBudgetPlot <- renderPlot({
      # Refresh the dataset
      finances <- refresh_data()
      
      if (nrow(finances) > 0) {
        # Calculate totals
        total_income <- sum(finances$Quantity[finances$Type == "Income"], na.rm = TRUE)
        total_expense <- sum(finances$Quantity[finances$Type == "Expense"], na.rm = TRUE)
        
        # Calculate differences
        diff_income_budget <- total_income - total_expense
        max_value <- max(c(total_income, total_expense, diff_income_budget), na.rm = TRUE)
        min_value <- min(c(0,total_income, total_expense, diff_income_budget), na.rm = TRUE)
        
        # Create a bar plot
        bar_midpoints <- barplot(
          c(total_income, total_expense, diff_income_budget),
          names.arg = c("Monthly Income", "Monthly Budget", "Monthly Savings"),
          col = c( "blue", "red","green"),
          main = "Monthly Income vs. Budget Difference",
          ylab = "Amount",
          ylim = c(min_value*1.2, max_value * 1.2)  
        )
        # Add value labels above each bar
        text(
          x = bar_midpoints,                      # Position on x-axis (bar centers)
          y = c(total_income, total_expense, diff_income_budget) * 1.05, # Position on y-axis slightly above the bars
          labels = c(total_income, total_expense, diff_income_budget), # Labels (values)
          col = "black",                          # Label color
          cex = 1.5                               # Font size for labels
        )
      }
    })
    
    # Render the expense chart
    output$expenseChart <- renderPlot({
      expenses <- refresh_data() %>% filter(Type %in% "Expense")
      
      if (nrow(expenses) > 0) {
        # Calculate percentage of each expense
        expenses <- expenses %>%
          mutate(Percentage = round(100 * Quantity / sum(Quantity), 1))
        
        # Create a bar plot or donut chart using ggplot2
        ggplot(expenses, aes(x = "", y = Quantity, fill = Item)) +
          geom_bar(stat = "identity", width = 1) +
          coord_polar("y", start = 0) +  # Convert to polar coordinates for pie/donut style
          labs(title = "Monthly Expense Breakdown", fill = "Expenses") +
          theme_void() +  # Clean theme
          theme(
            plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
            legend.position = "right"
          ) +
          geom_text(aes(label = paste0(Percentage, "%")),
                    position = position_stack(vjust = 0.5), 
                    color = "black", size = 4)
      }
    })
    
  })
}
