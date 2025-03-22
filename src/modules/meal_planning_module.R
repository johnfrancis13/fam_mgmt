mealPlanUI <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      textInput(ns("mealName"), "Meal Name:", placeholder = "E.g., Spaghetti Bolognese"),
      dateInput(ns("mealDate"), "Date:", value = Sys.Date()),
      actionButton(ns("addMeal"), "Add Meal")
    ),
    mainPanel(
      h3("Meal Plan"),
      tableOutput(ns("mealTable")), # Display meal plan
      actionButton(ns("clearMeals"), "Clear Meal Plan")
    )
  )
}


mealPlanServer <- function(id,shopping_list) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Reactive values to store meals
    meals <- reactiveValues(data = data.frame(
      Name = character(),
      Date = as.Date(character()),
      stringsAsFactors = FALSE
    ))
    
    # Add a new meal to the plan
    observeEvent(input$addMeal, {
      newMeal <- data.frame(
        Name = input$mealName,
        Date = format(as.POSIXct(input$mealDate), "%Y-%m-%d"),
        stringsAsFactors = FALSE
      )
      meals$data <- rbind(meals$data, newMeal)
      updateTextInput(session, "mealName", value = "") # Clear meal name field
    })
    
    # Render the meal plan table
    output$mealTable <- renderTable({
      meals$data
    })
    
    
    # Clear the meal plan
    observeEvent(input$clearMeals, {
      meals$data <- data.frame(
        Name = character(),
        Date = as.Date(character()),
        stringsAsFactors = FALSE
      )
    })
    
    # Add ingredients to the shopping list list
    observeEvent(input$addMeal, {
      if (input$mealName == "Spaghetti Bolognese") {
        newShopping <- data.frame(
          Item = c("Spaghetti", "Tomato Sauce", "Ground Beef"),
          Quantity = c(1, 1, 1),
          stringsAsFactors = FALSE
        )
        shopping_list$data <- rbind(shopping_list$data, newShopping)
      }
    })
    
  })
}
