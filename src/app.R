# Load required libraries
library(shiny)
# app.R
source("modules/chores_module.R")
source("modules/finances_module.R")
source("modules/Shopping_list_module.R")
source("modules/meal_planning_module.R")
source("modules/calendar_module.R")
source("modules/message_board_module.R")

ui <- fluidPage(
  titlePanel("Family Management App"),
  tabsetPanel(
    tabPanel("Chores", choresUI("chores")),
    tabPanel("Finances", financesUI("finances")),
    tabPanel("Shopping list", shopping_listUI("shopping")),
    tabPanel("Meal Planning", mealPlanUI("meal_plan")),
    tabPanel("Calendar", calendarUI("calendar")),
    tabPanel("Message Board", messageBoardUI("messageBoard"))
  )
)

server <- function(input, output, session) {
  shopping_list <- reactiveValues(data = data.frame(
    Item = character(),
    Quantity = numeric(),
    stringsAsFactors = FALSE
  ))
  
  choresServer("chores")
  financesServer("finances")
  shopping_listServer("shopping",shopping_list)
  mealPlanServer("meal_plan",shopping_list)
  calendarServer("calendar")
  messageBoardServer("messageBoard")
}

shinyApp(ui, server)