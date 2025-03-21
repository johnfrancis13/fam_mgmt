# Load required libraries
library(shiny)
# app.R
source("modules/chores_module.R")
source("modules/finances_module.R")
source("modules/groceries_module.R")
source("modules/calendar_module.R")
source("modules/message_board_module.R")

ui <- fluidPage(
  titlePanel("Family Management App"),
  tabsetPanel(
    tabPanel("Chores", choresUI("chores")),
    tabPanel("Finances", financesUI("finances")),
    tabPanel("Groceries", groceriesUI("groceries")),
    tabPanel("Calendar", calendarUI("calendar")),
    tabPanel("Message Board", messageBoardUI("messageBoard"))
  )
)

server <- function(input, output, session) {
  choresServer("chores")
  financesServer("finances")
  groceriesServer("groceries")
  calendarServer("calendar")
  messageBoardServer("messageBoard")
}

shinyApp(ui, server)