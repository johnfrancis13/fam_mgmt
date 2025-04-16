# Load required libraries
library(shiny)
library(DBI)
library(RSQLite)

ui <- fluidPage(
  titlePanel("Family Management App"),
  
  # Add login UI at the top
  shinyauthr::loginUI("login"),
  
  # Use conditional UI to show the rest of the app only after login
  uiOutput("auth_content")


)

server <- function(input, output, session) {
  # app.R
  source("utils.R",local = TRUE)
  source("modules/home_module.R",local = TRUE)
  source("modules/chores_module.R",local = TRUE)
  source("modules/finances_module.R",local = TRUE)
  source("modules/Shopping_list_module.R",local = TRUE)
  source("modules/meal_planning_module.R",local = TRUE)
  source("modules/calendar_module.R",local = TRUE)
  source("modules/message_board_module.R",local = TRUE)
  source("modules/fitness_module.R",local = TRUE)
  
  # Open database connection
  db <- dbConnect(SQLite(), "data/family_management.db")
  ensure_database_setup(db)
  
  user_base <- data.frame(
    user = c("John", "Jess", "GB"),
    password = sapply(c("password123", "mypassword", "anotherpassword"), digest::digest), # Hashed passwords
    stringsAsFactors = FALSE
  )
  
  # Implement login logic using shinyauthr
  credentials <- shinyauthr::loginServer(
    id = "login",
    data = user_base,
    user_col ="user",
    pwd_col = "password",
    log_out = reactiveVal(FALSE)
  )
  
  # Render conditional app content after login
  output$auth_content <- renderUI({
    req(credentials()$user_auth) # Only display content if authenticated
    
    tabsetPanel(
      tabPanel("Home", homePageUI("home")),
      tabPanel("Chores", choresUI("chores")),
      tabPanel("Finances", financesUI("finances")),
      tabPanel("Shopping list", shopping_listUI("shopping")),
      tabPanel("Meal Planning", mealPlanUI("meal_plan")),
      tabPanel("Calendar", calendarUI("calendar")),
      tabPanel("Message Board", messageBoardUI("messageBoard")),
      tabPanel("Fitness", fitnessUI("fitness"))
    )
  })
  
  observeEvent(credentials()$user_auth, {
    req(credentials()$user_auth)  # ensure login is successful
  homePageServer("home", db)
  choresServer("chores", db)
  financesServer("finances", db)
  shopping_listServer("shopping",db)
  mealPlanServer("meal_plan",db)
  calendarServer("calendar", db)
  messageBoardServer("messageBoard", db)
  fitnessServer("fitness", db)
  })
  
  # Close the connection when the session ends
  session$onSessionEnded(function() {
    dbDisconnect(db)
  })
  
}

shinyApp(ui, server)