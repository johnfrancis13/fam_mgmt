# Home Page UI
homePageUI <- function(id) {
  ns <- NS(id) # Namespace for unique IDs
  fluidPage(
    # Center align the content
    tags$div(
      style = "text-align: center; margin-top: 50px;",
      h1("Welcome to The Family Management App"),   # Main title
      h4("Organize your chores, finances, and health with ease!"), # Subtitle/description
      #img(src = "https://via.placeholder.com/300", alt = "Placeholder Image", style = "margin: 20px 0;"), # Optional image
      #actionButton(ns("getStarted"), "Get Started", class = "btn-primary btn-lg"), # Button
      tags$div(style = "margin-top: 20px;"),
      actionButton(ns("learnMore"), "Learn More About Us", class = "btn-secondary btn-lg")
    )
  )
}

# Home Page Server
homePageServer <- function(id,db) {
  moduleServer(id, function(input, output, session) {
    
    # Example: Navigation when "Learn More" is clicked
    observeEvent(input$learnMore, {
      showModal(modalDialog(
        title = "About Us",
        "This is a simple app designed to help families stay organized and connected.",
        easyClose = TRUE,
        footer = NULL
      ))
    })
  })
}
