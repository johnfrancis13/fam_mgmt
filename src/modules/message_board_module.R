# Define UI for Message Board Module
messageBoardUI <- function(id) {
  ns <- NS(id) # Namespace for unique IDs within the module
  sidebarLayout(
    sidebarPanel(
      textInput(ns("commentInput"), "Leave a Comment:", placeholder = "Write your message here..."),
      actionButton(ns("addComment"), "Post Comment")
    ),
    mainPanel(
      h3("Message Board"),
      uiOutput(ns("messageBoard")) # Dynamically render the message board
    )
  )
}

# Define Server Logic for Message Board Module
messageBoardServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns # Namespace for handling IDs
    
    # Reactive values to store comments and reactions
    messages <- reactiveValues(data = data.frame(
      Comment = character(),
      Reactions = integer(),
      stringsAsFactors = FALSE
    ))
    
    # Add a new comment
    observeEvent(input$addComment, {
      if (input$commentInput != "") {
        newComment <- data.frame(
          Comment = input$commentInput,
          Reactions = 0,
          stringsAsFactors = FALSE
        )
        messages$data <- rbind(messages$data, newComment)
        updateTextInput(session, "commentInput", value = "") # Clear input field
      }
    })
    
    # Render the message board dynamically
    output$messageBoard <- renderUI({
      if (nrow(messages$data) == 0) {
        h4("No comments yet. Be the first to post!")
      } else {
        commentList <- lapply(1:nrow(messages$data), function(i) {
          div(
            p(strong("Comment:"), messages$data$Comment[i]),
            p("Reactions:", messages$data$Reactions[i]),
            actionButton(ns(paste0("react_", i)), "React"),
            actionButton(ns(paste0("delete_", i)), "Delete"),
            tags$hr()
          )
        })
        do.call(tagList, commentList)
      }
    })
    
    # React to comments
    observe({
      lapply(1:nrow(messages$data), function(i) {
        observeEvent(input[[paste0("react_", i)]], {
          messages$data$Reactions[i] <- messages$data$Reactions[i] + 1
        })
      })
    })
    
    # Delete comments
    observe({
      lapply(1:nrow(messages$data), function(i) {
        observeEvent(input[[paste0("delete_", i)]], {
          messages$data <- messages$data[-i, ]
        })
      })
    })
  })
}
