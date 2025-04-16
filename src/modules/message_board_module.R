# Define UI for Message Board Module
messageBoardUI <- function(id) {
  ns <- NS(id) # Namespace for unique IDs within the module
  
  fluidPage(
    sidebarPanel(
      selectInput(ns("authorInput"), "Your Name:", choices = c("John", "Jesse", "GB")),
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
messageBoardServer <- function(id, db) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Live-update data using `reactivePoll`
    refresh_data <- reactivePoll(
      intervalMillis = 2000,  # Check for updates every 2 seconds
      session = session,
      checkFunc = function() { dbGetQuery(db, "SELECT COUNT(*) FROM message_board_db") },  # Detect table changes
      valueFunc = function() { dbReadTable(db, "message_board_db") }  # Load updated data
    )
    
  
    
    # Render the message board UI based on the comments.
    output$messageBoard <- renderUI({
      message_data <- refresh_data()
      if (nrow(message_data) > 0) {
        renderComments(refresh_data())
      } else {
        h4("No comments on the message board.")
      }
    })
    
    # Recursively build the comment UI with an attached reply button.
    renderComments <- function(comments) {
      comment_list <- comments %>% group_by(threadID) %>% arrange(messageID,.by_group = TRUE) %>% 
        summarise(comment_list=list(list(threadid=threadID,
                                         messageid=messageID,
                                         author=author,
                                         comment=comment))) %>% pull(comment_list)
      
      lapply(comment_list, function(comment) {
        div(
          style = "border: 1px solid #ccc; padding: 15px; margin-bottom: 20px; border-radius: 8px; background-color: #f9f9f9;",
          
          # Thread comments
          lapply(seq_along(comment$messageid), function(c) {
            div(
              style = "margin-bottom: 10px;",
              p(
                tags$span(style = "font-weight: bold; color: #333;", comment$author[c]),
                tags$span(": "),
                tags$span(style = "color: #555;", comment$comment[c])
              ),
              if (c < length(comment$messageid)) tags$hr()
            )
          }),
          
          # Buttons at the bottom
          div(
            style = "margin-top: 10px;",
            tags$button(
              type = "button",
              class = "btn btn-sm btn-primary action-button",
              onclick = sprintf("Shiny.setInputValue('%s', '%s', {priority: 'event'})",
                                ns("reply_click"), comment$threadid[1]),
              "Reply"
            ),
            tags$button(
              type = "button",
              class = "btn btn-sm btn-danger action-button",
              style = "margin-left: 10px;",
              onclick = sprintf("Shiny.setInputValue('%s', '%s', {priority: 'event'})",
                                ns("delete_click"), comment$threadid[1]),
              "Delete"
            )
          )
        )
      }) %>% tagList()
    }
    
    
    # Handle posting a new root-level comment.
    observeEvent(input$addComment, {
      threadID = as.character(Sys.time())  # use timestamp as unique ID
      messageID = paste0(threadID,"_1")
      dbExecute(db, "INSERT INTO message_board_db (threadID ,messageID, author, comment) VALUES (?, ?, ?, ?)", 
                params = list(threadID,messageID, input$authorInput, input$commentInput))
      updateTextInput(session, "commentInput", value = "")
      
    })
    
    # Store the threadID for replying
    activeThreadID <- reactiveVal(NULL)
                                  
    # Single observer that catches every reply click via the "reply_click" input.
    observeEvent(input$reply_click, {
      activeThreadID(input$reply_click)
      showModal(modalDialog(
          title = "Reply",
          textInput(ns("replyInput"), "Your Reply:", placeholder = "Write your reply here..."),
          textInput(ns("replyAuthor"), "Your Name:", placeholder = "Enter your name..."),
          footer = tagList(
            modalButton("Cancel"),
            actionButton(ns("submitReply"), "Post Reply")
          )
        ))
    })
        
    # One-time observer for when the user clicks "Post Reply" in the modal.
    observeEvent(input$submitReply, {
          # Ensure both reply fields have content.
          req(input$replyInput, input$replyAuthor, activeThreadID())
          threadID <- activeThreadID()
          new_id_n <- refresh_data()%>% filter(threadID %in% threadID) %>% nrow()+1
          messageID = paste0(threadID,"_",new_id_n)
          dbExecute(db, "INSERT INTO message_board_db (threadID ,messageID, author, comment) VALUES (?, ?, ?, ?)", 
                    params = list(threadID,messageID, input$replyAuthor, input$replyInput))
          removeModal()
          activeThreadID(NULL)  # Clear after submission
        })
    
    
    observeEvent(input$delete_click, {
      activeThreadID(input$delete_click)
      showModal(modalDialog(
        title = "Delete thread",
        "Are you sure you want to delete this thread?",
        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns("confirmDelete"), "Yes, Delete", class = "btn-danger")
        )
      ))
    })
    
    # One-time observer for when the user clicks "Post Reply" in the modal.
    observeEvent(input$confirmDelete, {
      # Ensure both reply fields have content.
      req(activeThreadID())
      threadID <- activeThreadID()
      dbExecute(db, "DELETE FROM message_board_db WHERE threadID IN (?)", 
                params = list(threadID))
      removeModal()
      activeThreadID(NULL)  # Clear after submission
    })
    
    })
}


