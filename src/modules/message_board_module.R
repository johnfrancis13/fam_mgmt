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
messageBoardServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Reactive list to store comments and replies
    messages <- reactiveVal(list())
    
    # Function to add a comment or a reply.
    addComment <- function(parentId = NULL, author, text) {
      newComment <- list(
        id = as.character(Sys.time()),  # use timestamp as unique ID
        parentId = parentId,
        author = author,
        text = text,
        replies = list()
      )
      if (is.null(parentId)) {
        messages(c(messages(), list(newComment)))
      } else {
        messages(addReply(messages(), parentId, newComment))
      }
    }
    
    # Recursive function to add a reply into the right place.
    addReply <- function(comments, parentId, reply) {
      for (i in seq_along(comments)) {
        if (comments[[i]]$id == parentId) {
          comments[[i]]$replies <- c(comments[[i]]$replies, list(reply))
          return(comments)
        } else if (length(comments[[i]]$replies) > 0) {
          comments[[i]]$replies <- addReply(comments[[i]]$replies, parentId, reply)
        }
      }
      comments
    }
    
    # Recursive helper function to find a comment by id.
    findCommentById <- function(comments, id) {
      for (comment in comments) {
        if (comment$id == id) return(comment)
        if (length(comment$replies) > 0) {
          found <- findCommentById(comment$replies, id)
          if (!is.null(found)) return(found)
        }
      }
      NULL
    }
    
    # Render the message board UI based on the comments.
    output$messageBoard <- renderUI({
      renderComments(messages())
    })
    
    # Recursively build the comment UI with an attached reply button.
    renderComments <- function(comments) {
      lapply(comments, function(comment) {
        div(
          p(strong(comment$author), ": ", comment$text),
          # Notice the onclick attribute! When clicked, it sets input$reply_click to the comment id.
          actionButton(
            inputId = ns(paste0("reply_", comment$id)),
            label = "Reply",
            onclick = sprintf("Shiny.setInputValue('%s', '%s', {priority: 'event'})",
                              ns("reply_click"), comment$id)
          ),
          # Recursively render replies with indentation.
          div(style = "margin-left:20px;", renderComments(comment$replies)),
          tags$hr()
        )
      }) %>% tagList()
    }
    
    # Handle posting a new root-level comment.
    observeEvent(input$addComment, {
      if (nzchar(input$authorInput) && nzchar(input$commentInput)) {
        addComment(NULL, input$authorInput, input$commentInput)
        updateTextInput(session, "commentInput", value = "")
      }
    })
    
    # Single observer that catches every reply click via the "reply_click" input.
    observeEvent(input$reply_click, {
      clicked_id <- input$reply_click
      # Retrieve the comment details so we can show the author’s name in the modal.
      clickedComment <- findCommentById(messages(), clicked_id)
      if (!is.null(clickedComment)) {
        showModal(modalDialog(
          title = paste("Reply to", clickedComment$author),
          textInput(ns("replyInput"), "Your Reply:", placeholder = "Write your reply here..."),
          textInput(ns("replyAuthor"), "Your Name:", placeholder = "Enter your name..."),
          footer = tagList(
            modalButton("Cancel"),
            actionButton(ns("submitReply"), "Post Reply")
          )
        ))
        
        # One-time observer for when the user clicks "Post Reply" in the modal.
        observeEvent(input$submitReply, {
          # Ensure both reply fields have content.
          req(input$replyInput, input$replyAuthor)
          addComment(clicked_id, input$replyAuthor, input$replyInput)
          removeModal()
        }, ignoreInit = TRUE, once = TRUE)
      }
    })
  })
}


