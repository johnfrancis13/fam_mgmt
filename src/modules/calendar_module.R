library(shiny)
library(timevis)

calendarUI <- function(id) {
  ns <- NS(id) # Namespace for unique IDs
  fluidPage(
    sidebarLayout(
      sidebarPanel(
        textInput(ns("eventDesc"), "Event Description:", placeholder = "E.g., Family dinner"),
        dateInput(ns("eventDate"), "Event Date:", value = Sys.Date()),
        sliderInput(ns("startHour"), "Start Hour:", min = 0, max = 23, value = 12),
        sliderInput(ns("startMinute"), "Start Minute:", min = 0, max = 59, value = 0),
        sliderInput(ns("endHour"), "End Hour:", min = 0, max = 23, value = 13),
        sliderInput(ns("endMinute"), "End Minute:", min = 0, max = 59, value = 0),
        actionButton(ns("addEvent"), "Add Event")
      ),
      mainPanel(
        h3("Interactive Timeline"),
        timevisOutput(ns("timeline")) # Timeline output
      )
    )
  )
}




# Define Server Logic for Calendar Module
calendarServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Reactive values to store events
    events <- reactiveVal(data.frame(
      id = character(),
      content = character(),
      start = character(),
      end = character(), # New column for end time
      stringsAsFactors = FALSE
    ))
    
    # Add events to the timeline
    observeEvent(input$addEvent, {
      startTime <- sprintf("%02d:%02d:00", input$startHour, input$startMinute)
      endTime <- sprintf("%02d:%02d:00", input$endHour, input$endMinute)
      eventStart <- paste(input$eventDate, startTime)
      eventEnd <- paste(input$eventDate, endTime)
      
      newEvent <- data.frame(
        id = as.character(nrow(events()) + 1),
        content = input$eventDesc,
        start = eventStart,
        end = eventEnd, # Add the end time to the event
        stringsAsFactors = FALSE
      )
      events(rbind(events(), newEvent))
    })
    
    # Render the timeline with interactive pop-ups
    output$timeline <- renderTimevis({
      timevis(events(), options = list(
        editable = FALSE,
        tooltip = TRUE # Enable tooltips on hover
      ))
    })
    
    # Show pop-up with event details when an event is clicked
    observeEvent(input$timeline_selected, {
      selectedEvent <- events()[events()$id == input$timeline_selected, ]
      if (nrow(selectedEvent) > 0) {
        showModal(
          modalDialog(
            title = paste("Event:", selectedEvent$content),
            paste("Start:", selectedEvent$start),
            paste("End:", selectedEvent$end),
            footer = modalButton("Close")
          )
        )
      }
    })
  })
}

