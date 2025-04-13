library(shiny)
library(jsonlite)

# Add Custom JavaScript for FullCalendar Initialization
jsCode <- "
Shiny.addCustomMessageHandler('initialize_calendar', function(eventJSON) {
  try {
  console.log(eventJSON)
  //const events = JSON.parse(eventJSON);
  const events = eventJSON;
  console.log('Parsed Events:', events);
  
  const calendarEl = document.getElementById('calendar');
  if (calendarEl) {
            const calendar = new FullCalendar.Calendar(calendarEl, {
                initialView: 'dayGridMonth',
                events: events, // Attach parsed events here
                editable: true,
                selectable: true 
            });
            calendar.render();
            console.log('Calendar rendered successfully.');
        } else {
            console.error('Element not found.');
        }
    } catch (error) {
        console.error('Error parsing or rendering events:', error);
    }
});
"

calendarUI <- function(id) {
  ns <- NS(id)
  fluidPage(
    tags$head(
      tags$script(src = "https://cdn.jsdelivr.net/npm/fullcalendar/index.global.min.js"),
      tags$script(HTML(jsCode))
    ),
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
        h3("Family Calendar"),
        div(id = "calendar", style = "width: 100%; height: 600px;") # Placeholder for the calendar
      )
    )
  )
}


# Define Server Logic for Calendar Module
# Updated Server Logic for the Calendar Module
calendarServer <- function(id, db) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Reactive value to store events
    events <- reactiveVal(data.frame(
      #id = integer(),
      title = character(),
      start = character(),
      end = character(),
      stringsAsFactors = FALSE
    ))
    
    # Add events dynamically
    observeEvent(input$addEvent, {
      startTime <- sprintf("%02d:%02d:00", input$startHour, input$startMinute)
      endTime <- sprintf("%02d:%02d:00", input$endHour, input$endMinute)
      eventStart <- format(as.POSIXct(paste(input$eventDate, startTime)), "%Y-%m-%dT%H:%M:%S")
      eventEnd <- format(as.POSIXct(paste(input$eventDate, endTime)), "%Y-%m-%dT%H:%M:%S")
      
      newEvent <- data.frame(
        #id = nrow(events()) + 1,
        title = input$eventDesc,
        start = eventStart,
        end = eventEnd,
        stringsAsFactors = FALSE
      )
      events(rbind(events(), newEvent))
      # Send updated events to client
      session$sendCustomMessage("initialize_calendar", jsonlite::toJSON(events(), auto_unbox = TRUE,pretty=FALSE))
    })
    
    # Ensure calendar initialization on the client side
    observe({
      if (nrow(events()) > 0) {
        session$sendCustomMessage("initialize_calendar", jsonlite::toJSON(events(), auto_unbox = TRUE,pretty=FALSE))
      } else {
        session$sendCustomMessage("initialize_calendar", "[]") # Send an empty array if no events exist
      }
    })
  })
}
