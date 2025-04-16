library(shiny)
library(jsonlite)
library(lubridate)

# Add Custom JavaScript for FullCalendar Initialization with Event Details Popup
jsCode <- "
Shiny.addCustomMessageHandler('initialize_calendar', function(eventJSON) {
  try {
    console.log(eventJSON);
    const events = eventJSON; // Use the parsed events JSON
    console.log('Parsed Events:', events);

    const calendarEl = document.getElementById('calendar');
    if (calendarEl) {
      const calendar = new FullCalendar.Calendar(calendarEl, {
        initialView: 'dayGridMonth',
        events: events, // Attach parsed events here
        editable: true,
        selectable: true,
        
        // Add event click handler to show a modal with event details
        eventClick: function(info) {
          // Create a modal to display event details
          const eventDetails = info.event;
          const eventTitle = eventDetails.title;
          const eventStart = eventDetails.start.toLocaleString();
          const eventEnd = eventDetails.end ? eventDetails.end.toLocaleString() : 'N/A';
          const eventDescription = eventDetails.extendedProps.description || 'No description available';
          
          // Display modal with event details
          const modalContent = `
            <h4>Event Details</h4>
            <p><strong>Title:</strong> ${eventTitle}</p>
            <p><strong>Start:</strong> ${eventStart}</p>
            <p><strong>End:</strong> ${eventEnd}</p>
            <p><strong>Description:</strong> ${eventDescription}</p>
            <button onclick='closeModal()'>Close</button>
          `;
          
          // Create and display the modal dynamically
          const modal = document.createElement('div');
          modal.setAttribute('id', 'eventModal');
          modal.setAttribute('style', 'position: fixed; top: 20%; left: 50%; transform: translateX(-50%); background-color: white; padding: 20px; border: 1px solid #ccc; box-shadow: 0 0 10px rgba(0, 0, 0, 0.1); z-index: 1000; width: 300px; height: auto; overflow: auto;');
          modal.innerHTML = modalContent;
          document.body.appendChild(modal);
        }
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

function closeModal() {
  const modal = document.getElementById('eventModal');
  if (modal) {
    modal.remove();
  }
}
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
        textInput(ns("eventTitle"), "Event Title:", placeholder = "E.g., Family dinner"),
        textInput(ns("eventDesc"), "Event Description:", placeholder = "E.g., Dinner at olive garden"),
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
    
    # Live-update data using `reactivePoll`
    refresh_data <- reactivePoll(
      intervalMillis = 2000,  # Check for updates every 2 seconds
      session = session,
      checkFunc = function() { dbGetQuery(db, "SELECT COUNT(*) FROM calendar_db") },  # Detect table changes
      valueFunc = function() { dbReadTable(db, "calendar_db") }  # Load updated data
    )

    
    # Add events dynamically
    observeEvent(input$addEvent, {
      startTime <- sprintf("%02d:%02d:00", input$startHour, input$startMinute)
      endTime <- sprintf("%02d:%02d:00", input$endHour, input$endMinute)
      eventStart <- format(as.POSIXct(paste(input$eventDate, startTime)), "%Y-%m-%dT%H:%M:%S")
      eventEnd <- format(as.POSIXct(paste(input$eventDate, endTime)), "%Y-%m-%dT%H:%M:%S")
      
      dbExecute(db, "INSERT INTO calendar_db (title ,start, end,description) VALUES (?, ?, ?,?)", 
                params = list(input$eventTitle,eventStart, eventEnd,input$eventDesc))
      
      
      # Send updated events to client
      session$sendCustomMessage("initialize_calendar", jsonlite::toJSON(refresh_data(), auto_unbox = TRUE,pretty=FALSE))
    })
    
    # Ensure calendar initialization on the client side
    observe( {
      later::later(function() {
        events <- dbReadTable(db, "calendar_db")
        if (nrow(events) > 0) {
          print("Initializing calendar with data...")
          session$sendCustomMessage("initialize_calendar", jsonlite::toJSON(events, auto_unbox = TRUE,pretty=FALSE))
        } else {
          print("Initializing a blank calendar without data...")
          session$sendCustomMessage("initialize_calendar", "[]") # Send an empty array if no events exist
        }
      }, delay = 2)  # delay is in seconds
      
      
    })
  })
}
