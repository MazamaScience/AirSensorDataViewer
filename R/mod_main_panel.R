#' main_panel UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @importFrom lubridate today ymd days 
#' @importFrom shinyWidgets pickerInput airDatepickerInput radioGroupButtons sliderTextInput
mod_main_panel_ui <- function(id) {
  ns <- NS(id)
  TZ <- 'UTC'
  tagList(
    
    selectizeInput(
      inputId = ns("community_select"),
      label = tags$h4("Community"), 
      selected = NULL,
      choices = "Loading Communities..."
    ),
    
    selectizeInput(
      inputId = ns("sensor_select"),
      label = tags$h4("Sensor"), 
      selected = NULL,
      choices = "Loading Sensors..."
    ),
    
    dateRangeInput(
      inputId = ns("date_range"), 
      label = tags$h4("Date Range"), 
      start = (today(tzone = TZ) - days(1)) - days(7), 
      end = (today(tzone = TZ) - days(1)), 
      min = ymd(20171001), 
      max = (today(tzone = TZ) - days(1))
    )
    
  )
}

#' main_panel Server Function
#'
#' @noRd 
#' 
#' @import AirSensor
#' @import MazamaCoreUtils
#' @importFrom future future resolved
#' @importFrom promises then catch
#' @importFrom lubridate ymd ymd_hms days years %within% %--% 
#' @importFrom shinyWidgets updatePickerInput
#' @importFrom PWFSLSmoke createEmptyMonitor
#' @importFrom rlang .data
#' @importFrom waiter Waitress
#' @importFrom stats na.omit 
mod_main_panel_server <- function(input, output, session, obj, selected) {
  ns <- session$ns
  startupWaitress <- waitress()
  startupWaitress$notify(html = tags$h3("Loading Data..."), position = "bl")
  startupWaitress$set(20)
  observeEvent(
    ignoreNULL = TRUE,
    once = TRUE, 
    eventExpr = {
      obj$data$sensors
    },
    handlerExpr = {
      # Check diff bewteen sensors aobj in sensor obj and pas obj and only use
      # the sensors with mutual existence
      communities <- na.omit(unique(id2com(obj$data$sensors$meta$communityRegion)))
      sensor_labels <- na.omit(unique(obj$data$sensors$meta$label))
      
      # update sensors rv
      #objues$sensors <- d
      # Fill the community selection
      updateSelectizeInput(
        session,
        inputId = "community_select",
        selected = "All...",
        choices = c("Choose a community" = "","All...", communities)
      )
      # Fill the sensor selection
      updateSelectizeInput(
        session,
        inputId = "sensor_select",
        choices = sensor_labels
      )
      startupWaitress$close()
    }
  )
  
  observeEvent(
    ignoreNULL = TRUE,
    ignoreInit = TRUE,
    eventExpr = {
      input$sensor_select
    }, 
    handlerExpr = {
      # Throws a freaking loop
      # updateSelectizeInput(
      #   session,
      #   inputId = "sensor_select",
      #   selected = isolate(input$sensor_select)
      # )
      obj$selected$sensor <- isolate(input$sensor_select)
    }
  )
  
  observeEvent(
    ignoreNULL = TRUE,
    eventExpr = {
      input$date_range
    }, 
    handlerExpr = {
      
      if ( ymd(input$date_range[2]) <= ymd(input$date_range[1]) ) {
        updateDateRangeInput(
          session,
          "date_range",
          start = ymd(input$date_range[2]) - days(1)
        )
      }
      if ( ymd(input$date_range[2]) - ymd(input$date_range[1]) > 21 ) {
        updateDateRangeInput(
          session,
          "date_range",
          start = ymd(input$date_range[2]) - days(21)
        )
        showNotification("Dates range too long!", "Max date range is 21 days.", type = "warn")
      }
      
      obj$updateSensors(input$date_range[1], input$date_range[2])
      
      obj$selected$sd <- input$date_range[1]
      obj$selected$ed <- input$date_range[2]
      
    }
  )
  
  observeEvent(
    ignoreInit = TRUE, 
    eventExpr = {
      input$community_select
    }, 
    handlerExpr = {
      
      if ( input$community_select == "All..." ) {
        choices <- obj$data$sensors$meta
      } else {
        choices <-
          obj$data$sensors$meta[id2com(obj$data$sensors$meta$communityRegion) ==
                                  input$community_select,]
      }
      updateSelectizeInput(
        session,
        "sensor_select",
        choices = na.omit(choices$label)
      )
      
      obj$selected$community <- input$community_select
      
    }
  )
  # observeEvent(ignoreInit = TRUE,obj$selected$page, {
  #   if (objues$selected$page == 'latest') {
  #     shinyjs::hide("date_range", anim = TRUE)
  #   } else {
  #     shinyjs::show("date_range", anim = TRUE)
  #   }
  # })
  # 
} # End Server

## To be copied in the UI
# mod_main_panel_ui("main_panel_ui_1")

## To be copied in the server
# callModule(mod_main_panel_server, "main_panel_ui_1")

