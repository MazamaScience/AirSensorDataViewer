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
    ),
    
    tags$hr(), 
    
    fluidRow(
      column(
        downloadLink(
          outputId = ns("download_button"), 
          label = tags$div(HTML('<svg width="1em" height="1em" viewBox="0 0 16 16" class="bi bi-cloud-download" fill="currentColor" xmlns="http://www.w3.org/2000/svg">
  <path fill-rule="evenodd" d="M4.406 1.342A5.53 5.53 0 0 1 8 0c2.69 0 4.923 2 5.166 4.579C14.758 4.804 16 6.137 16 7.773 16 9.569 14.502 11 12.687 11H10a.5.5 0 0 1 0-1h2.688C13.979 10 15 8.988 15 7.773c0-1.216-1.02-2.228-2.313-2.228h-.5v-.5C12.188 2.825 10.328 1 8 1a4.53 4.53 0 0 0-2.941 1.1c-.757.652-1.153 1.438-1.153 2.055v.448l-.445.049C2.064 4.805 1 5.952 1 7.318 1 8.785 2.23 10 3.781 10H6a.5.5 0 0 1 0 1H3.781C1.708 11 0 9.366 0 7.318c0-1.763 1.266-3.223 2.942-3.593.143-.863.698-1.723 1.464-2.383z"/>
  <path fill-rule="evenodd" d="M7.646 15.854a.5.5 0 0 0 .708 0l3-3a.5.5 0 0 0-.708-.708L8.5 14.293V5.5a.5.5 0 0 0-1 0v8.793l-2.146-2.147a.5.5 0 0 0-.708.708l3 3z"/>
</svg> Download...'))
        ), 
        width = 6
      ),
      column(
        actionLink(
          inputId = ns("share_button"), 
          label = tags$div(HTML('<svg width="1em" height="1em" viewBox="0 0 16 16" class="bi bi-link-45deg" fill="currentColor" xmlns="http://www.w3.org/2000/svg">
  <path d="M4.715 6.542L3.343 7.914a3 3 0 1 0 4.243 4.243l1.828-1.829A3 3 0 0 0 8.586 5.5L8 6.086a1.001 1.001 0 0 0-.154.199 2 2 0 0 1 .861 3.337L6.88 11.45a2 2 0 1 1-2.83-2.83l.793-.792a4.018 4.018 0 0 1-.128-1.287z"/>
  <path d="M5.712 6.96l.167-.167a1.99 1.99 0 0 1 .896-.518 1.99 1.99 0 0 1 .518-.896l.167-.167A3.004 3.004 0 0 0 6 5.499c-.22.46-.316.963-.288 1.46z"/>
  <path d="M6.586 4.672A3 3 0 0 0 7.414 9.5l.775-.776a2 2 0 0 1-.896-3.346L9.12 3.55a2 2 0 0 1 2.83 2.83l-.793.792c.112.42.155.855.128 1.287l1.372-1.372a3 3 0 0 0-4.243-4.243L6.586 4.672z"/>
  <path d="M10 9.5a2.99 2.99 0 0 0 .288-1.46l-.167.167a1.99 1.99 0 0 1-.896.518 1.99 1.99 0 0 1-.518.896l-.167.167A3.004 3.004 0 0 0 10 9.501z"/>
</svg> Share...'))
        ), 
        width =  6
      )
      
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
#' @importFrom clipr write_clip
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
  
  observeEvent(input$share_button, {
    
    logger.trace(paste0("bookmarked @: ", obj$url))
    clipr::write_clip(obj$url)
    
  })
  
  output$download_button <- downloadHandler(
    filename = function() {
      paste0(obj$selected$sensor,'_',obj$selected$sd,'_',obj$selected$ed,".csv")
    },
    content = function(file) {
      write.csv(obj$data$pat$data, file, row.names = FALSE)
    }
  )
  
  observeEvent(ignoreInit = TRUE,obj$selected$page, {
    if (obj$selected$page == 'latest') {
      shinyjs::hide("date_range", anim = TRUE)
    } else {
      shinyjs::show("date_range", anim = TRUE)
    }
  })
  
  # 
} # End Server

## To be copied in the UI
# mod_main_panel_ui("main_panel_ui_1")

## To be copied in the server
# callModule(mod_main_panel_server, "main_panel_ui_1")

