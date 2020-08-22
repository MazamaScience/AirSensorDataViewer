#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  
  options(warn = -1)
  
  future::plan(future::multiprocess)
  
  # Record session start 
  observe({ 
    logger.trace(paste("session started:", session$token)) 
  })
  # Record session end 
  session$onSessionEnded(function() { 
    logger.trace(paste("session ended:",  session$token)) 
  })
  
  DataObj <- R6::R6Class(
    "DataObject",
    public = list(
      url = "http://data.mazamascience.com/PurpleAir/v1",
      token = session$token, 
      
      selected = rv(),
      data = rv(),
      
      initialize = function() {
        logger.trace("init")
        setArchiveBaseUrl(self$url)
        self$data$pas <- get_pas()
        self$data$sensors <- get_sensors(today() - days(7), today())
      },
      
      updatePas = function() {
        setArchiveBaseUrl(self$url)
        self$data$pas <- get_pas()
      }, 
      
      updateSensors = function(sd, ed) {
        setArchiveBaseUrl(self$url)
        self$data$sensors <- get_sensors(sd, ed)
      }, 
      
      updateSensor = function(sensors, ...) {
        self$data$sensor <- AirSensor::sensor_filterMeta(sensors, ...)
      }, 
      
      updatePat = function(pas, label, sd, ed) {
        setArchiveBaseUrl(self$url)
        self$data$pat <- get_pat(
          pas = pas,
          label = label,
          sd = sd,
          ed = ed
        )
      }, 
      
      updatePwfsl = function(id, sd, ed) {
        self$data$pwfsl <- PWFSLSmoke::monitor_load(sd, ed, id)
      }, 
      
      updateLatest = function(pas, label, tz = 'UTC') {
        self$data$latest <- get_pat_latest(pas, label, tz)
      }
    )
  )
  
  obj <- DataObj$new()
  
  observeEvent(input$navbar, {
    logger.trace(paste("navbar:", input$navbar))
    obj$selected$page <- input$navbar
  })
  
  observeEvent(input$tab, {
    logger.trace(paste("tab:", input$tab))
    obj$selected$tab <- input$tab
  })
  
  # List the first level callModules here
  callModule(mod_main_panel_server, "main_panel_ui_1", obj)
  callModule(mod_overview_server, "overview_ui_1", obj)
  callModule(mod_calendar_server, "calendar_ui_1", obj)
  callModule(mod_raw_server, "raw_ui_1", obj) 
  callModule(mod_patterns_server, "patterns_ui_1", obj)
  callModule(mod_compare_server, "compare_ui_1", obj)
  callModule(mod_video_server, "video_ui_1", obj)
  callModule(mod_latest_server, "latest_ui_1", obj)
  callModule(mod_datatable_server, "datatable_ui_1", obj)
  
  
}
