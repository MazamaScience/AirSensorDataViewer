#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  
  # Record session start 
  observe({ 
    logger.trace(paste("session started:", session$token)) 
  })
  # Record session end 
  session$onSessionEnded(function() { 
    logger.trace(paste("session ended:",  session$token)) 
  })
  
  # Create reactive values 
  values <- rv(
    init = TRUE, 
    pas = NULL, # pas obj 
    sensors = NULL, # sensor obj (full)
    pat = NULL, # pat obj (selected)
    sensor = NULL, # sensor obj (selected)
    latest = NULL, # latest pat obj
    navbar = NULL, # navbar tab
    tab = NULL, # tabset tab
    ed = NULL, # startdate 
    sd = NULL # enddate 
  )
  
  observeEvent(input$navbar, {
    logger.trace(paste("navbar:", input$navbar))
    values$navbar <- input$navbar
  })
  
  observeEvent(input$tab, {
    logger.trace(paste("tab:", input$tab))
    values$tab <- input$tab
  })
  
  # List the first level callModules here
  callModule(mod_main_panel_server, "main_panel_ui_1", values)
  callModule(mod_overview_server, "overview_ui_1", values)
  callModule(mod_calendar_server, "calendar_ui_1", values)
  callModule(mod_raw_server, "raw_ui_1", values) 
  callModule(mod_patterns_server, "patterns_ui_1", values)
  callModule(mod_compare_server, "compare_ui_1", values)
  callModule(mod_video_server, "video_ui_1", values)
  callModule(mod_latest_server, "latest_ui_1", values)
  callModule(mod_datatable_server, "datatable_ui_1", values)
  
  
}
