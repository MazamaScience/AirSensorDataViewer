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
    pas = NULL, # pas obj 
    pat = NULL, # pat obj (selected)
    sensor = NULL, # sensor obj (selected)
    sensors = NULL, # sensor obj (full)
    latest = NULL, # latest pat obj
    nav = NULL # navbar tab
  )
  
  observeEvent(input$navbar, {
    logger.trace(paste("navbar:", input$navbar))
    values$nav <- input$navbar
  })
  
  # List the first level callModules here
  callModule(mod_main_panel_server, "main_panel_ui_1", values)
  callModule(mod_raw_server, "raw_ui_1", values) 
  callModule(mod_latest_server, "latest_ui_1", values)
  callModule(mod_datatable_server, "datatable_ui_1", values)
  
}
