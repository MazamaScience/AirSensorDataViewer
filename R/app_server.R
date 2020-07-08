#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  
  values <- rv()
  
  # List the first level callModules here
  callModule(mod_main_panel_server, "main_panel_ui_1", values)

}
