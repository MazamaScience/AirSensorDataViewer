#' overview UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_overview_ui <- function(id){
  ns <- NS(id)
  tagList(
    
    
  )
}
    
#' overview Server Function
#'
#' @noRd 
mod_overview_server <- function(input, output, session){
  ns <- session$ns
 
}
    
## To be copied in the UI
# mod_overview_ui("overview_ui_1")
    
## To be copied in the server
# callModule(mod_overview_server, "overview_ui_1")
 