#' overview UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @importFrom tiotemp timeseriesMapOutput
mod_overview_ui <- function(id){
  ns <- NS(id)
  tagList(
    wellPanel(
      timeseriesMapOutput(
        outputId = ns("timeseriesMap")
      )
    )
    
  )
}
    
#' overview Server Function
#'
#' @noRd 
#' @importFrom tiotemp renderTimeseriesMap timeseriesMap
mod_overview_server <- function(input, output, session, values) {
  ns <- session$ns
 
  output$timeseriesMap <- renderTimeseriesMap({
    timeseriesMap(data = values$sensors$data, meta = values$sensors$meta)
  })
}
    
## To be copied in the UI
# mod_overview_ui("overview_ui_1")
    
## To be copied in the server
# callModule(mod_overview_server, "overview_ui_1")
 
