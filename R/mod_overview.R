#' overview UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @importFrom tiotemp timeseriesMapOutput timeseriesBarChartOutput 
mod_overview_ui <- function(id){
  ns <- NS(id)
  tagList(
    #wellPanel(
      timeseriesMapOutput(
        outputId = ns("timeseriesMap"),
        width = "100%", 
        height = 600
     ),
    #),
    wellPanel(
      timeseriesBarChartOutput(
        outputId = ns("timeseriesBarChart")
      )
    )
  )
  
}
    
#' overview Server Function
#'
#' @noRd 
#' @importFrom tiotemp renderTimeseriesMap timeseriesMap 
#' @importFrom tiotemp renderTimeseriesBarChart timeseriesBarChart
mod_overview_server <- function(input, output, session, values) {
  ns <- session$ns
  
  output$timeseriesMap <- renderTimeseriesMap({
    req(values$sensors)
    timeseriesMap(
      data = values$sensors$data, 
      meta = values$sensors$meta, 
      inputId = 'main_panel_ui_1-sensor_select'
    )
  })
  
  output$timeseriesBarChart <- renderTimeseriesBarChart({
    req(values$sensors)
    timeseriesBarChart(
      data = values$sensors$data, 
      meta = values$sensors$meta, 
      inputId = 'main_panel_ui_1-sensor_select', 
      ylab = "\u03bcg / m\u00b3"
    )
  })
  
}
    
## To be copied in the UI
# mod_overview_ui("overview_ui_1")
    
## To be copied in the server
# callModule(mod_overview_server, "overview_ui_1")
 
