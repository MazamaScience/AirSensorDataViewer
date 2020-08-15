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
    timeseriesMapOutput(
      outputId = ns("timeseriesMap"),
      width = "100%", 
      height = 600
    ),
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
    then(values$sensors, function(d) {
      timeseriesMap(
        data = d$data, 
        meta = d$meta, 
        inputId = 'main_panel_ui_1-sensor_select', 
        selected = isolate(values$sensor_select)
      )
    }, onRejected = function(err) {
      logger.error(err)
    })
  })
  
  output$timeseriesBarChart <- renderTimeseriesBarChart({
    req(values$sensors)
    then(values$sensors, function(d) {
      timeseriesBarChart(
        data = d$data, 
        meta = d$meta, 
        inputId = 'main_panel_ui_1-sensor_select', 
        ylab = "\u03bcg / m\u00b3"
      )
    }, onRejected = function(err) {
      logger.error(err)
    })
  })
  
}

## To be copied in the UI
# mod_overview_ui("overview_ui_1")

## To be copied in the server
# callModule(mod_overview_server, "overview_ui_1")

