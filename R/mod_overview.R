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
      width = "inherit", 
      height = "60vh"
    ) %>% withLoader(),
    timeseriesBarChartOutput(
      outputId = ns("timeseriesBarChart"), 
      height = "20vh"
    ) %>% withLoader()
  )
}

#' overview Server Function
#'
#' @noRd 
#' @importFrom tiotemp renderTimeseriesMap timeseriesMap 
#' @importFrom tiotemp renderTimeseriesBarChart timeseriesBarChart
#' @importFrom promises `%...>%` `%...!%`
mod_overview_server <- function(input, output, session, usr) {
  ns <- session$ns
  
  timezone <- getOption("asdv.timezone")
  
  output$timeseriesMap <- renderTimeseriesMap({
    req(usr$sensors)
    selected <- isolate(usr$selected$sensor)
    sensors <- usr$sensors
    
    usr$sensors %...>% (function(sensors) {
      timeseriesMap(
        data = sensors[['data']], 
        meta = sensors[['meta']], 
        inputId = 'main_panel_ui_1-sensor_select', 
        selected = selected, 
        tz = timezone
      )
    }) %...!% (function(err) {
      catchError(err)
    })
    
  })
  
  output$timeseriesBarChart <- renderTimeseriesBarChart({
    req(usr$sensors)
    
    usr$sensors %...>% (function(sensors) {
      timeseriesBarChart(
        data = sensors[['data']], 
        meta = sensors[['meta']],
        inputId = 'main_panel_ui_1-sensor_select',
        ylab = "PM\u2082.\u2085 (\u03bcg / m\u00b3)", 
        xlab = "Date", 
        tz = timezone
      ) 
    }) %...!% (function(err) {
      notify("Failed to load sensor data. Try selecting a different date or a different sensor.")
      catchError(err)
    })
    
  })
  
}

## To be copied in the UI
# mod_overview_ui("overview_ui_1")

## To be copied in the server
# callModule(mod_overview_server, "overview_ui_1")

