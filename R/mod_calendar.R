#' calendar UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @importFrom tiotemp timeseriesCalendarOutput
mod_calendar_ui <- function(id){
  ns <- NS(id)
  tagList(
    wellPanel(
      timeseriesCalendarOutput(
        outputId = ns("calendarPlot"), width = 800, height = 800
      )
    )
  )
}
    
#' calendar Server Function
#'
#' @noRd 
#' 
#' @importFrom tiotemp renderTimeseriesCalendar timeseriesCalendar
mod_calendar_server <- function(input, output, session, values) {
  ns <- session$ns
  output$calendarPlot <- renderTimeseriesCalendar({
    then(values$sensors, function(d) {
      timeseriesCalendar(
        data = d$data, 
        meta = d$meta, 
        inputId = 'main_panel_ui_1-sensor_select'
      )
    })
  })
}
    
## To be copied in the UI
# mod_calendar_ui("calendar_ui_1")
    
## To be copied in the server
# callModule(mod_calendar_server, "calendar_ui_1")
