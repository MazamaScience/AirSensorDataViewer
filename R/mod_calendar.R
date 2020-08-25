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
mod_calendar_server <- function(input, output, session, obj) {
  ns <- session$ns
  
  observeEvent(
    ignoreInit = TRUE, 
    eventExpr = {
      obj$selected$sensor
      obj$selected$ed
      obj$selected$sd
      obj$selected$tab
    }, 
    handlerExpr = {
      # if ( obj$selected$tab == 'calendar' )
        # obj$updateAnnual(
        #   pas = obj$data$pas, 
        #   label = obj$selected$sensor, 
        #   date = obj$selected$ed
        # )
    }
  )
  
  output$calendarPlot <- renderTimeseriesCalendar({
      timeseriesCalendar(
        data = obj$data$sensors$data, 
        meta = obj$data$sensors$meta, 
        inputId = 'main_panel_ui_1-sensor_select'
      )
    })
}
    
## To be copied in the UI
# mod_calendar_ui("calendar_ui_1")
    
## To be copied in the server
# callModule(mod_calendar_server, "calendar_ui_1")
