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
mod_calendar_server <- function(input, output, session, usr) {
  ns <- session$ns
  
  output$calendarPlot <- renderTimeseriesCalendar({
    sensors <- usr$sensors
    tryCatch(
      expr = {
        timeseriesCalendar(
          data = sensors$data, 
          meta = sensors$meta, 
          inputId = 'main_panel_ui_1-sensor_select'
        )
      }, 
      error = function(err) {
        logger.error(err)
        NULL
      }
    )
  })
}

## To be copied in the UI
# mod_calendar_ui("calendar_ui_1")

## To be copied in the server
# callModule(mod_calendar_server, "calendar_ui_1")
