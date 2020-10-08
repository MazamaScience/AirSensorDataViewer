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
      uiOutput(ns("yearLabel"), container = tags$text),
      tags$hr(),
      timeseriesCalendarOutput(
        outputId = ns("calendarPlot"), width = 800, height = 800
      ) %>% withLoader()
  )
}

#' calendar Server Function
#'
#' @noRd 
#' 
#' @importFrom tiotemp renderTimeseriesCalendar timeseriesCalendar
#' @importFrom promises `%...>%` `%...!%`
mod_calendar_server <- function(input, output, session, usr) {
  ns <- session$ns

  output$calendarPlot <- renderTimeseriesCalendar({
    req(usr$annual)
    
    usr$annual %...>% (function(annual) {
      timeseriesCalendar(
        data = annual$data, 
        meta = annual$meta, 
        inputId = 'main_panel_ui_1-sensor_select', 
        unitString = "  (\u00B5g/m\u00B3)"
      )
    }) %...!% (function(err) {
      notify("Failed to load annual data. Try selecting a different date (of year) or a different sensor.")
      catchError(err)
    })
    
  })
  
  output$yearLabel <- renderUI({
    yr <- usr$selected$year
    HTML(paste(usr$selected$sensor,": ", yr, sep = " "))
  })
  
}

## To be copied in the UI
# mod_calendar_ui("calendar_ui_1")

## To be copied in the server
# callModule(mod_calendar_server, "calendar_ui_1")
