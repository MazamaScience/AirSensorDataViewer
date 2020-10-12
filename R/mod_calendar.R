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
      ) %>% withLoader(), 
      uiOutput(ns("calendarLegend"))
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
  
  output$calendarLegend <- renderUI({
    HTML('<h4>24-hr Legend</h4>
<i class="legend" style="background: #abe3f4"></i><span class="legend" >0-8 &#181;g/&#13221;</span>
           <i class="legend" style="background: #118cba"></i><span class="legend">8-20 &#181;g/&#13221;</span>
           <i class="legend" style="background: #286096"></i><span class="legend">20-35 &#181;g/&#13221;</span>
           <i class="legend" style="background: #8659a5"></i><span class="legend">35-55 &#181;g/&#13221;</span>
           <i class="legend" style="background: #6a367a"></i><span class="legend">&gt;55 &#181;g/&#13221;</span>
           <style>
           /*Legend specific*/
           .legend {
             padding: 2px 15px;
             font: 14px Arial, Helvetica, sans-serif;
             background: white;
             background: rgba(255, 255, 255, 0.8);
             box-shadow: 0 0 15px rgba(0, 0, 0, 0.2);
             line-height: 24px;
             color: #555;
           }
           .legend h4 {
             text-align: center;
             font-size: 16px;
             margin: 2px 12px 8px;
             color: #777;
           }
           .legend span {
             position: relative;
             bottom: 3px;
             padding-left: 5px;
           }
           .legend i {
             width: 18px;
             height: 18px;
             float: left;
             margin: 0 8px 0 0;
             opacity: 0.7;
           }
           .legend i.icon {
             background-size: 18px;
             background-color: rgba(255, 255, 255, 1);
           }
         </style>')
  })
  
}

## To be copied in the UI
# mod_calendar_ui("calendar_ui_1")

## To be copied in the server
# callModule(mod_calendar_server, "calendar_ui_1")
