#' compare UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @importFrom leaflet leafletOutput
#' @importFrom DT DTOutput
mod_compare_ui <- function(id) {
  ns <- NS(id)
  tagList(
      column(
        width = 12,
        wellPanel(
          leafletOutput(
            outputId = ns("comparisonLeaflet")
          )
        )
    ),
    fluidRow(
      column(
        width = 4,
        #tags$h4("Sensor Status"),
        wellPanel(
          plotOutput(
            outputId = ns("sensorMonitorCorr")
          ) 
        ),
        #tags$h4("Sensor-Monitor Correlation"),
        wellPanel(
          DTOutput(
            outputId = ns("statusTable")
          )
        )
      ),
      column(
        width = 8,
        #tags$h4("Sensor-Monitor Comparison"),
        wellPanel(
          plotOutput(
            height = "30vh",
            outputId = ns("sensorMonitorComp")
          ) 
        )
      )
    )
  )
  
}

#' compare Server Function
#'
#' @noRd 
#' 
#' @importFrom DT datatable formatRound renderDT
#' @importFrom leaflet renderLeaflet
#' @importFrom waiter Waiter
#' @importFrom promises `%...>%` `%...!%` promise_all 
mod_compare_server <- function(input, output, session, usr) {
  ns <- session$ns
  
  w <- Waiter$new(
    c(ns("statusTable"), ns("sensorMonitorCorr"), ns("sensorMonitorComp")), 
    spin_throbber(), 
    color = "#fff"
  )
  
  output$comparisonLeaflet <- renderLeaflet({
    
    w$show()

    promise_all(sensor = usr$sensor, pwfsl = usr$pwfsl) %...>% with({
        comparisonLeaflet(sensor, pwfsl)
      }) %...!% (function(err) {
        catchError(err)
      })
    
  })
  
  output$sensorMonitorCorr <- renderPlot({
    
    promise_all(sensor = usr$sensor, pwfsl = usr$pwfsl) %...>% with({
      lmSensorMonitor(sensor, pwfsl) 
    }) %...!% (function(err) {
      catchError(err)
    })

  })
  
  output$sensorMonitorComp <- renderPlot({
    
    usr$pat %...>% (function(pat) {
      pat_monitorComparison(pat)
    }) %...!% (function(err) {
      catchError(err)
    })

  })
  
  output$statusTable <- renderDT({
    
    usr$pat %...>% (function(pat) {
      datatable(
        data = sensorMonitorCompTable(pat),
        selection = "none",
        colnames = "",
        options = list(dom = 't', bSort = FALSE),
        class = 'cell-border stripe'
      ) %>%
        formatRound(columns = 1, digits = 2)
    })

  })
  
}

## To be copied in the UI
# mod_compare_ui("compare_ui_1")

## To be copied in the server
# callModule(mod_compare_server, "compare_ui_1")

