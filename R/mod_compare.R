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
    fluidRow(
      column(
        width = 12,
        wellPanel(
          leafletOutput(
            outputId = ns("comparisonLeaflet")
          )
        )
      )
    ),
    fluidRow(
      column(
        width = 5,
        tags$h4("Sensor Status"),
        wellPanel(
          DTOutput(
            outputId = ns("statusTable")
          )
        ),
        tags$h4("Sensor-Monitor Correlation"),
        wellPanel(
          plotOutput(
            outputId = ns("sensorMonitorCorr")
          ) 
        )
      ),
      column(
        width = 7,
        tags$h4("Sensor-Monitor Comparison"),
        wellPanel(
          plotOutput(
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
mod_compare_server <- function(input, output, session, obj) {
  ns <- session$ns
  
  w <- Waiter$new(
    c(ns("statusTable"), ns("sensorMonitorCorr"), ns("sensorMonitorComp"))
  )
  
  output$comparisonLeaflet <- renderLeaflet({
    sensor <- obj[['data']][['sensor']]
    pwfsl <- obj[['data']][['pwfsl']]
    tryCatch(
      expr = {
        comparisonLeaflet(sensor, pwfsl)
      }, 
      error = function(err) {
        logger.error(err)
        NULL
      }
    )
  })
  
  output$sensorMonitorCorr <- renderCachedPlot({
    # w$show()
    sensor <- obj[['data']][['sensor']]
    pwfsl <- obj[['data']][['pwfsl']]
    tryCatch(
      expr = {
        lmSensorMonitor(sensor, pwfsl) 
      }, 
      error = function(err) {
        logger.error(err)
        NULL
      }
    )
  }, cacheKey("sensorMonitorCorr", obj[['data']][['sensor']]))
  
  output$sensorMonitorComp <- renderCachedPlot({
    # w$show()
    pat <- obj[['data']][['pat']]
    tryCatch(
      expr = {
        pat_monitorComparison(pat)
      }, 
      error = function(err) {
        logger.error(err)
        NULL
      }
    )
  }, cacheKey("sensorMonitorComp", obj[['data']][['pat']]))
  
  output$statusTable <- renderDT({
    #w$show()
    pat <- obj[['data']][['pat']]
    tryCatch(
      expr = {
        datatable(
          data = sensorMonitorCompTable(pat),
          selection = "none",
          colnames = "",
          options = list(dom = 't', bSort = FALSE),
          class = 'cell-border stripe'
        ) %>%
          formatRound(columns = 1, digits = 2)
      }, 
      error = function(err) {
        logger.error(err)
        NULL
      }
    )
  })
  
}

## To be copied in the UI
# mod_compare_ui("compare_ui_1")

## To be copied in the server
# callModule(mod_compare_server, "compare_ui_1")

