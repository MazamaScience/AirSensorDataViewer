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
#' @import leaflet
#' @import ggplot2
mod_compare_server <- function(input, output, session, obj) {
  ns <- session$ns
  
  observeEvent(
    ignoreInit = TRUE,
    eventExpr = {
      obj$selected$sensor
      obj$selected$sd
      obj$selected$ed
    },
    handlerExpr = {
      obj$updateSensor(
        obj$data$sensors, 
        .data$label == obj$selected$sensor
      )
      
      obj$updatePwfsl(
        obj$data$sensor$meta$pwfsl_closestMonitorID,
        obj$selected$sd,
        obj$selected$ed
      )
    }
  )
  
  output$comparisonLeaflet <- renderLeaflet({
    comparisonLeaflet(obj$data$sensor, obj$data$pwfsl)
  })
  
  output$sensorMonitorCorr <- renderPlot({
    lmSensorMonitor(obj$data$sensor, obj$data$pwfsl) 
  })
  
  output$sensorMonitorComp <- renderPlot({
    obj[['data']][['pat']] %...>%
      pat_monitorComparison() %...!%
      catchError()
  })
  
  
  output$statusTable <- renderDT({
    obj[['data']][['pat']] %...>% {
      datatable(
        data = sensorMonitorCompTable(.),
        selection = "none",
        colnames = "",
        options = list(dom = 't', bSort = FALSE),
        class = 'cell-border stripe'
      ) %>%
        formatRound(columns = 1, digits = 2)
    } %...!%
      catchError()

  })
  
}

## To be copied in the UI
# mod_compare_ui("compare_ui_1")

## To be copied in the server
# callModule(mod_compare_server, "compare_ui_1")

