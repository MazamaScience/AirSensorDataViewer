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
            outputId = ns("comparisonMap")
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
mod_compare_server <- function(input, output, session, values) {
  ns <- session$ns
  
  # leaflet??
  L1 <- waiter(ns("sensorMonitorCorr"))
  L2 <- waiter(ns("sensorMonitorComp"))
  L3 <- waiter(ns("statusTable"))
  
  output$sensorMonitorCorr <- renderPlot({
    req(values$sensor)
    L1$show()
    then(values$sensor, function(d) {
      asdv_externalFit(d, tz = 'UTC')
    })
  })
  
  output$sensorMonitorComp <- renderPlot({
    req(values$pat)
    L2$show()
    then(values$pat, function(d) {
      pat_monitorComparison(values$pat)
    })
  })
  
  output$statusTable <- renderDT({
    req(values$pat)
    L3$show()
    then(values$pat, function(d) {
      datatable(
        data = sensorMonitorCompTable(values$pat), 
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
 
