#' patterns UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @importFrom DT DTOutput
mod_patterns_ui <- function(id){
  ns <- NS(id)
  tagList(
    wellPanel(
      plotOutput(
        outputId = ns("patternPlot")
      ) 
    ),
    fluidRow(
      column(
        width = 5,
        
        tags$h4("Additional NOAA Weather Data"),
        wellPanel(
          DTOutput(
            outputId = ns("noaaTable")
          )
        )
      ),
      column(
        width = 7,
        tags$h4("Wind Rose Plot"),
        wellPanel(
          plotOutput(
            outputId = ns("windPlot")
          ) 
        )
      )
    )
  )
}

#' patterns Server Function
#'
#' @noRd 
#' @importFrom DT renderDT datatable
#' @importFrom waiter Waiter
#' @importFrom promises `%...>%` `%...!%`
mod_patterns_server <- function(input, output, session, usr){
  ns <- session$ns
  
  w <- Waiter$new(
    c(ns("patternPlot")), 
    spin_throbber(), 
    color = "#fff"
  )
  
  output$patternPlot <- renderPlot({
    sensor <- usr$sensor
      w$show()
      
      usr$sensor %...>% (function(sensor) {
        asdv_pm25Diurnal(sensor) + stat_meanByHour(output = "scaqmd")
      }) %...!% (function(err) {
        catchError(err)
      })
    
  })
  
  # output$noaaTable <- renderDT({
  #   req(values$noaa)
  #   L2$show()
  #   then(values$noaa, function(d) {
  #     datatable(
  #       data = noaaTable(d),
  #       selection = "none",
  #       colnames = "",
  #       options = list(dom = 't', bSort = FALSE),
  #       class = 'cell-border stripe'
  #     ) %>%
  #       formatRound(columns = 1, digits = 2)
  #   })
  # })
  # 
  # output$windPlot <- renderPlot({
  #   req(values$sensor)
  #   req(values$noaa)
  #   L3$show()
  #   then(values$sensor, function(d) {
  #     then(values$noaa, function(h) {
  #       sensor_pollutionRose(d, h)  
  #     })
  #   })
  # })
  
}

## To be copied in the UI
# mod_patterns_ui("patterns_ui_1")

## To be copied in the server
# callModule(mod_patterns_server, "patterns_ui_1")

