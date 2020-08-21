#' latest UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @importFrom plotly plotlyOutput
mod_latest_ui <- function(id){
  ns <- NS(id)
  tagList(
    "w1" = wellPanel(
      plotlyOutput(
        outputId = ns("pm_latest")
      )
    ),
    column(
      width = 6,
      wellPanel(
        plotlyOutput(
          outputId = ns("humidity_latest")
        ) 
      )
    ),
    column(
      width = 6,
      wellPanel(
        plotlyOutput(
          outputId = ns("temperature_latest")
        ) 
      )
    )
  )
}

#' latest Server Function
#'
#' @noRd 
#' 
#' @importFrom plotly renderPlotly
mod_latest_server <- function(input, output, session, values){
  ns <- session$ns
  
  output$pm_latest <- renderPlotly({
    req(values$pat_latest)
      then(values$pat_latest, function(d) {
        channelPlotly(pat = d, channel = 'ab')
      })
  })
  
  output$humidity_latest <- renderPlotly({
    req(values$pat_latest)
      then(values$pat_latest, function(d) {
        humidityPlotly(pat = d)
      })
  })
  
  output$temperature_latest <- renderPlotly({
    req(values$pat_latest)
      then(values$pat_latest, function(d) {
        temperaturePlotly(pat = d)
      })
  })
}

## To be copied in the UI
# mod_latest_ui("latest_ui_1")

## To be copied in the server
# callModule(mod_latest_server, "latest_ui_1")

