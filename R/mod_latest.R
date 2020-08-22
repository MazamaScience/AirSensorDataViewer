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
mod_latest_server <- function(input, output, session, obj){
  ns <- session$ns
  
  observeEvent({
    obj$selected$page
    obj$selected$sensor
  }, {
    if ( obj$selected$page == 'latest' ) {
      obj$updateLatest(
        pas = obj$data$pas,
        label = obj$selected$sensor
      )
    }
  }, ignoreInit = TRUE)
  
  output$pm_latest <- renderPlotly({
    channelPlotly(obj$data$latest, channel = 'ab')
  })
  
  output$humidity_latest <- renderPlotly({
        humidityPlotly(obj$data$latest)
  })
  
  output$temperature_latest <- renderPlotly({
    temperaturePlotly(obj$data$latest)
  })
}

## To be copied in the UI
# mod_latest_ui("latest_ui_1")

## To be copied in the server
# callModule(mod_latest_server, "latest_ui_1")

