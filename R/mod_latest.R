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
  
  w <- Waiter$new(c(ns("pm_latest")))
  
  observeEvent(
    ignoreNULL = TRUE,
    ignoreInit = TRUE,
    eventExpr = {
    obj$selected$page
    obj$selected$sensor
  }, {
    if ( obj$selected$page == 'latest' ) {
      obj$updateLatest(
        pas = obj$data$pas,
        label = obj$selected$sensor,
        tz = obj$clientTz
      )
    }
  })
  
  output$pm_latest <- renderPlotly({
    req(obj$data$latest)
    latest <- obj$data$latest 
    
    tryCatch(
      expr= {
        channelPlotly(latest, channel = 'ab') 
      }, 
      error = function(err) {
        logger.error(err)
        NULL
      }
    )
    
  })
  
  output$humidity_latest <- renderPlotly({
    req(obj$data$latest)
    latest <- obj$data$latest
    
    tryCatch(
      expr = {
        humidityPlotly(latest)
      }, 
      error = function(err) {
        logger.error(err)
        NULL
      }
    )
    
  })
  
  output$temperature_latest <- renderPlotly({
    req(obj$data$latest)
    latest <- obj$data$latest
    
    tryCatch(
      expr = {
        temperaturePlotly(latest)
      }, 
      error = function(err) {
        logger.error(err)
        NULL
      }
    )
    
  })
}

## To be copied in the UI
# mod_latest_ui("latest_ui_1")

## To be copied in the server
# callModule(mod_latest_server, "latest_ui_1")

