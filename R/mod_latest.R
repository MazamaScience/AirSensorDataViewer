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
    column(
      width = 12,
      height = "800",
      wellPanel(
        plotlyOutput(
          outputId = ns("pm_latest")
        )
      )
    ),
    fluidRow(
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
  )
}

#' latest Server Function
#'
#' @noRd 
#' 
#' @importFrom plotly renderPlotly
#' @importFrom waiter Waiter spin_throbber
#' @importFrom promises `%...>%` `%...!%`
mod_latest_server <- function(input, output, session, usr){
  ns <- session$ns
  
  w <- Waiter$new(
    c(("pm_latest")),# ns("humidity_latest"), ns("temperature_latest")), 
    spin_throbber(), 
    color = "#fff"
  )
  
  output$pm_latest <- renderPlotly({

    usr$latest %...>% (function(latest) {
      channelPlotly(latest, channel = 'ab') 
    }) %...!% (function(err) {
      catchError(err)
    })

  })
  
  output$humidity_latest <- renderPlotly({

    usr$latest %...>% (function(latest) {
      humidityPlotly(latest)
    }) %...!% (function(err) {
      catchError(err)
    })
    
  })
  
  output$temperature_latest <- renderPlotly({

    usr$latest %...>% (function(latest) {
      temperaturePlotly(latest)
    }) %...!% (function(err) {
      catchError(err)
    })
    
  })
}

## To be copied in the UI
# mod_latest_ui("latest_ui_1")

## To be copied in the server
# callModule(mod_latest_server, "latest_ui_1")

