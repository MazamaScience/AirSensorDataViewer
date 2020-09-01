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
#' @importFrom gt gt_output
mod_compare_ui <- function(id) {
  ns <- NS(id)
  tagList(
      column(
        width = 12,
        wellPanel(
          leafletOutput(
            outputId = ns("comparisonLeaflet")
          ) %>% withLoader()
        )
    ),
    fluidRow(
      column(
        width = 4,
        #tags$h4("Sensor Status"),
        wellPanel(
          plotOutput(
            outputId = ns("sensorMonitorCorr")
          ) %>% withLoader()
        ),
        #tags$h4("Sensor-Monitor Correlation"),
        wellPanel(
          gt_output(
            outputId = ns("statusTable")
          ) %>% withLoader()
        )
      ),
      column(
        width = 8,
        #tags$h4("Sensor-Monitor Comparison"),
        wellPanel(
          plotOutput(
            height = "30vh",
            outputId = ns("sensorMonitorComp")
          ) %>% withLoader()
        )
      )
    )
  )
  
}

#' compare Server Function
#'
#' @noRd 
#' 
#' @importFrom gt gt 
#' @importFrom leaflet renderLeaflet
#' @importFrom waiter Waiter
#' @importFrom promises `%...>%` `%...!%` promise_all 
mod_compare_server <- function(input, output, session, usr) {
  ns <- session$ns
  
  # w <- Waiter$new(
  #   c(ns("statusTable"), ns("sensorMonitorCorr"), ns("sensorMonitorComp")), 
  #   spin_throbber(), 
  #   color = "#fff"
  # )

  output$comparisonLeaflet <- renderLeaflet({
    req(usr$sensor, usr$pwfsl)

    promise_all(sensor = usr$sensor, pwfsl = usr$pwfsl) %...>% with({
        comparisonLeaflet(sensor, pwfsl)
      }) %...!% (function(err) {
        catchError(err)
      })
    
  })
  
  output$sensorMonitorCorr <- renderPlot({
    req(usr$sensor, usr$pwfsl)
    
    promise_all(sensor = usr$sensor, pwfsl = usr$pwfsl) %...>% with({
      lmSensorMonitor(sensor, pwfsl) 
    }) %...!% (function(err) {
      catchError(err)
    })

  })
  
  output$sensorMonitorComp <- renderPlot({
    req(usr$pat)
    
    usr$pat %...>% (function(pat) {
      pat_monitorComparison(pat)
    }) %...!% (function(err) {
      catchError(err)
    })

  })
  
  output$statusTable <- render_gt({
    
    usr$pat %...>% (function(pat) {
      
      table <- pat$data %>% 
        summarise(
          "Number of Measurments" = length(pm25_A),
          "Recovered (%)" = mean(sum(!is.na(pm25_A))/length(pm25_A), sum(!is.na(pm25_B))/length(pm25_B))*100
          ) %>%
        pivot_longer(everything())
        
      
      gt(table) %>% 
        fmt_number(2, decimals = 0) %>% 
        cols_label(name = "", value = "") %>% 
        tab_header(pat$meta$label)
      
    })

  })
  
}

## To be copied in the UI
# mod_compare_ui("compare_ui_1")

## To be copied in the server
# callModule(mod_compare_server, "compare_ui_1")

