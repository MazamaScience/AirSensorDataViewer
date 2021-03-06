#' compare UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom rlang .data
#' @importFrom shiny NS tagList 
#' @importFrom leaflet leafletOutput
#' @importFrom gt gt_output
mod_compare_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      leafletOutput(
        outputId = ns("comparisonLeaflet")
      ) %>% withLoader()
    ),
    tags$hr(), 
    fluidRow(
      plotOutput(
        height = "30vh",
        outputId = ns("sensorMonitorComp")
      ) %>% withLoader()
    ),
    tags$hr(), 
    fluidRow(
      column(
        width = 4,
        gt_output(
          outputId = ns("statusTable")
        ) %>% withLoader()
      ),
      column(
        width = 8,
        plotOutput(
          outputId = ns("sensorMonitorCorr")
        ) %>% withLoader()
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
  
  output$comparisonLeaflet <- renderLeaflet({
    req(usr$sensor, usr$pwfsl)
    
    promise_all(sensor = usr$sensor, pwfsl = usr$pwfsl) %...>% with({
      
      # Notify the user if distance is large (>15km)
      if ( sensor$meta$pwfsl_closestDistance > 15000 ) {
        showNotification(
          ui = paste("The distance between", sensor$meta$label, "and", pwfsl$meta$siteName, "is >15 km."), 
          duration = 10
        )
      }
        
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
      pat_monitorComparison(pat, distanceCutoff = 30)
    }) %...!% (function(err) {
      catchError(err)
    })
    
  })
  
  output$statusTable <- render_gt({
    req(usr$pat)
    usr$pat %...>% (function(pat) {
      table <- pat$data %>% 
        summarise(
          "Number of Measurments" = length(.data$pm25_A),
          "Recovered (%)" = mean(sum(!is.na(.data$pm25_A))/length(.data$pm25_A), sum(!is.na(.data$pm25_B))/length(.data$pm25_B))*100
        ) %>%
        pivot_longer(everything())
      
      gt(table) %>% 
        fmt_number(2, decimals = 0) %>% 
        cols_label(name = "", value = "") %>% 
        tab_header(pat$meta$label)
      
    }) %...!% (function(err) {
      catchError(err)
    })
    
  })
  
}

## To be copied in the UI
# mod_compare_ui("compare_ui_1")

## To be copied in the server
# callModule(mod_compare_server, "compare_ui_1")

