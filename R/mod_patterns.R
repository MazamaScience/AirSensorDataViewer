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
#' @importFrom gt gt_output render_gt
mod_patterns_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      tags$h4("Daily-Hour Averages"), 
      tags$h6("Of selected Date Range"), 
      plotOutput(
        outputId = ns("patternPlot")
      ) %>% withLoader()
    ), 
    tags$hr(),
    fluidRow(
      column(
        tags$h4("Pollution Rose"), 
        tags$h6("Of selected Date Range"), 
        width = 8, 
        plotOutput(
          outputId = ns("windPlot")
        ) %>% withLoader()
      ),
      column(
        width = 4, 
        gt_output(
          outputId = ns("noaaTable")
        ) %>% withLoader()
      )
    )
  )
}

#' patterns Server Function
#'
#' @noRd 
#' @importFrom gt gt tab_header tab_source_note fmt_number cols_label
#' @importFrom waiter Waiter
#' @importFrom promises `%...>%` `%...!%` promise_all
#' @importFrom tidyr everything pivot_longer
mod_patterns_server <- function(input, output, session, usr){
  ns <- session$ns
  
  output$patternPlot <- renderPlot({
    req(usr$sensor)
    
    usr$sensor %...>% (function(sensor) {
      asdv_pm25Diurnal(sensor) + stat_meanByHour(output = "scaqmd")
    }) %...!% (function(err) {
      catchError(err)
    })
    
  })
  
  output$noaaTable <- render_gt({
    req(usr$noaa)
    
    promise_all(noaa = usr$noaa, sensor = usr$sensor) %...>% with({
      
      table <- noaa %>%
        dplyr::summarise(
          "Average Windspeed (m/s)" = mean(.data$ws, na.rm = TRUE),
          "Minimum Windspeed (m/s)" = min(.data$ws, na.rm = TRUE),
          "Maximum Windspeed (m/s)" = max(.data$ws, na.rm = TRUE),
          "Average Wind Direction (deg)" = mean(.data$wd, na.rm = TRUE),
          "Average Air Temperature (C)" = mean(.data$air_temp, na.rm = TRUE),
          "Minimum Air Temperature (C)"= min(.data$air_temp, na.rm = TRUE),
          "Maximum Air Temperature (C)" = max(.data$air_temp, na.rm = TRUE),
          "Average Relative Humidity (%)" = mean(.data$RH, na.rm = TRUE),
          "Minimum Relative Humidity (%)" = min(.data$RH, na.rm = TRUE),
          "Maximum Relative Humidity (%)" = max(.data$RH, na.rm = TRUE)
        ) %>% 
        tidyr::pivot_longer(everything()) 
      
      metDist <- tryCatch(
        expr = { 
          signif(
            unique(
              na.omit(
                geodist::geodist(
                  cbind(noaa$longitude, noaa$latitude), 
                  cbind(sensor$meta$longitude, sensor$meta$latitude)
                )
              )
            )[1], 
            digits = 2
          ) / 1000
        }, 
        error = function(err) {
          NA
        }
      )
      
      gt(table) %>%
        tab_header(
          title = "Addtional NOAA Weather Data",
          subtitle = paste("Distance to Station:", metDist, "km")
        ) %>%
        tab_source_note(
          source_note = "Source: Integrated Surface Database (ISD) https://www.ncdc.noaa.gov/isd"
        ) %>% 
        fmt_number(2, decimals = 1) %>%
        cols_label(name = "", value = "")
        
    }) %...!% (function(err) {
      catchError(err)
    })
    
  })
  
  output$windPlot <- renderPlot({
    req(usr$sensor, usr$noaa)
    
    promise_all(sensor = usr$sensor, noaa = usr$noaa) %...>% 
      with({
        sensor_pollutionRose(sensor, noaa)
      }) %...!% (function(err) {
        notify("Failed to load NOAA data. Try selecting a different date or a different sensor.")
        catchError(err)
      })
    
  })
  
}

## To be copied in the UI
# mod_patterns_ui("patterns_ui_1")

## To be copied in the server
# callModule(mod_patterns_server, "patterns_ui_1")

