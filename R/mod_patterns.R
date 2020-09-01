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
    wellPanel(
      plotOutput(
        outputId = ns("patternPlot")
      ) %>% withLoader() 
    ),
    fluidRow(
      column(
        width = 5,
        # tags$h4("Additional NOAA Weather Data"),
        wellPanel(
          gt_output(
            outputId = ns("noaaTable")
          ) %>% withLoader()
        )
      ),
      column(
        width = 7,
        tags$h4("Wind Rose Plot"),
        wellPanel(
          plotOutput(
            outputId = ns("windPlot")
          ) %>% withLoader()
        )
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
    
    usr$noaa %...>% (function(noaa) {
      table <- noaa %>%
        summarise(
          "Average Windspeed (m/s)" = mean(ws, na.rm = TRUE),
          "Minimum Windspeed (m/s)" = min(ws, na.rm = TRUE),
          "Maximum Windspeed (m/s)" = max(ws, na.rm = TRUE),
          "Average Wind Direction (deg)" = mean(wd, na.rm = TRUE),
          "Average Air Temperature (C)" = mean(air_temp, na.rm = TRUE),
          "Minimum Air Temperature (C)"= min(air_temp, na.rm = TRUE),
          "Maximum Air Temperature (C)" = max(air_temp, na.rm = TRUE),
          "Average Relative Humidity (%)" = mean(RH, na.rm = TRUE),
          "Minimum Relative Humidity (%)" = min(RH, na.rm = TRUE),
          "Maximum Relative Humidity (%)" = max(RH, na.rm = TRUE),
        ) %>% 
        pivot_longer(everything())
      
      gt(table) %>%
        tab_header(
          title = "Addtional NOAA Weather Data",
          subtitle = ""
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
        catchError(err)
      })
    
  })
  
}

## To be copied in the UI
# mod_patterns_ui("patterns_ui_1")

## To be copied in the server
# callModule(mod_patterns_server, "patterns_ui_1")

