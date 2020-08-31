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
      ) 
    ),
    fluidRow(
      column(
        width = 5,
        tags$h4("Additional NOAA Weather Data"),
        wellPanel(
          gt_output(
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
#' @importFrom promises `%...>%` `%...!%` promise_all
mod_patterns_server <- function(input, output, session, usr){
  ns <- session$ns
  
  # w <- Waiter$new(
  #   c(ns("patternPlot")), 
  #   spin_throbber(), 
  #   color = "#fff"
  # )
  
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
          avg_ws = mean(ws, na.rm = TRUE),
          min_ws = min(ws, na.rm = TRUE),
          max_ws = max(ws, na.rm = TRUE),
          avg_wd = mean(wd, na.rm = TRUE),
          avg_t = mean(air_temp, na.rm = TRUE),
          min_t = min(air_temp, na.rm = TRUE),
          max_t = max(air_temp, na.rm = TRUE),
          avg_rh = mean(RH, na.rm = TRUE),
          min_rh = min(RH, na.rm = TRUE),
          max_rh = max(RH, na.rm = TRUE),
          )
      
      gt(table) %>%
        tab_header(
          title = "Addtional NOAA Weather Data",
          subtitle = ""
        ) %>%
        tab_source_note(
          source_note = "Source: Integrated Surface Database (ISD) https://www.ncdc.noaa.gov/isd"
        )
      
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

