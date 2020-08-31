#' datatable UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @importFrom DT DTOutput
mod_datatable_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # # Meta explorer
    column(
      width = 9,
      tableOutput(
        outputId = ns("metatable")
      )
    ),
    # Data explorer
    column(
      width = 10,
      DTOutput(
        outputId = ns("datatable")
      ) 
    )
  )
}

#' datatable Server Function
#'
#' @noRd 
#' 
#' @importFrom DT renderDT datatable formatDate
#' @importFrom promises `%...>%` `%...!%`
mod_datatable_server <- function(input, output, session, usr) {
  ns <- session$ns
  
  w <- Waiter$new(ns("datatable"))
  
  output$metatable <- renderTable({
    req(usr$pat)
    
    usr$pat %...>% (function(pat) {
      data.frame( "Sensor" = pat$meta$label,
                  "Community" = pat$meta$communityRegion,
                  "Sensor Type" = pat$meta$sensorType,
                  "Longitude" = pat$meta$longitude,
                  "Latitude" = pat$meta$latitude,
                  "State" = pat$meta$stateCode,
                  "Country" = pat$meta$countryCode )  
    }) %...!% (function(err) {
      catchError(err)
    })
    
  })
  
  output$datatable <- renderDT({ 
    req(usr$pat)
    
    usr$pat %...>% (function(pat) {
      data <- pat$data[c("datetime", "pm25_A", "pm25_B", "temperature", "humidity")]
      names(data) <- c( "Datetime (UTC)",
                        "PM2.5 Ch. A (\u03bcg / m\u00b)",
                        "PM2.5 Ch. B (\u03bcg / m\u00b)",
                        "Temperature (F)",
                        "Relative Humidity (%)" )
      
      datatable(data, selection = "none", options = list(pageLength = 25) ) %>%
        formatDate(1, method = 'toLocaleString', params = list('en-EN'))
    }) %...!% (function(err) {
      catchError(err)
    })
  
  })
  
}

## To be copied in the UI
# mod_datatable_ui("datatable_ui_1")

## To be copied in the server
# callModule(mod_datatable_server, "datatable_ui_1")

