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
mod_datatable_server <- function(input, output, session, obj) {
  ns <- session$ns
  
  output$metatable <- renderTable({
    obj[['data']][['pat']] %...>% {
      data.frame( "Sensor" = .$meta$label,
                  "Community" = .$meta$communityRegion,
                  "Sensor Type" = .$meta$sensorType,
                  "Longitude" = .$meta$longitude,
                  "Latitude" = .$meta$latitude,
                  "State" = .$meta$stateCode,
                  "Country" = .$meta$countryCode ) 
    } %...!%
      catchError()
  })
  
  output$datatable <- renderDT({ 
    obj[['data']][['pat']] %...>% {
      data <- .$data[c("datetime", "pm25_A", "pm25_B", "temperature", "humidity")]
      names(data) <- c( "Datetime (UTC)",
                        "PM2.5 Ch. A (\u03bcg / m\u00b)",
                        "PM2.5 Ch. B (\u03bcg / m\u00b)",
                        "Temperature (F)",
                        "Relative Humidity (%)" )
      
      datatable(data, selection = "none", options = list(pageLength = 25) ) %>%
        formatDate(1, method = 'toLocaleString', params = list('en-EN'))
    } %...!% 
      catchError()

  })
  
}

## To be copied in the UI
# mod_datatable_ui("datatable_ui_1")

## To be copied in the server
# callModule(mod_datatable_server, "datatable_ui_1")

