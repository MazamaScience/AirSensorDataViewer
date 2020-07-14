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
mod_datatable_server <- function(input, output, session, values) {
  ns <- session$ns
  
  output$metatable <- renderTable({
    req(values$pat)
    data.frame( "Sensor" = values$pat$meta$label,
                "Community" = values$pat$meta$communityRegion,
                "Sensor Type" = values$pat$meta$sensorType,
                "Longitude" = values$pat$meta$longitude,
                "Latitude" = values$pat$meta$latitude,
                "State" = values$pat$meta$stateCode,
                "Country" = values$pat$meta$countryCode )
  })
  
  output$datatable <- renderDT({ 
    req(values$pat)
    data <- values$pat$data[-(6:10)]
    names(data) <- c( "Datetime (UTC)",
                      "PM2.5 Ch. A (\u03bcg / m\u00b)",
                      "PM2.5 Ch. B (\u03bcg / m\u00b)",
                      "Temperature (F)",
                      "Relative Humidity (%)" )
    
    datatable(data, selection = "none", options = list(pageLength = 25) ) %>%
      formatDate(1, method = 'toLocaleString', params = list('en-EN'))
  })
  
}
    
## To be copied in the UI
# mod_datatable_ui("datatable_ui_1")
    
## To be copied in the server
# callModule(mod_datatable_server, "datatable_ui_1")
 
