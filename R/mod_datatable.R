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
      ) %>% withLoader()
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
      data.frame( "Sensor Name" = pat$meta$label,
                  "Community" = pat$meta$communityRegion,
                  "Sensor Model" = "PA-II (PurpleAir)",#pat$meta$sensorType # replaced @ req issue #24 by SCAQMD
                  "Longitude" = pat$meta$longitude,
                  "Latitude" = pat$meta$latitude,
                  "State" = pat$meta$stateCode,
                  "Country" = pat$meta$countryCode, check.names = FALSE )  
    }) %...!% (function(err) {
      catchError(err)
    })
    
  })
  
  output$datatable <- renderDT({ 
    req(usr$pat)
    
    usr$pat %...>% (function(pat) {
      data <- pat$data[c("datetime", "pm25_A", "pm25_B", "temperature", "humidity")]
      names(data) <- c( "Datetime (UTC)",
                        "PM2.5 Ch. A (\u03bcg / m\u00b3)",
                        "PM2.5 Ch. B (\u03bcg / m\u00b3)",
                        "Temperature (F)",
                        "Relative Humidity (%)" )
      
      datatable(
        data, 
        selection = "none", 
        extensions = 'Scroller', 
        options = list(
          deferRender = TRUE, 
          scrollY = 624, 
          scroller = TRUE, 
          dom = 't', 
          pageLength = 25, 
          columnDefs = list(list(className = 'dt-center', targets = "_all"))
          )
        ) %>%
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

