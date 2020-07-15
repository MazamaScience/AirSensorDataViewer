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
          DTOutput(
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
mod_patterns_server <- function(input, output, session, values){
  ns <- session$ns
  
  noaa <- reactive({
    req(values$sensor)
    future({ loadNOAA(values$sensor) })
  })
  
  observeEvent(values$tab, {
    if ( values$tab == 'patterns' ) {
      then(noaa(), function(d) { values$noaa <- d })
    }
  })
  
  output$patternPlot <- renderPlot({
    req(values$sensor)
    asdv_pm25Diurnal(ws_data = values$sensor) + 
      stat_meanByHour(output = "scaqmd")
  })
  
  output$noaaTable <- renderDT({
    req(values$noaa)
    datatable(
      data = noaaTable(values$noaa),
      selection = "none",
      colnames = "",
      options = list(dom = 't', bSort = FALSE),
      class = 'cell-border stripe'
    ) %>%
      formatRound(columns = 1, digits = 2)
  })
  
  output$windPlot <- renderPlot({
    req(values$sensor)
    req(values$noaa)
    sensor_pollutionRose(values$sensor, values$noaa)
  })
  
}
    
## To be copied in the UI
# mod_patterns_ui("patterns_ui_1")
    
## To be copied in the server
# callModule(mod_patterns_server, "patterns_ui_1")

