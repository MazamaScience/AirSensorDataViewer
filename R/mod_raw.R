#' raw UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_raw_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        width = 12,
        height = "800",
        tags$h4("Raw Data"),
        wellPanel(
          plotOutput(
            outputId = ns("multiPlot"),
            height = "800"
          ) 
        )
      )
    ),
    fluidRow(
      column(
        width = 6,
        tags$h4("Channel Overlay"),
        wellPanel(
          plotOutput(
            outputId = ns("comparePlot")
          ) 
        )
      ),
      column(
        width = 6,
        tags$h4("Channel Correlation"),
        wellPanel(
          plotOutput(
            outputId = ns("lmPlot")
          ) 
        )
      )
    )
  )
}
    
#' raw Server Function
#'
#' @noRd 
mod_raw_server <- function(input, output, session, values){
  ns <- session$ns

  output$multiPlot <- renderPlot({
    req(values$pat)
    pat_multiPlot(pat = values$pat)
  })
  
  output$comparePlot <- renderPlot({
    req(values$pat)
    asdv_internalFit(pat = values$pat, tz = 'UTC', whichPlot = 'ab')
  })
  
  output$lmPlot <- renderPlot({
    req(values$pat)
    asdv_internalFit(pat = values$pat, tz = 'UTC', whichPlot = 'lm')
  })
 
}
    
## To be copied in the UI
# mod_raw_ui("raw_ui_1")
    
## To be copied in the server
# callModule(mod_raw_server, "raw_ui_1")
 
