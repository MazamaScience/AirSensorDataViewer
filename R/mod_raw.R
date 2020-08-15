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
  
  # create loaders
  L1 <- waiter(ns("multiPlot"))
  L2 <- waiter(ns("comparePlot"))
  L3 <- waiter(ns("lmPlot"))
  
  output$multiPlot <- renderPlot({
    req(values$pat)
    L1$show()
    then(values$pat, function(d) {
      pat_multiPlot(pat = d)
    }, onRejected = function(err) {
      logger.error(err)
    })
    
  })
  
  output$comparePlot <- renderPlot({
    req(values$pat)
    L2$show()
    then(values$pat, function(d) {
      asdv_internalFit(pat = d, tz = 'UTC', whichPlot = 'ab')
    }, onRejected = function(err) {
      logger.error(err)
    })
    
  })
  
  output$lmPlot <- renderPlot({
    req(values$pat)
    L3$show()
    then(values$pat, function(d) {
      asdv_internalFit(pat = d, tz = 'UTC', whichPlot = 'lm')
    }, onRejected = function(err) {
      logger.error(err)
    })
  })
 
}
    
## To be copied in the UI
# mod_raw_ui("raw_ui_1")
    
## To be copied in the server
# callModule(mod_raw_server, "raw_ui_1")
 
