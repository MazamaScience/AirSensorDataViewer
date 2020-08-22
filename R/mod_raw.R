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
mod_raw_server <- function(input, output, session, obj){
  ns <- session$ns
  
  observeEvent(
    ignoreInit = TRUE,
    eventExpr = {
      obj$selected$sensor
      obj$selected$sd
      obj$selected$ed
      obj$selected$page
    },
    handlerExpr = {
      if ( obj$selected$page != 'latest' ) {
        obj$updatePat(
          pas = obj$data$pas, 
          label = obj$selected$sensor, 
          sd = obj$selected$sd, 
          ed = obj$selected$ed
        )
      }
    }
  )
  
  output$multiPlot <- renderPlot({
    pat_multiPlot(obj$data$pat)
  })
  
  output$comparePlot <- renderPlot({
    asdv_internalFit(pat = obj$data$pat, tz = 'UTC', whichPlot = 'ab') +
      ggplot2::theme_light()
    
  })
  
  output$lmPlot <- renderPlot({
    asdv_internalFit(pat = obj$data$pat, tz = 'UTC', whichPlot = 'lm') +
      ggplot2::theme_light()
    
  })
  
}
    
## To be copied in the UI
# mod_raw_ui("raw_ui_1")
    
## To be copied in the server
# callModule(mod_raw_server, "raw_ui_1")
 
