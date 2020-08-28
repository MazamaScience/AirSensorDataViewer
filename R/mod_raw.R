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
#' @importFrom ggplot2 theme_light 
#' @importFrom waiter Waiter spin_throbber
mod_raw_server <- function(input, output, session, tc){
  ns <- session$ns
  w <- Waiter$new(
    c(ns("multiPlot"), ns("comparePlot"), ns("lmPlot")), 
    spin_throbber(), 
    color = "#F8F8F8"
  )
  
  output$multiPlot <- renderPlot({
    #req(obj[['data']][['pat']])
    w$show()
    pat <- tc$pat#obj[['data']][['pat']]
    tryCatch(
      expr = {
        pat_multiPlot(pat)
      }, 
      error = function(err) {
        logger.error(err)
        NULL
      }
    )
    
  })
  
  output$comparePlot <- renderPlot({
    pat <- tc$pat#obj[['data']][['pat']]
    tryCatch( 
      expr = {
        asdv_internalFit(pat,tz = 'UTC', whichPlot = 'ab') + theme_light()
      }, 
      error = function(err) {
        logger.error(err)
        NULL
      }
    )
  })
  
  output$lmPlot <- renderPlot({
    pat <- tc$pat#obj[['data']][['pat']]
    tryCatch(
      expr = {
        asdv_internalFit(pat,tz = 'UTC', whichPlot = 'lm') + theme_light()
      }, 
      error = function(err) {
        logger.error(err)
        NULL
      }
    )
  })
  
}

## To be copied in the UI
# mod_raw_ui("raw_ui_1")

## To be copied in the server
# callModule(mod_raw_server, "raw_ui_1")

