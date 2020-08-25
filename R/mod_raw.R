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
#' @importFrom promises `%...>%` `%...!%`
mod_raw_server <- function(input, output, session, obj){
  ns <- session$ns
  
  observeEvent(
    ignoreNULL = TRUE, 
    ignoreInit = TRUE,
    eventExpr = {
      obj[['selected']][['sensor']]
      obj[['selected']][['sd']]
      obj[['selected']][['ed']]
      obj[['selected']][['page']]
    },
    handlerExpr = {
      if ( obj[['selected']][['page']] != 'latest' ) {
        obj[['updatePat']](
          pas = obj[['data']][['pas']], 
          label = obj[['selected']][['sensor']], 
          sd = obj[['selected']][['sd']], 
          ed = obj[['selected']][['ed']]
          #pat = obj[['data']][['pat']]
        )
      }
    }
  )
  
  output[['multiPlot']] <- renderPlot({
    req(obj[['data']][['pat']])
    obj[['data']][['pat']] %...>% {
      pat_multiPlot(.)
    } %...!% catchError(.)
  })
  
  output[['comparePlot']] <- renderPlot({
    req(obj[['data']][['pat']])
    obj[['data']][['pat']] %...>% {
      asdv_internalFit(pat = .,tz = 'UTC', whichPlot = 'ab') +
        ggplot2::theme_light()
    } %...!% catchError(.)
  })
  
  output[['lmPlot']] <- renderPlot({
    req(obj[['data']][['pat']])
    obj[['data']][['pat']] %...>% {
      asdv_internalFit(pat = .,tz = 'UTC', whichPlot = 'lm') +
        ggplot2::theme_light()
    } %...!% catchError(.)
  })
  
}

## To be copied in the UI
# mod_raw_ui("raw_ui_1")

## To be copied in the server
# callModule(mod_raw_server, "raw_ui_1")

