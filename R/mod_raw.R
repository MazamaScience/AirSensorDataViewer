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
    tags$div(
      id = "raw-tab-content",
      fluidRow(
        column(
          width = 6,
            plotOutput(
              outputId = ns("comparePlot")
            ) %>% withLoader() 
        ),
        column(
          width = 6,
            plotOutput(
              outputId = ns("lmPlot")
            ) %>% withLoader() 
        )
      ), 
      tags$hr(),
      column(
        width = 12,
        height = "800",
          plotOutput(
            outputId = ns("multiPlot"),
            height = "800"
          ) %>% withLoader() 
      )
    )
  )
}

#' raw Server Function
#'
#' @noRd 
#' @importFrom ggplot2 theme_light 
#' @importFrom waiter Waiter spin_throbber
#' @importFrom promises `%...>%` `%...!%`
mod_raw_server <- function(input, output, session, usr) {
  ns <- session$ns
   
  timezone <- getOption("asdv.timezone")

  output$multiPlot <- renderPlot({
    req(usr$pat)
    usr$pat %...>% (function(pat) {
      pat_multiPlot(pat, columns = 1, timezone = timezone) 
    }) %...!% (function(err) {
      notify("Failed to load sensor data. Try selecting a different date or a different sensor.")
      catchError(err)
    })
    
  })
  
  output$comparePlot <- renderPlot({
    req(usr$pat)
    
    usr$pat %...>% (function(pat) {
      asdv_internalFit(pat, whichPlot = 'ab', tz = timezone) + theme_light()
    }) %...!% (function(err) {
      catchError(err)
    })
    
  })
  
  output$lmPlot <- renderPlot({
    req(usr$pat)
    
    usr$pat %...>% (function(pat) {
      asdv_internalFit(pat, whichPlot = 'lm', tz = timezone) + theme_light()
    }) %...!% (function(err) {
      catchError(err)
    })
    
  })
  
}

## To be copied in the UI
# mod_raw_ui("raw_ui_1")

## To be copied in the server
# callModule(mod_raw_server, "raw_ui_1")

