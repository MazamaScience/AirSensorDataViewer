#' overview UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @importFrom tiotemp timeseriesMapOutput timeseriesBarChartOutput 
mod_overview_ui <- function(id){
  ns <- NS(id)
  tagList(
    timeseriesMapOutput(
      outputId = ns("timeseriesMap"),
      width = "inherit", 
      height = "80vh"
    ) %>% withLoader(),
    absolutePanel(
      id = "plot_panel",
      fixed = FALSE,
      left = "auto",
      right = "auto",
      bottom = 0,
      width = "inherit",
      height = "inherit",
      # Show/Hide barplot panel button
      HTML('<a id = "collapse_btn" class = "collapsed" data-toggle="collapse" data-target="#dem" style="margin-left:50%;">
           <span class="glyphicon glyphicon-chevron-up"></span> Select a Sensor</a>'),
      # Put barplot in "dem" html
      tags$div(
        id = 'dem',
        class = "collapse",
        timeseriesBarChartOutput(
          outputId = ns("timeseriesBarChart"), 
          height = "20vh"
        ) %>% withLoader()
      )
    ),
    # Barplot panel opacity CSS and leaflet padding fix
    tags$style(
      type = "text/css",
      '#plot_panel{
        /* Appearance */
        background-color: white;
        padding: 0 0 0 0;
        cursor: move;
        /* Fade out while not hovering */
        opacity: 0.95;
        zoom: 1;
        transition: opacity 200ms 400ms;
      }
      #plot_panel:hover {
        /* Fade in while hovering */
        opacity: 1;
        transition-delay: 0;
      }
      .col-sm-12{
        padding: 0 0 0 0;
      }'
    )
  )

}

#' overview Server Function
#'
#' @noRd 
#' @importFrom tiotemp renderTimeseriesMap timeseriesMap 
#' @importFrom tiotemp renderTimeseriesBarChart timeseriesBarChart
#' @importFrom promises `%...>%` `%...!%`
mod_overview_server <- function(input, output, session, usr) {
  ns <- session$ns
  
  output$timeseriesMap <- renderTimeseriesMap({
    req(usr$sensors)
    selected <- isolate(usr$selected$sensor)
    sensors <- usr$sensors
    
    usr$sensors %...>% (function(sensors) {
      timeseriesMap(
        data = sensors[['data']], 
        meta = sensors[['meta']], 
        inputId = 'main_panel_ui_1-sensor_select', 
        selected = selected
      )
    }) %...!% (function(err) {
      catchError(err)
    })
    
  })
  
  output$timeseriesBarChart <- renderTimeseriesBarChart({
    req(usr$sensors)
    
    usr$sensors %...>% (function(sensors) {
      timeseriesBarChart(
        data = sensors[['data']], 
        meta = sensors[['meta']],
        inputId = 'main_panel_ui_1-sensor_select',
        ylab = "\u03bcg / m\u00b3"
      ) 
    }) %...!% (function(err) {
      notify("Failed to load sensor data. Try selecting a different date or a different sensor.")
      catchError(err)
    })
    
  })
  
}

## To be copied in the UI
# mod_overview_ui("overview_ui_1")

## To be copied in the server
# callModule(mod_overview_server, "overview_ui_1")

