#' overview UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @importFrom tiotemp timeseriesMapOutput barChartOutput 
mod_overview_ui <- function(id){
  ns <- NS(id)
  tagList(
    timeseriesMapOutput(
      outputId = ns("timeseriesMap"),
      width = "100%", 
      height = "80vh"
    ),
    absolutePanel(
      id = "plot_panel",
      fixed = FALSE,
      left = "auto",
      right = "auto",
      bottom = 0,
      width = "100%",
      height = "inherit",
      # Show/Hide barplot panel button
      HTML('<a id = "collapse_btn" class = "collapsed" data-toggle="collapse" data-target="#dem" style="margin-left:50%;">
           <span class="glyphicon glyphicon-chevron-up"></span> Select a Sensor</a>'),
      # Put barplot in "dem" html
      tags$div(
        id = 'dem',
        class = "collapse",
        barChartOutput(
          outputId = ns("timeseriesBarChart"), 
          height = "20vh"
        )
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
        opacity: 0.70;
        zoom: 0.9;
        transition: opacity 300ms 500ms;
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
  
  # .leaflet-bottom .leaflet-control {
  #   margin-bottom: 352px;
  # }
  # 
}

#' overview Server Function
#'
#' @noRd 
#' @importFrom tiotemp renderTimeseriesMap timeseriesMap 
#' @importFrom tiotemp renderBarChart barChart
mod_overview_server <- function(input, output, session, obj) {
  ns <- session$ns
  
  output$timeseriesMap <- renderTimeseriesMap({
    timeseriesMap(
      data = obj$data$sensors$data, 
      meta = obj$data$sensors$meta, 
      inputId = 'main_panel_ui_1-sensor_select', 
      selected = isolate(obj$selected$sensor)
    )
  })
  
  output$timeseriesBarChart <- renderBarChart({
    barChart(
      data = obj$data$sensors$data,
      meta = obj$data$sensors$meta,
      inputId = 'main_panel_ui_1-sensor_select',
      ylab = "\u03bcg / m\u00b3"
    )
  })
  
  observeEvent(
    ignoreInit = TRUE, 
    eventExpr = {
      obj$selected$sensor
    },
    handlerExpr = {
      plotUp()
    }
  )
  
}

## To be copied in the UI
# mod_overview_ui("overview_ui_1")

## To be copied in the server
# callModule(mod_overview_server, "overview_ui_1")

