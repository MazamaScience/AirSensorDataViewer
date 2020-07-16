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
#' @importFrom worldmet getMeta importNOAA
#' @importFrom lubridate ymd_hms year
#' @importFrom future future availableCores 
#' @importFrom promises then catch
mod_patterns_server <- function(input, output, session, values){
  ns <- session$ns
  
  # create loaders 
  L1 <- waiter(ns("patternPlot"))
  L2 <- waiter(ns("noaaTable"))
  L3 <- waiter(ns("windPlot"))
  
  getNOAA <- reactive({
    req(values$sensor)
    noaa <- future({
      logger.trace("loading NOAA...")
      sd <- min(ymd_hms(values$sensor$data$datetime))
      ed <- max(ymd_hms(values$sensor$data$datetime))
      # Find wind data readings from the closest NOAA site
      year <- year(ed)
      lon <- values$sensor$meta$longitude
      lat <- values$sensor$meta$latitude
      closestSite <- getMeta(lon = lon, lat = lat, n = 1, plot = FALSE)[1,]
      siteCode <- closestSite$code
      siteData <- importNOAA(code = siteCode, year = year, n.cores = availableCores() - 1) 
      return(siteData)
    })
    then(noaa, function(d) {
      values$noaa <- d
      logger.trace("NOAA done.")
    })
    catch(noaa, function(err) {
      logger.error(err)
    })
    return(noaa)
  })
  
  observeEvent({ input$sensor_picker;  values$tab; values$sensor } , {
    if ( values$tab == 'patterns' ) {
      makeWaitress({
        getNOAA()
      }, paste0("Loading NOAA Data..."))
    }
  }, priority = 1)
  
  output$patternPlot <- renderPlot({
    req(values$sensor)
    L1$show()
    asdv_pm25Diurnal(ws_data = values$sensor) + 
      stat_meanByHour(output = "scaqmd")
  })
  
  output$noaaTable <- renderDT({
    req(values$noaa)
    L2$show()
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
    L3$show()
    sensor_pollutionRose(values$sensor, values$noaa)
  })
  
}
    
## To be copied in the UI
# mod_patterns_ui("patterns_ui_1")
    
## To be copied in the server
# callModule(mod_patterns_server, "patterns_ui_1")

