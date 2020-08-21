#' compare UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @importFrom leaflet leafletOutput
#' @importFrom DT DTOutput
mod_compare_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        width = 12,
        wellPanel(
          leafletOutput(
            outputId = ns("comparisonLeaflet")
          )
        )
      )
    ),
    fluidRow(
      column(
        width = 5,
        tags$h4("Sensor Status"),
        wellPanel(
          DTOutput(
            outputId = ns("statusTable")
          )
        ),
        tags$h4("Sensor-Monitor Correlation"),
        wellPanel(
          plotOutput(
            outputId = ns("sensorMonitorCorr")
          ) 
        )
      ),
      column(
        width = 7,
        tags$h4("Sensor-Monitor Comparison"),
        wellPanel(
          plotOutput(
            outputId = ns("sensorMonitorComp")
          ) 
        )
      )
    )
  )
  
}

#' compare Server Function
#'
#' @noRd 
#' 
#' @importFrom DT datatable formatRound renderDT
#' @import leaflet
#' @import ggplot2
mod_compare_server <- function(input, output, session, values) {
  ns <- session$ns
  
  observeEvent(
    eventExpr = {
      values$tab
    }, 
    handlerExpr = {
      req(values$sensor)
      if (values$tab == 'compare' ) {
        values$pwfsl <- future({then(values$sensor, function(d) {
          dates <- range(d$data$datetime)
          mlab <- d$meta$pwfsl_closestMonitorID
          PWFSLSmoke::monitor_load(startdate = dates[1], enddate = dates[2], monitorIDs = mlab)
        })
        })
      }
    } 
  )
  
  output$comparisonLeaflet <- renderLeaflet({
    req(values$sensor)
    req(values$pwfsl)
      then(values$sensor, function(d) {
        then(values$pwfsl, function(h) {
          
          slab <- d$meta$label
          mdist <- signif(d$meta$pwfsl_closestDistance/1000, 2)
          mlab <- d$meta$pwfsl_closestMonitorID
          sensor <- PWFSLSmoke::monitor_toTidy(d)
          dates <- range(d$data$datetime)
          monitor <- PWFSLSmoke::monitor_toTidy(h)
          df <- dplyr::left_join(sensor, monitor, by = 'datetime', suffix = c('.pwfsl', '.pa'))
          
          leaflet(df) %>% 
            addTiles() %>% 
            addAwesomeMarkers(
              lng = ~unique(longitude.pwfsl), 
              lat = ~unique(latitude.pwfsl), 
              icon = makeAwesomeIcon(markerColor = "gray", icon = 'flag'), 
              popup = mlab
            ) %>% 
            addAwesomeMarkers(
              lng = ~unique(longitude.pa), 
              lat = ~unique(latitude.pa), 
              icon = makeAwesomeIcon(markerColor = "purple", icon = 'flag'), 
              popup = slab
            ) %>% 
            addPolylines(
              lng = c(unique(df$longitude.pwfsl), unique(df$longitude.pa)), 
              lat = c(unique(df$latitude.pwfsl), unique(df$latitude.pa)),
              dashArray = "12", 
              color = 'red', 
              popup = paste0(mdist, " km")
            )
        })
      })
  })
  
  output$sensorMonitorCorr <- renderPlot({
    req(values$sensor)
      then(values$sensor, function(d) {
        
        slab <- d$meta$label
        mlab <- d$meta$pwfsl_closestMonitorID
        sensor <- PWFSLSmoke::monitor_toTidy(d)
        dates <- range(sensor$datetime)
        monitor <- PWFSLSmoke::monitor_toTidy(PWFSLSmoke::monitor_load(startdate = dates[1], enddate = dates[2], monitorIDs = mlab))
        
        df <- dplyr::left_join(sensor, monitor, by = 'datetime', suffix = c('.pwfsl', '.pa'))
        
        dataMin <- min(c(df$pm25.pa, df$pm25.pwfsl), na.rm = TRUE)
        dataMax <- max(c(df$pm25.pa, df$pm25.pwfsl), na.rm = TRUE)
        xylim <- c(dataMin, dataMax)
        
        model <- lm(df$pm25.pa ~ df$pm25.pwfsl, subset = NULL,
                    weights = NULL, na.action = 'na.omit')
        
        slope <- as.numeric(model$coefficients[2])      # as.numeric() to remove name
        intercept <- as.numeric(model$coefficients[1])
        r_squared <- summary(model)$r.squared
        
        # # Label for linear fit
        equationLabel <-
          ggplot2::annotate(
            geom = "text",
            x = 0.75 * xylim[2],
            y = c(0.25, 0.15, 0.05) * xylim[2],
            label = c(paste0("Slope = ", round(slope, digits = 2)),
                      paste0("Intercept = ", round(intercept, digits = 1)),
                      paste0("R\U00B2 = ", round(r_squared, digits = 3))) )
        
        print(str(df))
        
        ggplot(df, aes(x = pm25.pa, y = pm25.pwfsl)) +   
          geom_point(color = 'black', shape = 15, alpha = 0.2, size = 1) +
          geom_smooth(method = "lm", se = FALSE, color = 'red', alpha = 0.3) + 
          
          xlim(xylim) +
          ylim(xylim) +
          xlab(slab) + 
          ylab(mlab) +
          theme_light() + 
          coord_fixed() + 
          equationLabel
        
      })
  })
  
  output$sensorMonitorComp <- renderPlot({
    req(values$pat)
      then(values$pat, function(d) {
        pat_monitorComparison(d)
      })
  })
  
  # output$statusTable <- renderDT({
  #   req(values$pat)
  #   then(values$pat, function(d) {
  #     datatable(
  #       data = sensorMonitorCompTable(values$pat), 
  #       selection = "none",
  #       colnames = "",
  #       options = list(dom = 't', bSort = FALSE),
  #       class = 'cell-border stripe'
  #     ) %>%
  #       formatRound(columns = 1, digits = 2)
  #   })
  # })
  
}

## To be copied in the UI
# mod_compare_ui("compare_ui_1")

## To be copied in the server
# callModule(mod_compare_server, "compare_ui_1")

