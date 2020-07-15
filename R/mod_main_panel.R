#' main_panel UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @importFrom lubridate today ymd days 
#' @importFrom shinyWidgets pickerInput airDatepickerInput radioGroupButtons sliderTextInput
mod_main_panel_ui <- function(id){
  ns <- NS(id)
  
  TZ <- 'UTC'
  
  tagList(
    pickerInput(
      inputId = ns("community_picker"),
      label = tags$h4("Community"),
      choices = "Loading Communites...",
      selected = "",
      options = list(
        `live-search` = TRUE,
        title = "Select community...",
        size = 7 )
    ),
    
    pickerInput(
      inputId = ns("sensor_picker"),
      label = tags$h4("Sensor"),
      choices = "Loading Sensors...",
      selected = "",
      options = list(
        `live-search` = TRUE,
        title = "Select sensor...",
        size = 7 )
    ),
    
    airDatepickerInput(
      inputId = ns("date_picker"),
      label = tags$h4("Date"),
      value = today(tzone = TZ) - days(1),
      todayButton = TRUE,
      addon = "none",
      inline = FALSE,
      separator = " to ",
      range = FALSE,
      maxDate = today(tzone = TZ) - days(1),
      minDate = ymd(20180101, tz = TZ)
    ),
    
    sliderTextInput(
      inputId = ns("lookback_picker"), 
      label = tags$h4("View Past Days"),
      choices = seq(from = 2, to = 30, by = 1), 
      grid = TRUE, 
      selected = 5
      
    )
    
    # radioGroupButtons(
    #   inputId = ns("lookback_picker"),
    #   label = tags$h4("View Past"),
    #   choices = c( "3 Days" = 3,
    #                "7 Days" = 7,
    #                "15 Days" = 15,
    #                "30 Days" = 30 ),
    #   justified = T,
    #   direction = "vertical",
    #   individual = F,
    #   checkIcon = list(
    #     yes = tags$i(class = "fa fa-check",
    #                  style = "color: #008cba"))
    # )
    
  )
}

#' main_panel Server Function
#'
#' @noRd 
#' 
#' @import AirSensor
#' @import MazamaCoreUtils
#' @importFrom future future
#' @importFrom promises then catch
#' @importFrom lubridate ymd ymd_hms days years %within% %--% 
#' @importFrom shinyWidgets updatePickerInput
#' @importFrom PWFSLSmoke createEmptyMonitor
#' @importFrom rlang .data
mod_main_panel_server <- function(input, output, session, values){
  ns <- session$ns
  
  # SCAQMD sensors
  setArchiveBaseUrl("http://data.mazamascience.com/PurpleAir/v1") 
  
  ## sensors future
  sensors <- reactive({
    future({ sensor_load() })
  })
  
  # Add the promised sensor obj to reactive values when loaded
  observeEvent(sensors(), {
    logger.trace("loading sensor obj...")
    then(sensors(), function(d) { values$sensors <- d })
    catch(sensors(), function(e) { logger.error(e) })
    logger.trace("done.")
  })
  
  ## pas future
  pas <- reactive({
    future({ pas_load() })
  })
  
  # Add the promised pas obj to reactive values when loaded
  observeEvent(pas(), {
    logger.trace("loading pas obj...")
    then(pas(), function(d) { values$pas <- d } )
    catch(pas(), function(e) { logger.error(e) })
    logger.trace("done.")
  })
  
  ## pat obj future
  pat <- reactive({
    # Require a sensor selection
    req(input$sensor_picker) 
    # Require pas 
    req(values$pas)
    ed <- ymd(input$date_picker) + days(1)
    sd <- ed - days(input$lookback_picker) #days(31) #years(1) # Default to load 31 days
    future({ 
      pat_load(
        pas = values$pas, 
        label = input$sensor_picker, 
        startdate = sd, 
        enddate = ed
      ) 
    })
  })
  
  # Add the promised pat obj to reactive values when loaded
  observeEvent(pat(), {
    logger.trace(paste("loading", input$sensor_picker, "pat obj...")) 
    then(pat(), function(d) { values$pat <- d })
    catch(pat(), function(e) { logger.error(e) })
    logger.trace(paste(input$sensor_picker, "done."))
  })
  
  ## latest pat obj future
  latest <- reactive({
    req(input$sensor_picker)
    req(values$pas)
    future({
      pat_createNew(
        pas = values$pas, 
        label = input$sensor_picker, 
        timezone = 'UTC'
      ) 
    })
  })
  
  # Add the promised latest pat obj to reactive values when on latest navbar tab
  observeEvent({ values$nav; input$sensor_picker }, {
    if ( values$nav == 'latest' ) {
      req(input$sensor_picker)
      logger.trace(paste("loading latest", input$sensor_picker, "pat obj..."))
      then(latest(), function(d) { values$latest <- d })
      catch(latest(), function(e) { logger.error(e) })
      logger.trace(paste(input$sensor_picker, "latest done."))
    }
  })
  
  # Update the pickers when sensor promise is fulfilled
  observeEvent(sensors(), {
    then(sensors(), function(d) {
      updatePickerInput(
        session, 
        "community_picker", 
        choices = c("All...", unique(id2com(d$meta$communityRegion)))
      )
      updatePickerInput(
        session, 
        "sensor_picker", 
        choices = d$meta$label
      )
    })
  })
  
  # Update the available sensors in selected community
  observeEvent(input$community_picker, {
    req(input$community_picker)
    then(sensors(), function(d) {
      logger.trace(paste("selected community: ", input$community_picker))
      if ( input$community_picker == "All..." ) {
        community_sensors <- d$meta
      } else {
        community_sensors <- 
          d$meta[id2com(d$meta$communityRegion) == input$community_picker,]
      }
      updatePickerInput(
        session, 
        "sensor_picker", 
        choices = community_sensors$label
      )
    })
  })
  
  # Update the reactive sensor on input selection
  observeEvent(input$sensor_picker, {
    req(input$sensor_picker)
    logger.trace(paste("selected sensor: ", input$sensor_picker))
    # Update the selected sensor reactive data
    values$sensor <- 
      sensor_filterMeta(values$sensors, .data$label == input$sensor_picker)
  })
  
  # Update the reactive pat values on date range changes
  observeEvent({ input$date_picker; input$lookback_picker } , {
    req(values$pat)
    data_sd <- ymd_hms(min(values$pat$data$datetime))
    data_ed <- ymd_hms(max(values$pat$data$datetime))
    
    ed <- ymd(input$date_picker)
    sd <- ed - days(input$lookback_picker)
    
    if ( sd %within% (data_sd %--% data_ed) ) {
      logger.trace(paste(input$sensor_picker, "filter date to", sd, "--", ed))
      values$pat <- pat_filterDate(
        pat = values$pat,
        startdate = strftime(sd, "%Y%m%d"),
        enddate = strftime(ed, "%Y%m%d")
      )
    } else {
      logger.trace(paste(input$sensor_picker, "selected dates", sd, "--", 
                         ed, "out of range, reloading pat obj..."))
      then(pat(), function(d) {
        values$pat <- d
        logger.trace(paste(input$sensor_picker, "done."))
      })
    }
  })
  
}

## To be copied in the UI
# mod_main_panel_ui("main_panel_ui_1")

## To be copied in the server
# callModule(mod_main_panel_server, "main_panel_ui_1")

