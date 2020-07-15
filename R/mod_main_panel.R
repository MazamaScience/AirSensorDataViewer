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
  
  ## ---- Startup ----
  # load non-changing rv: sensors, pas objs
  observe({
    # create sensors obj promise
    sensors <- future({ 
      logger.trace("loading sensors obj...")
      sensor_load() 
    })
    then(sensors, function(d) {
      # update the pickers fulfilled sensor promise
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
      # update sensors rv
      values$sensors <- d 
      logger.trace("sensors done.")
    })
    catch(sensors, function(err) { 
      logger.error(err) 
    })
    # create pas promise
    pas <- future({ 
      logger.trace("loading pas obj...")
      pas_load() 
    })
    then(pas, function(d) { 
      # update pas rv 
      values$pas <- d 
      logger.trace("pas done.")
    })
    catch(pas, function(err) { 
      logger.error(err) 
    })
  })
  
  ## ---- Reactive Expressions ----
  # load pat reactive expression 
  getPat <- reactive({
    req(input$sensor_picker, values$pas)
    ed <- ymd(input$date_picker) + days(1)
    sd <- ed - days(input$lookback_picker) #days(31) #years(1) 
    pat <- future({ 
      logger.trace(paste("loading", input$sensor_picker, "pat obj..."))
      pat_load(
        pas = values$pas, 
        label = input$sensor_picker, 
        startdate = sd, 
        enddate = ed
      ) 
    })
    then(pat, function(d) {
      values$pat <- d 
      logger.trace(paste(input$sensor_picker, "pat done."))
    })
    catch(pat, function(err) {
      logger.error(err)
    })
    # return pat promise for flexibility
    return(pat)
  })
  # filter sensors reactive expression
  getSensor <- reactive({
    req(input$sensor_picker, values$sensors)
    # update the selected sensor reactive data
    sensor <- future({ 
      sensor_filterMeta(values$sensors, .data$label == input$sensor_picker)
    })
    then(sensor, function(d) {
      values$sensor <- d 
      logger.trace(paste(input$sensor_picker, "sensor done."))
    })
    catch(sensor, function(err) {
      logger.error(err)
    })
    # return sensor promise for flexibility
    return(sensor)
  })
  # filter pat obj dates reactive expression
  filterDates <- reactive({
    req(values$pat, input$date_picker, input$lookback_picker)
    # get data time domain 
    data_sd <- ymd_hms(min(values$pat$data$datetime))
    data_ed <- ymd_hms(max(values$pat$data$datetime))
    # get selection time domain
    ed <- ymd(input$date_picker)
    sd <- ed - days(input$lookback_picker)
    # if selection date domain within data date domain, filter pat obj
    # NOTE: this chunk -updates- the pat rv obj
    if ( sd %within% (data_sd %--% data_ed) ) {
      logger.trace(paste(input$sensor_picker, "filter date to", sd, "--", ed))
      values$pat <- pat_filterDate(
        pat = values$pat,
        startdate = strftime(sd, "%Y%m%d"),
        enddate = strftime(ed, "%Y%m%d")
      )
    # otherwise reload pat obj with domain selections
    } else {
      logger.trace(paste(input$sensor_picker, "selected dates", sd, "--", 
                         ed, "out of range, reloading pat obj..."))
      getPat()
      logger.trace(paste(input$sensor_picker, "done."))
    }
    # return start/end dates for flex
    return(c(sd, ed))
  })
  # load latest reactive expression
  getLatest <- reactive({
    req(input$sensor_picker, values$pas)
    latest <- future({ 
      logger.trace(paste("loading latest", input$sensor_picker, "pat obj..."))
      pat_createNew(
        pas = values$pas, 
        label = input$sensor_picker, 
        timezone = 'UTC'
      ) 
    })
    then(latest, function(d) {
      values$latest <- d
      logger.trace(paste("latest", input$sensor_picker, "pat done."))
    })
    catch(latest, function(err) {
      logger.trace(err)
    })
    return(latest)
  })

  ## ---- Event Handling ----
  # load the pat and filter the sensors on sensor selection
  observeEvent(input$sensor_picker, {
    # load the pat obj and filtered sensor from selection label
    getPat()
    getSensor()
  }, priority = 0)
  # filter dates on date || lookback picker change
  observeEvent({ input$date_picker; input$lookback_picker }, {
    filterDates()
  }, priority = 0)
  # filter sensors selection on community selection
  observeEvent(input$community_picker, {
    req(input$community_picker, values$sensors)
    logger.trace(paste("selected community: ", input$community_picker))
    if ( input$community_picker == "All..." ) {
      validSensors <- values$sensors$meta
    } else {
      validSensors <- 
        values$sensors$meta[
          id2com(values$sensors$meta$communityRegion) == input$community_picker,
        ]
    }
    updatePickerInput(session, "sensor_picker", choices = validSensors$label)
  }, priority = 0)
  # load latest pat obj on latest nav && sensor selection
  observeEvent({ input$sensor_picker; values$navbar }, {
    if ( values$navbar == "latest" ) {
      getLatest()
    }
  }, priority = 1) # if on latest page, load latest pat first
  
} # End Server

## To be copied in the UI
# mod_main_panel_ui("main_panel_ui_1")

## To be copied in the server
# callModule(mod_main_panel_server, "main_panel_ui_1")

