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
mod_main_panel_ui <- function(id) {
  ns <- NS(id)
  TZ <- 'UTC'
  tagList(
    
    selectizeInput(
      inputId = ns("community_select"),
      label = tags$h4("Community"), 
      selected = NULL,
      choices = "Loading Communities..."
    ),
    
    selectizeInput(
      inputId = ns("sensor_select"),
      label = tags$h4("Sensor"), 
      selected = NULL,
      choices = "Loading Sensors..."
    ),
    
    dateRangeInput(
      inputId = ns("date_range"), 
      label = tags$h4("Date Range"), 
      start = (today(tzone = TZ) - days(1)) - days(7), 
      end = (today(tzone = TZ) - days(1)), 
      min = ymd(20171001), 
      max = (today(tzone = TZ) - days(1))
    )
    
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
#' @importFrom waiter Waitress
#' @importFrom stats na.omit 
mod_main_panel_server <- function(input, output, session, values) {
  
  ns <- session$ns
  # SCAQMD sensors
  setArchiveBaseUrl("http://data.mazamascience.com/PurpleAir/v1") 
  
  ## ---- Startup ----
  
  # load non-changing rv: sensors, pas objs
  observeEvent(
    once = TRUE,
    eventExpr = {
    values$init
    }, 
    handlerExpr = {
    # notification
    startupWaitress <- waitress()
    startupWaitress$notify(html = tags$h4("Loading Data..."), position = "bl")
    
    ##  create sensors obj promise on startup 
    values$sensors <- future({ 
      get_sensors(sd = input$date_range[1], ed = input$date_range[2])
    })
    startupWaitress$set(10)
    
    then(values$sensors, function(d) {
      # Check diff bewteen sensors aval in sensor obj and pas obj and only use 
      # the sensors with mutual existence
      then(values$pas, function(p) {
        communities <- na.omit(unique(id2com(d$meta$communityRegion[d$meta$label %in% p$label])))
        sensor_labels <- na.omit(unique(d$meta$label[d$meta$label %in% p$label]))
        
        # update sensors rv
        #values$sensors <- d 
        # Fill the community selection
        updateSelectizeInput( 
          session,
          inputId = "community_select", 
          selected = "All...",
          choices = c("Choose a community" = "","All...", communities)
        )
        # Fill the sensor selection
        updateSelectizeInput(
          session, 
          inputId = "sensor_select", 
          choices = sensor_labels
        )
      })

      startupWaitress$set(50)
      logger.trace("sensors done.")
    })
    
    catch(values$sensors, function(err) { 
      logger.error(err) 
    })
    
    ## create pas promise
    values$pas <- future({ 
      get_pas()
    })
    startupWaitress$set(75)
    then(values$pas, function(d) { 
      # update pas rv 
      #values$pas <- d 
      startupWaitress$set(90)
      logger.trace("pas done.")
    })
    catch(values$pas, function(err) { 
      logger.error(err) 
    })
    startupWaitress$close()
  })
  
  ## ---- Event Handling ----
  
  # update the sensor selections on community selection
  observeEvent(
    eventExpr = {
      input$community_select
    }, 
    handlerExpr = {
      req(values$sensors, input$community_select)
      then(values$sensors, function(d) {
        if ( input$community_select == "All..." ) {
          validSensors <- d$meta
        } else {
          validSensors <-
            d$meta[id2com(d$meta$communityRegion) ==
                     input$community_select,]
        }
        updateSelectizeInput(
          session,
          "sensor_select",
          choices = na.omit(validSensors$label)
        )
      })
    }, ignoreNULL = TRUE, ignoreInit = TRUE)
  
  observeEvent(
    eventExpr = {
      input$sensor_select
    }, 
    handlerExpr = {
      req(input$sensor_select)
      
      # store sensor selection label as string for scoping
      values$sensor_select <- input$sensor_select
      # filter/download a new sensors
      values$sensor <- then(values$sensors, function(d) {
        future({
          get_sensor(
            sensors = d,
            .data$label == input$sensor_select
          )
        })
      })
      
      
      
      values$pat <- then(values$pas, function(d) {
        future({
          get_pat(
            pas = d, 
            label = input$sensor_select, 
            sd = input$date_range[1], 
            ed = input$date_range[2]
          )
        })
      }, onRejected = function(err) {
        print("NO!")
        logger.error(err)
      })
      
      # Update the selector on map point click, done in backend by tiotemp,
      # but value change must be watched 
      updateSelectizeInput(
        session, 
        "sensor_select", 
        selected = input$sensor_select
      )
      
    }, ignoreNULL = TRUE, ignoreInit = TRUE
  )
  
  # update the sensor and pat rv on sensor selection
  observeEvent(
    eventExpr = {
      #input$sensor_select
      input$date_range
    },
    handlerExpr = {
      req(values$sensors, values$pas)
      
      # if the user selects an enddate that is before or on the startdate, make
      # the startdate a day before the enddate to avoid weird stuff
      if ( ymd(input$date_range[2]) <= ymd(input$date_range[1]) ) {
        updateDateRangeInput(
          session,
          "date_range",
          start = ymd(input$date_range[2]) - days(1)
        )
      }
      if ( ymd(input$date_range[2]) - ymd(input$date_range[1]) > 31 ) {
        updateDateRangeInput(
          session,
          "date_range",
          start = ymd(input$date_range[2]) - days(31)
        )
        showNotification("Dates range too long!", "Max date range is 31 days.", type = "warn")
      }
      # establish startdate and endate rv
      values$sd <- input$date_range[1]
      values$ed <- input$date_range[2]
      
      # filter/download a new sensors
      then(values$sensors, function(d) {
        values$sensor <- future({
          get_sensor(
            sensors = d,
            label = input$sensor_select,
            sd = input$date_range[1],
            ed = input$date_range[2]
          )
        })
      })
      
      then(values$pas, function(d) {
        values$pat <- future({
          get_pat(
            pas = d, 
            label = input$sensor_select, 
            sd = input$date_range[1], 
            ed = input$date_range[2]
          )
        })
      })
      
      ##  create sensors obj promise on startup 
      values$sensors <- future({ 
        get_sensors(sd = input$date_range[1], ed = input$date_range[2])
      })
    }, ignoreNULL = TRUE, ignoreInit = TRUE)


  # 
  # # Attempt filter/load on date change
  # observeEvent(
  #   eventExpr = {
  #     input$date_range
  #   }, 
  #   handlerExpr = {
  #     req(input$sensor_select, values$pas, values$pat)
  #     # if the user selects an enddate that is before or on the startdate, make 
  #     # the startdate a day before the enddate to avoid weird stuff
  #     if ( ymd(input$date_range[2]) <= ymd(input$date_range[1]) ) {
  #       updateDateRangeInput(
  #         session, 
  #         "date_range", 
  #         start = ymd(input$date_range[2]) - days(1)
  #       )
  #     } 
  #     if ( ymd(input$date_range[2]) - ymd(input$date_range[1]) > 31 ) {
  #       updateDateRangeInput(
  #         session, 
  #         "date_range", 
  #         start = ymd(input$date_range[2]) - days(31)
  #       )
  #       showNotification("Dates range too long!", "Max date range is 31 days.", type = "warn")
  #     }
  #     # establish startdate and endate rv 
  #     values$sd <- input$date_range[1]
  #     values$ed <- input$date_range[2]
  #     # filter/download a new sensors
  #     values$sensors <- get_sensors(
  #       sd = input$date_range[1], 
  #       ed = input$date_range[2], 
  #       sensors = values$sensors
  #     )
  #     values$pat <- get_pat(
  #       pas = values$pas,
  #       label = input$sensor_select, 
  #       sd = input$date_range[1],
  #       ed = input$date_range[2], 
  #       pat = values$pat
  #     )
  #   }, 
  #   priority = 1
  # )
  # 
  # # Save loading for specific navbar pages
  observeEvent(
    eventExpr = {
      values$navbar
    },
    handlerExpr = {
      if (values$navbar == 'latest') {
        
      then(values$pas, function(d){
        values$pat_latest <- future({
          get_pat_latest(
            pas = d,
            label = input$sensor_select
          )
        })
      })

        # tryCatch(
        #   expr = {
        #     makeWaitress({
        #       values$pat_latest <- get_pat_latest(
        #         values$pas,
        #         input$sensor_select
        #       )
        #     }, tags$h4(paste0("Loading latest ", input$sensor_select, "...")))
        #   },
        #   error = function(err) {
        #     logger.error(err)
        #     showNotification("Oops!", "An Error has occured.", type = "warn")
        #   }
        # )
      }
    }
  )
  # 
  # # Watch for map click updates on the input from tiotemp
  # observeEvent(
  #   eventExpr = {
  #     input$sensor_select
  #   }, 
  #   handlerExpr = {
  #     updateSelectizeInput(
  #       session,
  #       "sensor_select",
  #       selected = input$sensor_select
  #     )
  #     values$sensor_select <- input$sensor_select
  #   }, 
  #   priority = 1, 
  #   ignoreNULL = TRUE
  # )

} # End Server

## To be copied in the UI
# mod_main_panel_ui("main_panel_ui_1")

## To be copied in the server
# callModule(mod_main_panel_server, "main_panel_ui_1")

