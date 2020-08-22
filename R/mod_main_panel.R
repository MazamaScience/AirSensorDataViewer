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
#' @importFrom future future resolved
#' @importFrom promises then catch
#' @importFrom lubridate ymd ymd_hms days years %within% %--% 
#' @importFrom shinyWidgets updatePickerInput
#' @importFrom PWFSLSmoke createEmptyMonitor
#' @importFrom rlang .data
#' @importFrom waiter Waitress
#' @importFrom stats na.omit 
mod_main_panel_server <- function(input, output, session, obj, selected) {
  ns <- session$ns
  
  # startupWaitress <- waitress()
  # startupWaitress$notify(html = tags$h4("Loading Data..."), position = "bl")
  # startupWaitress$set(20)
  
  observeEvent(
    ignoreNULL = TRUE,
    once = TRUE, 
    eventExpr = {
      obj$data$sensors
    },
    handlerExpr = {
      # Check diff bewteen sensors aobj in sensor obj and pas obj and only use
      # the sensors with mutual existence
      communities <- na.omit(unique(id2com(obj$data$sensors$meta$communityRegion)))
      sensor_labels <- na.omit(unique(obj$data$sensors$meta$label))
      
      # update sensors rv
      #objues$sensors <- d
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
    }
  )
  
  observeEvent(
    ignoreNULL = TRUE,
    ignoreInit = TRUE,
    eventExpr = {
      input$sensor_select
    }, 
    handlerExpr = {
      # Throws a freaking loop
      # updateSelectizeInput(
      #   session,
      #   inputId = "sensor_select",
      #   selected = input$sensor_select
      # )
      obj$selected$sensor <- input$sensor_select
    }
  )
  
  observeEvent(
    ignoreNULL = TRUE,
    eventExpr = {
      input$date_range
    }, 
    handlerExpr = {
      
      obj$updateSensors(input$date_range[1], input$date_range[2])
      
      obj$selected$sd <- input$date_range[1]
      obj$selected$ed <- input$date_range[2]
      
    }
  )
  
  observeEvent(
    ignoreInit = TRUE, 
    eventExpr = {
      input$community_select
    }, 
    handlerExpr = {
      obj$selected$community <- input$community_select
    }
  )
  
  # # Create pas future 
  # # TODO: Implement dates.
  # objues$pas <- future({
  #   setArchiveBaseUrl("http://data.mazamascience.com/PurpleAir/v1") # SCAQMD sensors
  #   get_pas()
  # })
  # 
  # # Create sensors future
  # objues$sensors <- future({
  #   setArchiveBaseUrl("http://data.mazamascience.com/PurpleAir/v1") # SCAQMD sensors
  #   get_sensors(isolate(input$date_range[1]), isolate(input$date_range[2]))
  # })
  # 
  # print(ff$sensors)
  # 
  # observeEvent(
  #   ignoreInit = TRUE,
  #   eventExpr = {
  #     input$community_select
  #   }, 
  #   handlerExpr = {
  #     if ( input$community_select != 'All...' ) {
  #       then(objues$sensors, function(d) {
  #         choices <- d$meta[id2com(d$meta$communityRegion) == input$community_select,]
  #         updateSelectizeInput(
  #           session,
  #           "sensor_select",
  #           choices = na.omit(choices$label)
  #         )
  #       })
  #     } else {
  #       then(objues$sensors, function(d) {
  #         choices <- d$meta
  #         updateSelectizeInput(
  #           session,
  #           "sensor_select",
  #           choices = na.omit(choices$label)
  #         )
  #       })
  #     }
  #   }
  # )
  # 
  # observeEvent(
  #   ignoreInit = TRUE,
  #   eventExpr = {
  #     input$sensor_select
  #   }, 
  #   handlerExpr = {
  #     objues$sensor <- then(objues$sensors, function(d) {
  #       logger.trace(paste0(input$sensor_select, "...loading sensor..."))
  #       future({
  #         get_sensor(d, .data$label == isolate(input$sensor_select))
  #         
  #       })
  #     })
  #     objues$pat <- then(objues$pas, function(d) {
  #       logger.trace(paste0(input$sensor_select, "...loading pat..."))
  #       future({
  #         setArchiveBaseUrl("http://data.mazamascience.com/PurpleAir/v1") # SCAQMD sensors
  #         get_pat(
  #           pas = d, 
  #           label = isolate(input$sensor_select), 
  #           sd = isolate(input$date_range[1]), 
  #           ed = isolate(input$date_range[2])
  #         )
  #       })
  #     })
  #   } 
  # )
  # 
  # observeEvent(
  #   ignoreInit = TRUE, 
  #   eventExpr = {
  #     input$date_range
  #   }, 
  #   handlerExpr = {
  #     ff$updateSensors(input$date_range[1], input$date_range[2])
  #     print(ff$sensors)
  #     if ( ymd(input$date_range[2]) <= ymd(input$date_range[1]) ) {
  #       updateDateRangeInput(
  #         session,
  #         "date_range",
  #         start = ymd(input$date_range[2]) - days(1)
  #       )
  #     }
  #     if ( ymd(input$date_range[2]) - ymd(input$date_range[1]) > 21 ) {
  #       updateDateRangeInput(
  #         session,
  #         "date_range",
  #         start = ymd(input$date_range[2]) - days(21)
  #       )
  #       showNotification("Dates range too long!", "Max date range is 21 days.", type = "warn")
  #     }
  #     
  #     objues$sensors <- future({
  #       setArchiveBaseUrl("http://data.mazamascience.com/PurpleAir/v1") # SCAQMD sensors
  #       get_sensors(isolate(input$date_range[1]), isolate(input$date_range[2]))
  #     })
  #     
  #     objues$sensor <- then(objues$sensors, function(d) {
  #       logger.trace(paste0(input$sensor_select, "...loading sensor..."))
  #       future({
  #         get_sensor(d, .data$label == isolate(input$sensor_select))
  #         
  #       })
  #     })
  #     objues$pat <- then(objues$pas, function(d) {
  #       logger.trace(paste0(input$sensor_select, "...loading pat..."))
  #       future({
  #         setArchiveBaseUrl("http://data.mazamascience.com/PurpleAir/v1") # SCAQMD sensors
  #         get_pat(
  #           pas = d, 
  #           label = isolate(input$sensor_select), 
  #           sd = isolate(input$date_range[1]), 
  #           ed = isolate(input$date_range[2])
  #         )
  #       })
  #     })
  #     
  #   }
  # )
  # 
  # observeEvent(
  #   eventExpr = {
  #     resolved(list(objues$pas, objues$sensors))
  #   },
  #   handlerExpr = {
  #     # Update the choices of input 
  #     then(objues$sensors, function(d) {
  #       sensor_choices <- na.omit(unique(d$meta$label))
  #       community_choices <- id2com(na.omit(unique(d$meta$communityRegion)))
  #       # Fill the community selection
  #       updateSelectizeInput(
  #         session,
  #         inputId = "community_select",
  #         selected = "All...",
  #         choices = c("Choose a community" = "","All...", community_choices)
  #       )
  #       # Fill the sensor selection
  #       updateSelectizeInput(
  #         session,
  #         inputId = "sensor_select",
  #         choices = sensor_choices
  #       )
  #     })
  #     startupWaitress$close()
  #   }
  # )
  # 
  # observe({
  #   selected$sensor <- input$sensor_select
  #   selected$community <- input$community_select
  #   selected$sd <- ymd(input$date_range[1])
  #   selected$ed <- ymd(input$date_range[2])
  # })

  #### ////
  

  # observeEvent(
  #   eventExpr = {resolved(sensors)}, 
  #   handlerExpr = {
  #     then(sensors, function(d) {
  #       # Fill the community selection
  #       # updateSelectizeInput(
  #       #   session,
  #       #   inputId = "community_select",
  #       #   selected = "All...",
  #       #   choices = c("Choose a community" = "","All...", communities)
  #       # )
  #       # Fill the sensor selection
  #       updateSelectizeInput(
  #         session,
  #         inputId = "sensor_select",
  #         choices = d$meta$label
  #       )
  #     })
  #   }
  # )
  
  # # SCAQMD sensors
  # setArchiveBaseUrl("http://data.mazamascience.com/PurpleAir/v1") 
  # 
  # ## ---- Startup ----
  # 
  # # load non-changing rv: sensors, pas objs
  # observeEvent(
  #   once = TRUE,
  #   eventExpr = {
  #     objues$init
  #   }, 
  #   handlerExpr = {
  #     # notification
  #     startupWaitress <- waitress()
  #     startupWaitress$notify(html = tags$h4("Loading Data..."), position = "bl")
  #     
  #     ##  create sensors obj promise on startup 
  #     objues$sensors <- future({ 
  #       setArchiveBaseUrl("http://data.mazamascience.com/PurpleAir/v1") 
  #       get_sensors(sd = input$date_range[1], ed = input$date_range[2])
  #     })
  #     startupWaitress$set(10)
  #     
  #     then(objues$sensors, function(d) {
  #       # Check diff bewteen sensors aobj in sensor obj and pas obj and only use 
  #       # the sensors with mutual existence
  #       then(objues$pas, function(p) {
  #         communities <- na.omit(unique(id2com(d$meta$communityRegion[d$meta$label %in% p$label])))
  #         sensor_labels <- na.omit(unique(d$meta$label[d$meta$label %in% p$label]))
  #         
  #         # update sensors rv
  #         #objues$sensors <- d 
  #         # Fill the community selection
  #         updateSelectizeInput( 
  #           session,
  #           inputId = "community_select", 
  #           selected = "All...",
  #           choices = c("Choose a community" = "","All...", communities)
  #         )
  #         # Fill the sensor selection
  #         updateSelectizeInput(
  #           session, 
  #           inputId = "sensor_select", 
  #           choices = sensor_labels
  #         )
  #       })
  #       
  #       startupWaitress$set(50)
  #       logger.trace("sensors done.")
  #     })
  #     
  #     catch(objues$sensors, function(err) { 
  #       logger.error(err) 
  #     })
  #     
  #     ## create pas promise
  #     objues$pas <- future({ 
  #       setArchiveBaseUrl("http://data.mazamascience.com/PurpleAir/v1") 
  #       get_pas()
  #     })
  #     startupWaitress$set(75)
  #     then(objues$pas, function(d) { 
  #       # update pas rv 
  #       #objues$pas <- d 
  #       startupWaitress$set(90)
  #       logger.trace("pas done.")
  #     })
  #     catch(objues$pas, function(err) { 
  #       logger.error(err) 
  #     })
  #     startupWaitress$close()
  #   })
  
  ## ---- Event Handling ----
  
  # update the sensor selections on community selection
  # observeEvent(
  #   eventExpr = {
  #     input$community_select
  #   }, 
  #   handlerExpr = {
  #     req(objues$sensors, input$community_select)
  #     objues$community_select <- input$community_select
  #     then(objues$sensors, function(d) {
  #       if ( input$community_select == "All..." ) {
  #         objidSensors <- d$meta
  #       } else {
  #         objidSensors <-
  #           d$meta[id2com(d$meta$communityRegion) ==
  #                    input$community_select,]
  #       }
  #       updateSelectizeInput(
  #         session,
  #         "sensor_select",
  #         choices = na.omit(objidSensors$label)
  #       )
  #     })
  #   }, ignoreNULL = TRUE, ignoreInit = TRUE)
  # 
  # observeEvent(
  #   eventExpr = {
  #     input$sensor_select
  #   }, 
  #   handlerExpr = {
  #     req(input$sensor_select)
  #     
  #     # store sensor selection label as string for scoping
  #     objues$sensor_select <- input$sensor_select
  #     # filter/download a new sensors
  #     objues$sensor <- future({
  #       setArchiveBaseUrl("http://data.mazamascience.com/PurpleAir/v1") 
  #       then(objues$sensors, function(d) {
  #         get_sensor(
  #           sensors = d,
  #           .data$label == input$sensor_select
  #         )
  #       })
  #     })
  #     
  #     objues$pat <- future({
  #       setArchiveBaseUrl("http://data.mazamascience.com/PurpleAir/v1") 
  #       then(objues$pas, function(d) {
  #         get_pat(
  #           pas = d, 
  #           label = input$sensor_select, 
  #           sd = input$date_range[1], 
  #           ed = input$date_range[2]
  #         )
  #       })
  #     }, onRejected = function(err) {
  #       print("NO!")
  #       logger.error(err)
  #     })
  #     
  #     # Update the selector on map point click, done in backend by tiotemp,
  #     # but objue change must be watched 
  #     updateSelectizeInput(
  #       session, 
  #       "sensor_select", 
  #       selected = input$sensor_select
  #     )
  #     
  #   }, ignoreNULL = TRUE, ignoreInit = TRUE
  # )
  
  # update the sensor and pat rv on sensor selection
  # observeEvent(
  #   eventExpr = {
  #     input$date_range
  #   },
  #   handlerExpr = {
  #     req(objues$sensors, objues$pas)
  #     
  #     # if the user selects an enddate that is before or on the startdate, make
  #     # the startdate a day before the enddate to avoid weird stuff
  #     if ( ymd(input$date_range[2]) <= ymd(input$date_range[1]) ) {
  #       updateDateRangeInput(
  #         session,
  #         "date_range",
  #         start = ymd(input$date_range[2]) - days(1)
  #       )
  #     }
  #     if ( ymd(input$date_range[2]) - ymd(input$date_range[1]) > 21 ) {
  #       updateDateRangeInput(
  #         session,
  #         "date_range",
  #         start = ymd(input$date_range[2]) - days(31)
  #       )
  #       showNotification("Dates range too long!", "Max date range is 21 days.", type = "warn")
  #     }
  #     # establish startdate and endate rv
  #     objues$sd <- input$date_range[1]
  #     objues$ed <- input$date_range[2]
  #     
  #     # filter/download a new sensors
  #     objues$sensor <- future({
  #       setArchiveBaseUrl("http://data.mazamascience.com/PurpleAir/v1") 
  #       then(objues$sensors, function(d) {
  #         get_sensor(
  #           sensors = d,
  #           label = input$sensor_select,
  #           sd = input$date_range[1],
  #           ed = input$date_range[2]
  #         )
  #       })
  #     })
  #     objues$pat <- future({
  #       setArchiveBaseUrl("http://data.mazamascience.com/PurpleAir/v1") 
  #     then(objues$pas, function(d) {
  #         get_pat(
  #           pas = d, 
  #           label = input$sensor_select, 
  #           sd = input$date_range[1], 
  #           ed = input$date_range[2]
  #         )
  #       })
  #     })
  #     
  #     ##  create sensors obj promise on startup 
  #     objues$sensors <- future({ 
  #       setArchiveBaseUrl("http://data.mazamascience.com/PurpleAir/v1") 
  #       get_sensors(sd = input$date_range[1], ed = input$date_range[2])
  #     })
  #   }, ignoreNULL = TRUE, ignoreInit = TRUE)
  
  
  # 
  # # Attempt filter/load on date change
  # observeEvent(
  #   eventExpr = {
  #     input$date_range
  #   }, 
  #   handlerExpr = {
  #     req(input$sensor_select, objues$pas, objues$pat)
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
  #     objues$sd <- input$date_range[1]
  #     objues$ed <- input$date_range[2]
  #     # filter/download a new sensors
  #     objues$sensors <- get_sensors(
  #       sd = input$date_range[1], 
  #       ed = input$date_range[2], 
  #       sensors = objues$sensors
  #     )
  #     objues$pat <- get_pat(
  #       pas = objues$pas,
  #       label = input$sensor_select, 
  #       sd = input$date_range[1],
  #       ed = input$date_range[2], 
  #       pat = objues$pat
  #     )
  #   }, 
  #   priority = 1
  # )
  # 
  # # Save loading for specific navbar pages
  # observeEvent(
  #   eventExpr = {
  #     objues$navbar
  #   },
  #   handlerExpr = {
  #     
  #     if (objues$navbar == 'latest') {
  #       objues$pat_latest <- future({
  #         setArchiveBaseUrl("http://data.mazamascience.com/PurpleAir/v1") 
  #       then(objues$pas, function(d){
  #         
  #           get_pat_latest(
  #             pas = d,
  #             label = input$sensor_select
  #           )
  #         })
  #       })
  #     } 
  #       
  #       
  #       # tryCatch(
  #       #   expr = {
  #       #     makeWaitress({
  #       #       objues$pat_latest <- get_pat_latest(
  #       #         objues$pas,
  #       #         input$sensor_select
  #       #       )
  #       #     }, tags$h4(paste0("Loading latest ", input$sensor_select, "...")))
  #       #   },
  #       #   error = function(err) {
  #       #     logger.error(err)
  #       #     showNotification("Oops!", "An Error has occured.", type = "warn")
  #       #   }
  #       # )
  #   }
  # )
  
  # observeEvent(obju$navbar, {
  #   if (objues$navbar == 'latest') {
  #     shinyjs::hide("date_range", anim = TRUE)
  #   } else {
  #     shinyjs::show("date_range", anim = TRUE)
  #   }
  # })
  
} # End Server

## To be copied in the UI
# mod_main_panel_ui("main_panel_ui_1")

## To be copied in the server
# callModule(mod_main_panel_server, "main_panel_ui_1")

