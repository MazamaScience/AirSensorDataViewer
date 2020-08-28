#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom waiter waiter_hide
#' @importFrom future plan 
#' @import MazamaCoreUtils
#' @noRd
app_server <- function( input, output, session ) {
  
  # go rogue
  options(warn = -1)
  
  # Specify how futures are resolved, e.g. sequentially or in parallel.
  plan(future::multiprocess)
  
  # Create the client session object
  #obj <- Client$new(session)
  usr <- User$new(session)
  
  # Record session start 
  observe({ 
    logger.trace(paste("session started:", session$token)) 
  })
  # Record session end 
  session$onSessionEnded(function() { 
    logger.trace(paste("session ended:",  session$token)) 
  })
  
  # Bookmarking
  observe({
    reactiveValuesToList(input)
    session[['doBookmark']]()
  }) 
  onBookmarked(function(url) {
    updateQueryString(url)
    usr$url <- url
  })
  
  # Watch tabs and page
  observeEvent(input$navbar, {
    logger.trace(paste("navbar:", input$navbar))
    usr$selected$page <- input$navbar
  })
  observeEvent(input$tab, {
    logger.trace(paste("tab:", input$tab))
    usr$selected$tab <- input$tab
  })
  
  # Watch the 
  observeEvent(
    ignoreNULL = TRUE, 
    ignoreInit = TRUE, 
    eventExpr = {
      usr$selected$sensor
      usr$selected$sd
      usr$selected$ed
      usr$selected$page
      usr$selected$tab
    }, 
    handlerExpr = {
      
      logger.trace("Some updates")
      
      label <- usr$selected$sensor
      tab <- usr$selected$tab 
      page <- usr$selected$page 
      sd <- usr$selected$sd
      ed <- usr$selected$ed
      tz <- usr$tz
      
      # Explicit Loading.
      if ( tab == 'raw' ) {
        pas <- usr$pas
        usr$updatePat(pas, label, sd, ed)
      }
      if ( tab == 'compare' ) {
        pas <- usr$pas
        usr$updatePat(pas, label, sd, ed)
        sensors <- usr$sensors
        usr$updateSensor(sensors, label)
        sensor <- usr$sensor
        usr$updatePwfsl(sensor$meta$pwfsl_closestMonitorID, sd, ed)
      }
      if ( tab == 'patterns' ) {
        sensors <- usr$sensors
        usr$updateSensor(sensors, label)
      }
      
      if ( page == 'table' ) {
        pas <- usr$pas
        usr$updatePat(pas, label, sd, ed)
      }
      
      if ( page == 'latest' ) {
        pas <- usr$pas
        usr$updateLatest(
          pas = pas,
          label = label,
          tz = tz
        )
      }
      
      # plotUp()
      
    }
  )
  
  # Watch the current page. if on the latest page, hide the date range input 
  observeEvent(
    ignoreNULL = TRUE,
    ignoreInit = TRUE,
    eventExpr = {
      usr$selected$page
    }, 
    handlerExpr = {
      if (usr$selected$page == 'latest') {
        hide("date_range", anim = TRUE)
      } else {
        show("date_range", anim = TRUE)
      }
    }
  )
  
  # List the first level callModules here
  #callModule(profvis::profvis_server, "profiler") # Dev Only
  callModule(mod_main_panel_server, "main_panel_ui_1", usr)
  callModule(mod_overview_server, "overview_ui_1", usr)
  callModule(mod_calendar_server, "calendar_ui_1", usr)
  callModule(mod_raw_server, "raw_ui_1", usr)
  callModule(mod_patterns_server, "patterns_ui_1", usr)
  callModule(mod_compare_server, "compare_ui_1", usr)
  callModule(mod_video_server, "video_ui_1", usr)
  callModule(mod_latest_server, "latest_ui_1", usr)
  callModule(mod_datatable_server, "datatable_ui_1", usr)
  callModule(mod_help_server, "help_ui_1", usr)
  
  # Hide the waiter startup once the modules have been loaded 
  waiter_hide()
  
}
