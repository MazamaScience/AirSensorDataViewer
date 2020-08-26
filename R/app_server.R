#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom waiter waiter_hide
#' @importFrom future plan 
#' @noRd
app_server <- function( input, output, session ) {
  
  # go rogue
  options(warn = -1)
  
  # Specify how futures are resolved, e.g. sequentially or in parallel.
  plan(future::multiprocess)
  
  # Record session start 
  observe({ 
    logger.trace(paste("session started:", session[['token']])) 
  })
  # Record session end 
  session[['onSessionEnded']](function() { 
    logger.trace(paste("session ended:",  session[['token']])) 
  })
  
  # Bookmarking
  observe({
    reactiveValuesToList(input)
    session[['doBookmark']]()
  }) 
  onBookmarked(function(url) {
    updateQueryString(url)
    obj[['url']] <- url
  })
  
  # Create the client session object
  obj <- Client[['new']](session)
  
  # Watch tabs and page
  observeEvent(input[['navbar']], {
    logger.trace(paste("navbar:", input[['navbar']]))
    obj[['selected']][['page']] <- input[['navbar']]
  })
  observeEvent(input[['tab']], {
    logger.trace(paste("tab:", input[['tab']]))
    obj[['selected']][['tab']] <- input[['tab']]
  })
  
  # List the first level callModules here
  #callModule(profvis::profvis_server, "profiler") # Dev Only
  callModule(mod_main_panel_server, "main_panel_ui_1", obj)
  callModule(mod_overview_server, "overview_ui_1", obj)
  callModule(mod_calendar_server, "calendar_ui_1", obj)
  callModule(mod_raw_server, "raw_ui_1", obj)
  callModule(mod_patterns_server, "patterns_ui_1", obj)
  callModule(mod_compare_server, "compare_ui_1", obj)
  callModule(mod_video_server, "video_ui_1", obj)
  callModule(mod_latest_server, "latest_ui_1", obj)
  callModule(mod_datatable_server, "datatable_ui_1", obj)
  callModule(mod_help_server, "help_ui_1", obj)
  
  # Hide the waiter startup once the modules have been loaded 
  waiter_hide()
  
}
