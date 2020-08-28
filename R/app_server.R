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
  plan(future::sequential)
  
  # Create the client session object
  #obj <- Client$new(session)
  usr <- User$new(session)
  
  # List the first level callModules here
  # callModule(profvis::profvis_server, "profiler") # Dev Only
  callModule(mod_stateman_server, "stateman_1", usr)
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
  
  #Bookmarking
  observe({
    reactiveValuesToList(input)
    session$doBookmark()
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
  
}
