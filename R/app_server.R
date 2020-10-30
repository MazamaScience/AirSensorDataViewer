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
  
  # ----- Setup ----------------------------------------------------------------
  
  # Ignore all warnings
  options(warn = -1)
  
  # Set app-specific global options
  options(
    asdv.timezone = "America/Los_Angeles"
  )
  
  # set up logs per session 
  setupSessionLogs(session)
  
  # Specify how futures are resolved, e.g. sequentially or in parallel.
  plan(future::sequential)
  # plan(future::multiprocess)
  
  # Create the client session object
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

  # ----- Bookmarking ----------------------------------------------------------
  
  observe({
    inputs <- reactiveValuesToList(input)
    bookmarkable <- c(
      "main_panel_ui_1-sensor_select", 
      "main_panel_ui_1-community_select", 
      "tab", 
      "page",
      "main_panel_ui_1-date_select", 
      "main_panel_ui_1-past_select"
    )
    nonBookmarkable <- names(input)[!names(input) %in% bookmarkable]
    setBookmarkExclude(nonBookmarkable)
    session$doBookmark()
  })
  onBookmarked(function(url) {
    updateQueryString(url)
    usr$url <- url
  })
  onRestored(function(state) {
    logger.trace("Restoring from url")
    shinyjs::delay(1500, {
      
      tab <- state$input$tab
      page <- state$input$page
      sded <- state$input$`main_panel_ui_1-date_select`
      label <-   state$input$`main_panel_ui_1-sensor_select`
      community <-  state$input$`main_panel_ui_1-community_select`
      past <- state$input$`main_panel_ui_1-past_select`
      
      # TODO: Add page and tab?
      
      shinyjs::runjs(
        paste0(
          '$("select#`main_panel_ui_1-community_select`")[0]
                .selectize
                .setValue("', community,'", false)'
        )
      )
      
      shinyjs::runjs(
        paste0(
          '$("select#main_panel_ui_1-sensor_select")[0]
                .selectize
                .setValue("', label,'", false)'
        )
      )
      
      shinyjs::runjs(
        paste0(
          '$("select#`main_panel_ui_1-past_select`")[0]
                .selectize
                .setValue("', past,'", false)'
        )
      )
      
      plotUp()
      
    })
    
  })
  
  # ----- Set up observers -----------------------------------------------------
  
  # Navbar at the top
  observeEvent(input$navbar, {
    logger.trace("NAVBAR: %s", input$navbar)
    usr$selected$page <- input$navbar
  })
  
  # Tabs underneath "Historical Data" (where most of the action happens)
  observeEvent(input$tab, {
    logger.trace("TAB: %s", input$tab)
    usr$selected$tab <- input$tab
  })
  
}
