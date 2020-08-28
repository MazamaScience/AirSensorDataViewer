#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom shinythemes shinytheme
#' @importFrom waiter waiter_show_on_load spin_three_bounce
#' @importFrom shinyjs extendShinyjs
#' @noRd
app_ui <- function(request) {
  fluidPage(
    golem_add_external_resources(),
    waiter_show_on_load(html = spin_three_bounce(), color = "#006687"),
    
    # ------ Panel Module -Column ----------------------------------------------
    column(
      width = 2,
      wellPanel(
        id = "panel",
        mod_main_panel_ui("main_panel_ui_1"), 
      ),        
      fluidRow(
        column(
          width = 6, 
          mod_help_ui("help_ui_1") 
        ), 
        column(
          width = 6, 
          tags$footer(id = "version", paste0("v", golem::get_golem_version())),
        )
      )
    ),
    
    navbarPage(
      # ------ Nav Bar ---------------------------------------------------------
      title = tags$b("AirSensor DataViewer (Beta)"),
      theme = shinytheme("yeti"),
      inverse = TRUE,
      id = "navbar",
      fluid = TRUE,
      collapsible = TRUE,
      position = "fixed-top",
      windowTitle = "AirSensor DataViewer",
      
      # ------ Explore Page ----------------------------------------------------
      tabPanel(
        title = tags$b("Historical Data"),
        value = "explore",
        fluidRow(
          column(
            width = 8,
            # ----- Tabs -----
            tabsetPanel(
              type = "tabs",
              id = "tab",
              # ---- Overview Tab ----
              tabPanel(
                title = tags$b("Overview"),
                icon = icon("map-marked-alt"),
                value = "overview",
                tags$br(),
                column(
                  width = 12,
                  mod_overview_ui("overview_ui_1")
                )
              ),
              # ---- Calendar tab ----
              tabPanel(
                title = tags$b("Calendar"),
                icon = icon("calendar-alt"),
                value = "calendar",
                tags$br(),
                fluidRow(
                  column(
                    width = 12,
                    tags$h4("Calendar"),
                    mod_calendar_ui("calendar_ui_1")
                  )
                )
              ),
              # ---- Raw data tab ----
              tabPanel(
                title = tags$b("Raw Data"),
                icon = icon("database"),
                value = "raw",
                tags$br(),
                mod_raw_ui("raw_ui_1")
              ),
              # ----- Daily patterns tab -----
              tabPanel(
                title = tags$b("Daily Patterns"),
                icon = icon("chart-bar"),
                value = "patterns",
                
                tags$br(),
                mod_patterns_ui("patterns_ui_1")
              ),
              # ----- Compare tab -----
              tabPanel(
                title = tags$b("Compare"),
                icon = icon("balance-scale"),
                value = "compare",
                tags$br(),
                mod_compare_ui("compare_ui_1")
              ),
              # ---- Video tab ----
              tabPanel(
                title = tags$b("Community Timelapse"),
                icon = icon("file-video"),
                value = "video",
                tags$br(),
                mod_video_ui("video_ui_1")
              )
            )
          ),
          # HELP
          column(
            width = 1,
            #mod_help_ui("help_ui_1")
          )
        )
      ),
      
      #----- View Data Page ----------------------------------------------------
      tabPanel(
        title = tags$b("View Data"),
        value = "table",
        fluidRow(
          column(
            width = 8,
            mod_datatable_ui("datatable_ui_1")
          )
        )
      ),
      
      # ----- Latest Data page -------------------------------------------------
      tabPanel(
        title = tags$b("Latest Data"),
        value = "latest",
        fluidRow(
          column(
            width = 8,
            mod_latest_ui("latest_ui_1")
          )
        )
      ),
      # ----- About Page -------------------------------------------------------
      tabPanel(
        title = tags$b("About"),
        value = "about",
        fluidRow(
          column(
            width = 8,
            includeHTML("inst/app/www/about.html")
          )
        )
      )
    ),
  
    # Load the extra JS script
    extendShinyjs("inst/app/www/plotAnimate.js"),
    
    # Other Random CSS
    tags$style(
      type="text/css", 
      "body {padding-top: 70px;}"
    ),
    tags$style(
      type="text/css", 
      "footer {
        color: #808080; 
        font-size: 0.7em;  
        text-align: right;
      }"
    ),
    tags$style(
      type="text/css", 
      ".well {background-color: #fff}"
    ),
    tags$style(
      type="text/css", 
      ".col-sm-2 {min-width:254px;}"
    ), 
  
  )
  
}

#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @importFrom bsplus use_bs_tooltip
#' @importFrom shinyjs useShinyjs 
#' @importFrom waiter use_waiter use_waitress
#' @noRd
golem_add_external_resources <- function(){
  
  add_resource_path(
    'www', app_sys('app/www')
  )
  
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'AirSensorDataViewer'
    ),
    # Add here other external resources
    # profvis::profvis_ui("profiler"), # Dev Only
    use_waiter(), 
    use_waitress(), 
    use_bs_tooltip(),
    useShinyjs(),
  )
}

