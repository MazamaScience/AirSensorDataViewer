#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom shinythemes shinytheme
#' @noRd
app_ui <- function(request) {
  fluidPage(
    # ------ Panel Module -Column ----------------------------------------------
    column(
      width = 2,
      wellPanel(
        id = "panel",
        mod_main_panel_ui("main_panel_ui_1")
      ),
      # shiny::tags$footer(id = "ver", paste0("Version: ", VERSION))
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
        title = tags$b("Explore"),
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
                  #overview_mod_ui("global"),
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
                    wellPanel(
                      #calendar_mod_ui("global")
                    )
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
                value = "dp",
                
                tags$br(),
                #pattern_mod_ui("global")
              ),
              # ----- Compare tab -----
              tabPanel(
                title = tags$b("Compare"),
                icon = icon("balance-scale"),
                value = "comp",
                tags$br(),
                #comparison_mod_ui("global")
              ),
              # ---- Video tab ----
              tabPanel(
                title = tags$b("Community Timelapse"),
                icon = icon("file-video"),
                value = "anim",
                tags$br(),
                #video_mod_ui("global")
              )
            )
          ),
          # HELP
          column(
            width = 2,
            #help_mod_ui("global")
            
          )
        )
      ),
      
      #----- View Data Page ----------------------------------------------------
      tabPanel(
        title = tags$b("View Data"),
        value = "dv",
        fluidRow(
          #dataview_mod_ui("global")
        )
      ),
      tabPanel(
        title = tags$b("Latest Data"),
        value = "latest",
        fluidRow(
          column(
            width = 10,
            #latest_mod_ui("global")
          )
        )
      ),
      # ----- About Page -------------------------------------------------------
      tabPanel(
        title = tags$b("About"),
        value = "about",
        fluidRow(
          column(
            width = 10,
            #shiny::includeHTML(file.path(getwd(),"../www/about.html"))
          )
        )
      )
    ),
    
    # Use ShinyJS
    #shinyjs::useShinyjs(debug = TRUE),
    # Enable the "Share" Clipboard JS
    #rclipboard::rclipboardSetup(),
    # Enable Toastr ntofications
    #shinytoastr::useToastr(),
    # Load the extra JS script
    #shinyjs::extendShinyjs("../www/extra.js"),
    
    # Other Random CSS
    tags$style(type="text/css", "body {padding-top: 70px;}"),
    tags$style(type="text/css", "footer {padding-left: 5%; color: #808080; font-size: 11px}"),
    tags$style(type="text/css", ".well {background-color: #fff}"),
    tags$style(type="text/css", "#panel {min-width:200px;}"),
    tags$style(type = "text/css", "#global-leaflet {height: calc(80vh) !important;}"),
  )

}

#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
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
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
  )
}

