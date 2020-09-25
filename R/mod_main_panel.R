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
#' @importFrom bsplus bs_embed_tooltip
mod_main_panel_ui <- function(id) {
  ns <- NS(id)
  TZ <- 'UTC'
  tagList(
    
    selectizeInput(
      inputId = ns("community_select"),
      label = tags$h4("Community"), 
      selected = NULL,
      choices = list("Loading Communities..." = NULL)
    ),
    
    selectizeInput(
      inputId = ns("sensor_select"),
      label = tags$h4("Sensor"), 
      selected = NULL,
      choices = list("Loading Sensors..." = NULL)
    ),

    tags$hr(), 
    
    tags$h4("Date Range"),
    
    uiOutput(
      outputId = ns("date_range_label")
    ), 

    fluidRow(
      column(
        width = 6, 
        dateInput(
          inputId = ns("date_select"),
          label = tags$small("Select Date"), 
          min = ymd(20171001),
          max = today(tzone = TZ), 
          format = "M d, yyyy" 
        )
      ),
      column(
        width = 6, 
        selectizeInput(
          inputId = ns("past_select"), 
          label = tags$small("Past"), 
          selected = list("Past 7 Days" = 7),
          choices = list(
            "Past Day" = 1, 
            "Past 2 Days" = 2, 
            "Past 3 Days" = 3, 
            "Past 7 Days" = 7, 
            "Past 14 Days" = 14, 
            "Past Month" = 31
          )
        )
      )
    ),
    
    # Hacky way to get the client timezone stored in the client object
    shinyjs::hidden(textInput(
      inputId = ns("client_tz"),
      "client time",
      value = ""
    )),
    
    tags$hr(), 
    
    fluidRow(
      column(
        downloadLink(
          outputId = ns("download_button"), 
          label = tags$div(HTML('<svg width="1em" height="1em" viewBox="0 0 16 16" class="bi bi-cloud-download" fill="currentColor" xmlns="http://www.w3.org/2000/svg">
  <path fill-rule="evenodd" d="M4.406 1.342A5.53 5.53 0 0 1 8 0c2.69 0 4.923 2 5.166 4.579C14.758 4.804 16 6.137 16 7.773 16 9.569 14.502 11 12.687 11H10a.5.5 0 0 1 0-1h2.688C13.979 10 15 8.988 15 7.773c0-1.216-1.02-2.228-2.313-2.228h-.5v-.5C12.188 2.825 10.328 1 8 1a4.53 4.53 0 0 0-2.941 1.1c-.757.652-1.153 1.438-1.153 2.055v.448l-.445.049C2.064 4.805 1 5.952 1 7.318 1 8.785 2.23 10 3.781 10H6a.5.5 0 0 1 0 1H3.781C1.708 11 0 9.366 0 7.318c0-1.763 1.266-3.223 2.942-3.593.143-.863.698-1.723 1.464-2.383z"/>
  <path fill-rule="evenodd" d="M7.646 15.854a.5.5 0 0 0 .708 0l3-3a.5.5 0 0 0-.708-.708L8.5 14.293V5.5a.5.5 0 0 0-1 0v8.793l-2.146-2.147a.5.5 0 0 0-.708.708l3 3z"/>
</svg> Download...'))
        ), 
        tags$style("#main_panel_ui_1-download_button { text-align:center; }"),
        width = 7
      ),
      column(
        bs_embed_tooltip(
          title = "Copy URL",
          actionLink(
            inputId = ns("share_button"), 
            label = tags$div(HTML('<svg width="1em" height="1em" viewBox="0 0 16 16" class="bi bi-link-45deg" fill="currentColor" xmlns="http://www.w3.org/2000/svg">
  <path d="M4.715 6.542L3.343 7.914a3 3 0 1 0 4.243 4.243l1.828-1.829A3 3 0 0 0 8.586 5.5L8 6.086a1.001 1.001 0 0 0-.154.199 2 2 0 0 1 .861 3.337L6.88 11.45a2 2 0 1 1-2.83-2.83l.793-.792a4.018 4.018 0 0 1-.128-1.287z"/>
  <path d="M5.712 6.96l.167-.167a1.99 1.99 0 0 1 .896-.518 1.99 1.99 0 0 1 .518-.896l.167-.167A3.004 3.004 0 0 0 6 5.499c-.22.46-.316.963-.288 1.46z"/>
  <path d="M6.586 4.672A3 3 0 0 0 7.414 9.5l.775-.776a2 2 0 0 1-.896-3.346L9.12 3.55a2 2 0 0 1 2.83 2.83l-.793.792c.112.42.155.855.128 1.287l1.372-1.372a3 3 0 0 0-4.243-4.243L6.586 4.672z"/>
  <path d="M10 9.5a2.99 2.99 0 0 0 .288-1.46l-.167.167a1.99 1.99 0 0 1-.896.518 1.99 1.99 0 0 1-.518.896l-.167.167A3.004 3.004 0 0 0 10 9.501z"/>
</svg> Share...'))
          )
        ),
        
        # Add neat interactivity to the copy tooltip onclick:copied!
        tags$style("#main_panel_ui_1-share_button { text-align:center; }"),
        tags$script(
          HTML(
            '$("#main_panel_ui_1-share_button").on("click", () => {
            $("#main_panel_ui_1-share_button")
              .attr("data-original-title", "Copied!")
              .tooltip("fixTitle").tooltip("show");
            });
            $("#main_panel_ui_1-share_button").on("hidden.bs.tooltip", () => {
              $("#main_panel_ui_1-share_button")
                .attr("data-original-title", "Copy URL")
                .tooltip("fixTitle");
            });')
        ),
        width =  5
      ), 
      
      # Handle the community selection using javascript and tiotemp internals :)
      shinyjs::extendShinyjs(
        text = "
          shinyjs.communityFilter = function(params) {
          
            let defaultParams = {
              labels: null
            }; 
            
            params = shinyjs.getParams(params, defaultParams); 
            
            d3.selectAll('.point-map')
              .transition()
              .duration(1000)
              .attr('visibility', 'hidden');
            
            d3.selectAll(params.labels + '.point-map')
              .transition()
              .duration(1000)
              .attr('visibility', 'visible');
          
          };
          ", 
        functions = c("communityFilter")
      ), 
      
      # Add radius to date picker and match height of selectize inputs 
      tags$style(
      ".form-control {
          border-radius: 4px; 
          height: 33px;
      }")
      
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
#' @importFrom clipr write_clip
#' @importFrom shinyjs show hide
#' @importFrom utils write.csv
#' @importFrom promises `%...>%` `%...!%` promise_all
mod_main_panel_server <- function(input, output, session, usr) {
  ns <- session$ns
  
  w <- Waitress$new(min = 0, max = 100)
  
  w$notify(html = tags$h3("Loading Data..."), position = "bl")
  w$set(20)
  
  # initialize on every new token i.e. new client session object
  observeEvent(
    once = TRUE, 
    ignoreNULL = TRUE,
    eventExpr = {
      usr$token
    },
    handlerExpr = {
      # get the client timezone
      shinyjs::runjs("let tz = Intl.DateTimeFormat().resolvedOptions().timeZone; 
                      Shiny.setInputValue('main_panel_ui_1-client_tz', tz)")
      # set the client object timezone
      usr$setTz("America/Los_Angeles")
      
      
      promise_all(sensors = usr$sensors, pas = usr$pas) %...>% 
        with({
          # Check diff bewteen sensors aobj in sensor obj and pas obj and only use
          # the sensors with mutual existence
          pas_communities <- na.omit(unique(id2com(pas[['communityRegion']])))
          pas_labels <- na.omit(unique(pas[['label']]))
          
          sensors_communities <- na.omit(unique(id2com(sensors[["meta"]][['communityRegion']])))
          sensors_labels <- na.omit(unique(sensors[["meta"]][['label']]))
          
          community_choices <- sensors_communities[sensors_communities %in% pas_communities]
          sensor_choices <- sensors_labels[sensors_labels %in% pas_labels]
          
          
          # Fill the community selection
          updateSelectizeInput(
            session,
            inputId = "community_select",
            selected = "All...",
            choices = c("Choose a community" = NULL,"All...", community_choices)
          )
          # Fill the sensor selection
          updateSelectizeInput(
            session,
            inputId = "sensor_select",
            choices = sensor_choices
          )
          
        }) %...!% (function(err) {
          catchError(err)
        })
      
      # Close the waitress
      w$close()
      
    }
  )
  
  # debounce the sensor input to avoid too many clicks & infinite loops
  debouncedSelectSensor <- debounce(reactive(input$sensor_select), 250)
  observeEvent(
    ignoreNULL = TRUE,
    eventExpr = {
      debouncedSelectSensor()
    }, 
    handlerExpr = {
    
      # Update the client object sensor selection 
      usr$selected$sensor <- input$sensor_select
      
    }
  )
  
  # debounce the date input to avoid too many clicks & infinite loops
  #debouncedDateRange <- debounce(reactive(input$date_range), 250)
  observeEvent(
    priority = 100,
    ignoreNULL = TRUE,
    eventExpr = {
     # debouncedDateRange()
      input$past_select
      input$date_select
    }, 
    handlerExpr = {
      
      # Calculate the dates based on user selection
      sd <- lubridate::ymd(input$date_select) - lubridate::days(input$past_select)
      ed <- lubridate::ymd(input$date_select)
      
      # Plot down to avoid weird bugs 
      plotDown()
      
      # update the client object date selections
      usr$selected$sd <- sd
      usr$selected$ed <- ed
      
      usr$updateSensors(sd, ed)
      usr$updatePas(ed)
      
      # TODO: only update on year changes
      usr$updateAnnual(ed)
      
    }
  )
  
  # Watch the community selection
  observeEvent(
    ignoreNULL = TRUE,
    ignoreInit = TRUE, 
    eventExpr = {
      input$community_select
    }, 
    handlerExpr = {
      
      sensors <- usr$sensors
      
      usr$sensors %...>% (function(sensors) {
        meta <- sensors$meta
        # update the sensor selection per community if not on all
        if ( input$community_select == "All..." ) {
          choices <- meta
        } else {
          community_str <- id2com(meta[['communityRegion']])
          choices <- meta[community_str == input$community_select,]
        }
        
        updateSelectizeInput(
          session,
          "sensor_select",
          choices = na.omit(choices[['label']])
        )
        
        # Run the javascript to update the community selection on the map
        html_labels <- paste0("circle#",na.omit(choices$label))
        shinyjs::js$communityFilter(html_labels)

        
        # update the client community selection input
        usr$selected$community <- input$community_select
        
      })
      
    }
  )
  
  # Write the url to the user clipboard on share click
  observeEvent(
    ignoreNULL = TRUE, 
    ignoreInit = TRUE, 
    eventExpr = { input$share_button }, 
    handlerExpr = {
      url <- usr$url
      
      tryCatch(
        expr = {
          # Why does this break shiny? 
          # A: see allow_non_interactive in docs
          write_clip(url, allow_non_interactive = TRUE)
        }, 
        error = function(err) {
          logger.error(err)
          NULL
        }
      )
    }
  )
  
  # Watch the current page and tab. 
  # if on the latest page, hide the date range input 
  # if on community timelapse tab hide sensor selection
  observeEvent(
    ignoreNULL = TRUE,
    ignoreInit = TRUE,
    eventExpr = {
      usr$selected$page
      usr$selected$tab
    }, 
    handlerExpr = {
      if (usr$selected$page == 'latest') {
        hide("date_range", anim = TRUE)
      } else {
        show("date_range", anim = TRUE)
      }
      if ( usr$selected$tab == 'video' ) {
        hide("sensor_select", anim = TRUE)
      } else {
        show("sensor_select", anim = TRUE)
      }
    }
  )
  
  # Handle the download button using shiny tools. see ?downloadHandler docs. 
  output$download_button <- downloadHandler(
    filename = function() {
      sensor <- usr$selected$sensor
      sd <- usr$selected$sd
      ed <- usr$selected$ed
      paste0(sensor,'_',sd,'_',ed,".csv")
    },
    content = function(file) {
      pas <- usr$pas
      label <- usr$selected$sensor
      sd <- usr$selected$sd
      ed <- usr$selected$ed
      # Make sure pat is up to date in usr object
      usr$updatePat(label, sd, ed)
      usr$pat %...>% (function(pat) {
        write.csv(pat[['data']], file, row.names = FALSE)
      }) %...!% (function(err) {
        catchError(err)
      })
      
    }
  )
  
  output$date_range_label <- renderUI({
    
    # sd <- lubridate::ymd(input$date_select) - lubridate::days(input$past_select)
    # ed <- lubridate::ymd(input$date_select)
    # 
    # 
    # usr$selected$sd <- sd 
    # usr$selected$ed <- ed
   
   tags$h5(tags$div(HTML(paste0('<svg width="1em" height="1em" viewBox="0 0 16 16" class="bi bi-calendar-event" fill="currentColor" xmlns="http://www.w3.org/2000/svg">
  <path fill-rule="evenodd" d="M3.5 0a.5.5 0 0 1 .5.5V1h8V.5a.5.5 0 0 1 1 0V1h1a2 2 0 0 1 2 2v11a2 2 0 0 1-2 2H2a2 2 0 0 1-2-2V3a2 2 0 0 1 2-2h1V.5a.5.5 0 0 1 .5-.5zM1 4v10a1 1 0 0 0 1 1h12a1 1 0 0 0 1-1V4H1z"/>
  <path d="M11 6.5a.5.5 0 0 1 .5-.5h1a.5.5 0 0 1 .5.5v1a.5.5 0 0 1-.5.5h-1a.5.5 0 0 1-.5-.5v-1z"/>
</svg> ', strftime(usr$selected$sd, "%B %d, %Y"), "  -  ", strftime(usr$selected$ed, "%B %d, %Y")))))
   
   
  })

} # End Server

## To be copied in the UI
# mod_main_panel_ui("main_panel_ui_1")

## To be copied in the server
# callModule(mod_main_panel_server, "main_panel_ui_1")

