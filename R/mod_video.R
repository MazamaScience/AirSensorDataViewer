#' video UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_video_ui <- function(id){
  ns <- NS(id)
  tagList(
    column(
      width = 12,
      tags$h4("6-Day Sensor Timelapse"),
      wellPanel(
        uiOutput(
          outputId = ns("video")
        ) 
      )
    )
  )
}

#' video Server Function
#'
#' @noRd 
#' @importFrom lubridate ymd
mod_video_server <- function(input, output, session, usr) {
  ns <- session$ns
  
  output$video <- renderUI({
    req(usr$selected$community, usr$selected$ed)
    
    community <- usr$selected$community
    ed <- ymd(usr$selected$ed)
    
    tryCatch(
      expr = {
        if ( community != "All.." ) {
          ed <- ed
          baseUrl <- "http://smoke.mazamascience.com/data/PurpleAir/videos/"
          year    <- strftime(ed, "%Y")
          mm      <- strftime(ed, "%m")
          dd      <- strftime(ed, "%d")
          id    <- com2id(community)
          url <- paste0(baseUrl, year, "/", id, "_", year, mm, dd, ".mp4" )
          tags$video(
            id = "video",
            type = "video/mp4",
            src = url,
            controls = "controls"
          )
        }
      }, 
      error = function(err) {
        logger.error(err)
        NULL
      }
    )

  })
 
}
    
## To be copied in the UI
# mod_video_ui("video_ui_1")
    
## To be copied in the server
# callModule(mod_video_server, "video_ui_1")
 
