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
      tags$h4("7-Day Sensor Timelapse"),
      tags$hr(),
        uiOutput(
          outputId = ns("video")
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
    
    timezone <- getOption("asdv.timezone")
    
    community <- usr$selected$community
    ed <- ymd(usr$selected$ed, tz = timezone)
    
    tryCatch(
      expr = {
        if ( community != "All.." ) {
          ed <- ed
          baseUrl <- "https://airsensor.aqmd.gov/PurpleAir/v1//videos"
          year    <- strftime(ed, "%Y", tz = timezone)
          mm      <- strftime(ed, "%m", tz = timezone)
          dd      <- strftime(ed, "%d", tz = timezone)
          id    <- com2id(community)
          url <- paste(baseUrl, year, mm, paste0(id, "_", year, mm, dd, ".mp4"), sep = "/")
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
 
