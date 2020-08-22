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
mod_video_server <- function(input, output, session, obj) {
  ns <- session$ns
  
  output$video <- renderUI({
    if ( obj$selected$community != "All.." ) {
      ed <- ymd(obj$selected$ed)
      baseUrl <- "http://smoke.mazamascience.com/data/PurpleAir/videos/"
      year    <- strftime(ed, "%Y")
      mm      <- strftime(ed, "%m")
      dd      <- strftime(ed, "%d")
      id    <- com2id(obj$selected$community)
      url <- paste0(baseUrl, year, "/", id, "_", year, mm, dd, ".mp4" )
      tags$video(
        id = "video",
        type = "video/mp4",
        src = url,
        controls = "controls"
      )
    }
  })
 
}
    
## To be copied in the UI
# mod_video_ui("video_ui_1")
    
## To be copied in the server
# callModule(mod_video_server, "video_ui_1")
 
