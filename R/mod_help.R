#' help UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @importFrom bsplus bs_embed_tooltip bs_attach_modal
mod_help_ui <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(
      ns("help") 
    ), 
    actionLink(
      inputId = ns("help_button"), 
      label = tags$div(
        HTML('<svg width="1.65em" height="1.65em" viewBox="0 0 16 16" class="bi bi-question-circle-fill" fill="currentColor" xmlns="http://www.w3.org/2000/svg">
                <path fill-rule="evenodd" d="M16 8A8 8 0 1 1 0 8a8 8 0 0 1 16 0zM6.57 6.033H5.25C5.22 4.147 6.68 3.5 8.006 3.5c1.397 0 2.673.73 2.673 2.24 0 1.08-.635 1.594-1.244 2.057-.737.559-1.01.768-1.01 1.486v.355H7.117l-.007-.463c-.038-.927.495-1.498 1.168-1.987.59-.444.965-.736.965-1.371 0-.825-.628-1.168-1.314-1.168-.901 0-1.358.603-1.358 1.384zm1.251 6.443c-.584 0-1.009-.394-1.009-.927 0-.552.425-.94 1.01-.94.609 0 1.028.388 1.028.94 0 .533-.42.927-1.029.927z"/>
              </svg>'))
    ) %>%
      bs_embed_tooltip("Show help") %>% 
      bs_attach_modal(id_modal = ns("help_modal")) 
  )
}
    
#' help Server Function
#'
#' @noRd 
#' @import bsplus
mod_help_server <- function(input, output, session, usr){
  ns <- session$ns
  
  output$help <- renderUI({
    req(usr$selected$tab, usr$selected$page)
    
    if ( usr$selected$page != 'latest' ) {
      tab <- usr$selected$tab
      
      help_path <- switch(
        tab,
        "overview" = "inst/app/www/overview_help.md", 
        "calendar" = "inst/app/www/calendar_help.md", 
        "raw" = "inst/app/www/raw_help.md", 
        "table" = "inst/app/www/table_help.md",
        "patterns" = "inst/app/www/patterns_help.md", 
        "compare" = "inst/app/www/comparison_help.md", 
        "video" = "inst/app/www/video_help.md"
      )
    } else {
      help_path <- "inst/app/www/latest_help.md"
    }

    bs_modal(
      id = ns("help_modal"), 
      title = tags$h2("Help"),
      body = includeMarkdown(help_path)
    )
  })
}
    
## To be copied in the UI
# mod_help_ui("help_ui_1")
    
## To be copied in the server
# callModule(mod_help_server, "help_ui_1")

 
