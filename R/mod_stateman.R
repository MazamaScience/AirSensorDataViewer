#' State Manager Server Function
#'
#' @noRd 
mod_stateman_server <- function(input, output, session, usr){
  ns <- session$ns
  
  # Watch the sensor selction 
  observeEvent(
    ignoreNULL = TRUE, 
    ignoreInit = TRUE, 
    eventExpr = {
      usr$selected$sensor
      usr$selected$sd
      usr$selected$ed
      usr$selected$page
      usr$selected$tab
    }, 
    handlerExpr = {
      magicUpdate(usr)
    }
  )
  
  onStop(
    fun = function() { 
      logger.debug(paste("User session ended:",  session$token)) 
    }, 
    session = session
  )
  
}
  
## To be copied in the server
# callModule(mod_stateman_server, "stateman_1")


#' A "Magic" Auto Updater
#' 
#' A tool intended to minimize local computational cost that handles the current 
#' client object state to best determine what to update. 
#'
#' @param usr The user object.
#'
#' @export
magicUpdate <- function(usr) {
  label <- usr$selected$sensor
  sd <- usr$selected$sd
  ed <- usr$selected$ed
  page <- usr$selected$page
  tab <- usr$selected$tab
  
  if ( page == 'explore' ) {
    
    if ( tab == 'overview' ) {
      
      #shinyjs::delay(100, plotUp())
      
      shinyjs::show('.tooltip-map')
      shinyjs::hide('.tooltip-calendar')
      
    } else if ( tab == 'calendar' ) {
      
      shinyjs::hide('.tooltip-map')
      shinyjs::show('.tooltip-calendar')
      
      
    } else if ( tab == 'raw' ) {
      
      usr$updatePat(label, sd, ed)
      
      shinyjs::hide('.tooltip-map')
      shinyjs::hide('.tooltip-calendar')
      
    } else if ( tab == 'patterns' ) {
      
      usr$updateSensor(label)
      usr$updateNoaa(sd, ed)
      
      shinyjs::hide('.tooltip-map')
      shinyjs::hide('.tooltip-calendar')
      
    } else if ( tab == 'compare' ) {
      
      usr$updatePat(label, sd, ed)
      usr$updateSensor(label)
      usr$updatePwfsl(label, sd, ed)
      
      shinyjs::hide('.tooltip-map')
      shinyjs::hide('.tooltip-calendar')
      
    } else if ( tab == 'table' ) {
      
      usr$updatePat(label, sd, ed)
      
      shinyjs::hide('.tooltip-map')
      shinyjs::hide('.tooltip-calendar')
      
    } else { # tab == 'video'
      # Do nothing
    }
  
  # Moved to tabset panel     
  # } else if ( page == 'table' ) {
  #   
  #   usr$updatePat(label, sd, ed)
    
  } else if ( page == 'latest' ) {
    
    usr$updateLatest(label)
    
    shinyjs::hide('.tooltip-map')
    shinyjs::hide('.tooltip-calendar')
    
  } else {
    # Do nothing
  }
  
}