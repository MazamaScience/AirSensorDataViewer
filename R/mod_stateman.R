#' stateman Server Function
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
  
  observeEvent(
    ignoreNULL = TRUE,
    ignoreInit = TRUE,
    eventExpr = {
      usr$selected$sd
      usr$selected$ed
    },
    handlerExpr = {
      sd <- usr$selected$sd
      ed <- usr$selected$ed
      usr$updateSensors(sd, ed)
    }
  )
  
  onStop(
    fun = function() { 
      logger.trace(paste("User session ended:",  session$token)) 
    }, 
    session = session
  )
  
}
  
## To be copied in the server
# callModule(mod_stateman_server, "stateman_1")


#' A "Magic" Auto Updater
#' 
#' A tool  intended to minimize local computational cost that handles the current 
#' client object state to best determine what to update. 
#'
#' @param usr 
#'
#' @return
#' @export
#'
#' @examples
magicUpdate <- function(usr) {
  label <- usr$selected$sensor
  sd <- usr$selected$sd
  ed <- usr$selected$ed
  page <- usr$selected$page
  tab <- usr$selected$tab
  
  if ( page == 'explore' ) {
    
    if ( tab == 'overview' ) {
      # Do nothing 
    } else if ( tab == 'calendar' ) {
      # Do Nothing
    } else if ( tab == 'raw' ) {
      usr$updatePat(label, sd, ed)
    } else if ( tab == 'patterns' ) {
      usr$updateSensor(label)
    } else if ( tab == 'compare' ) {
      usr$updatePat(label, sd, ed)
      usr$updateSensor(label)
      usr$updatePwfsl(label, sd, ed)
    } else { # tab == 'video'
      # Do nothing
    }
    
  } else if ( page == 'table' ) {
    
    usr$updatePat(label, sd, ed)
    
  } else if ( page == 'latest' ) {
    
    usr$updateLatest(label)
    
  } else {
    # Do nothing
  }
}