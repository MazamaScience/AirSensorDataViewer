#' stateman Server Function
#'
#' @noRd 
mod_stateman_server <- function(input, output, session, usr){
  ns <- session$ns
  
  # Watch the 
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
      
      logger.trace("Some updates")
      
      label <- usr$selected$sensor
      tab <- usr$selected$tab 
      page <- usr$selected$page 
      sd <- usr$selected$sd
      ed <- usr$selected$ed
      tz <- usr$tz
      
      # Explicit Loading.
      if ( tab == 'raw' ) {
        pas <- usr$pas
        usr$updatePat(pas, label, sd, ed)
      }
      if ( tab == 'compare' ) {
        pas <- usr$pas
        usr$updatePat(pas, label, sd, ed)
        sensors <- usr$sensors
        usr$updateSensor(sensors, label)
        sensor <- usr$sensor
        usr$updatePwfsl(sensor$meta$pwfsl_closestMonitorID, sd, ed)
      }
      if ( tab == 'patterns' ) {
        sensors <- usr$sensors
        usr$updateSensor(sensors, label)
      }
      
      if ( page == 'table' ) {
        pas <- usr$pas
        usr$updatePat(pas, label, sd, ed)
      }
      
      if ( page == 'latest' ) {
        pas <- usr$pas
        usr$updateLatest(
          pas = pas,
          label = label,
          tz = tz
        )
      }
      
      # plotUp()
      
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
