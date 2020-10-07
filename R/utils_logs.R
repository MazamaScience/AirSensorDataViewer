#' Setup Logging for Shiny Sessions
#'
#' @param session a shiny session object. 
#'
#' @export
#' @import MazamaCoreUtils
#' @importFrom utils capture.output sessionInfo
setupSessionLogs <- function(session) {
  
  # ----- Set up logging ---------------------------------------------------------
  
  if ( interactive() ) { # Running from RStudio
    LOG_DIR <- paste0(golem::get_golem_wd(), "/logs/logs_", session$token)
  } else { # Docker 
    LOG_DIR <- paste0("/var/log/shiny-server/logs_", session$token)
  }
  
  dir.create(LOG_DIR, recursive = TRUE, showWarnings = FALSE)
  
  initializeLogging(LOG_DIR)
  
  if ( interactive() ) { # Running from RStudio
    logger.setLevel(TRACE)
  }
  
  # Log session info
  logger.debug(capture.output(sessionInfo()))
  
  return(NULL)
  
}
