#' Setup Logging for Shiny Sessions
#'
#' @param session a shiny session object. 
#'
#' @export
#' @import MazamaCoreUtils
setupSessionLogs <- function(session) {
  
  # ----- Set up logging ---------------------------------------------------------
  
  if ( !dir.exists(paste0(golem::get_golem_wd(),"/logs")) ) {
    dir.create(paste0(golem::get_golem_wd(), "/logs"))
  }
  
  if ( interactive() ) { # Running from RStudio
    session_log_dir <- paste0(golem::get_golem_wd(), "/logs/logs_", session$token)
    dir.create(session_log_dir)
    LOG_DIR <- session_log_dir
  } else { # Docker 
    session_log_dir <- paste0("/var/log/shiny-server/logs_", session$token)
    dir.create(session_log_dir)
    LOG_DIR <- session_log_dir 
  }
  
  initializeLogging(LOG_DIR)
  
  if ( interactive() ) { # Running from RStudio
    logger.setLevel(TRACE)
  }
  
  # Log session info
  logger.debug(capture.output(sessionInfo()))
  
  # logger.debug("VERSION = %s", VERSION)
  # logger.debug("TZ = %s", TZ)
  logger.debug("LOG_DIR = %s", LOG_DIR)
  
  return(NULL)
}
