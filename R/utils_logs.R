library(MazamaCoreUtils)
# ----- Set up logging ---------------------------------------------------------

if ( !dir.exists(paste0(getwd(),"/logs")) ) {
  dir.create(paste0(getwd(), "/logs"))
}

if ( interactive() ) { # Running from RStudio
  # Somewhere easy to find
  LOG_DIR <- "logs"
} else {
  
  # NOTE: In order to propely check and test the package, this must 
  # use the local server directory for the check! Use Shiny server log 
  # directory for deployment.
  # Use the shiny-server default
  #LOG_DIR <- "/var/log/shiny-server/" #deploy
  LOG_DIR <- "logs" # Local 
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