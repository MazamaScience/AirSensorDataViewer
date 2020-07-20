# ----- Set up logging ---------------------------------------------------------

if ( !dir.exists(paste0(getwd(),"/logs")) ) {
  dir.create(paste0(getwd(), "/logs"))
}

if ( interactive() ) { # Running from RStudio
  # Somewhere easy to find
  LOG_DIR <- file.path(getwd(),"logs")
} else {
  # Use the shiny-server default
  LOG_DIR <- "logs" # "/var/log/shiny-server/"
}

MazamaCoreUtils::initializeLogging(LOG_DIR)

if ( interactive() ) { # Running from RStudio
  logger.setLevel(TRACE)
}

# Log session info
logger.debug(capture.output(sessionInfo()))

# logger.debug("VERSION = %s", VERSION)
# logger.debug("TZ = %s", TZ)
logger.debug("LOG_DIR = %s", LOG_DIR)