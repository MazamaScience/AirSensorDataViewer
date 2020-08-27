#' The Client Data Object
NULL

#'@export
#'@importFrom future future `%<-%` `%globals%`
Client <- R6::R6Class(
  "Client",
  private = list(
    session = NULL
  ),
  public = list(
    baseUrl = "http://data.mazamascience.com/PurpleAir/v1",
    token = NULL, 
    tz = NULL,
    url = NULL,
    
    lastInput = NULL,
    
    selected = NULL,
    data = NULL,
    
    initialize = function(session) {
      
      private$session <- session
      
      # NOTE: init of rv must be inside the init method itself. For some reason, 
      # NOTE: if the init it outside, rv is shared across all instances. 
      # NOTE: Found out why: see ?R6::R6Class details
      self$data <- rv(
        pas = NULL, 
        sensors = NULL, 
        sensor = NULL, 
        pat = NULL, 
        pwfsl = NULL, 
        latest = NULL, 
        annual = NULL
      )
      
      self$selected <- rv()
      
      self$token <- session$token
      logger.trace(paste("initializing a new client object session token:", self$token))
      
      
      setArchiveBaseUrl(self$baseUrl)
      self$data$pas <- tryCatch(
        expr = {
          get_pas()
        }, 
        error = function(err) { 
          logger.error(err)
          return(NULL)
        })
      
      self$data$sensors<- tryCatch(
        expr = { 
          get_sensors(today() - days(7), today())
        }, 
        error = function(err) { 
          logger.error(err)
          return(NULL)
        })
      
      
      self$lastInput <- as.numeric(Sys.time())
    },
    
    updatePas = function() {
      logger.trace("updating pas...")
      f %<-% {
        setArchiveBaseUrl(self$baseUrl)
        tryCatch(
          expr = {
            get_pas()
          }, 
          error = function(err) { 
            logger.error(err)
            return(NULL)
          })
      }
      self$data$pas <- f
    }, 
    
    updateSensors = function(sd, ed) {
      logger.trace(paste("updating sensors -->", sd, ed))
      print(self$data$sensors)
      f %<-% {
        setArchiveBaseUrl(self$baseUrl)
        tryCatch(
          expr = {
            get_sensors(sd, ed)
          }, 
          error = function(err) { 
            logger.error(err)
            return(NULL)
          })
      }
      self$data$sensors <- f
    }, 
    
    updateSensor = function(sensors, label) {
      logger.trace(paste("updating sensor -->", label))
      # Not really sure why, but this redef is absolutely neccesary. 
      lab <- label
      self$data[['sensor']] <- {
        sensor_filterMeta(sensors, .data$label == lab)
      }
      # self$data[['sensor']] <- f
    }, 
    
    updatePat = function(pas, label, sd, ed, ...) {
      logger.trace(paste("updating pat -->", label, sd, ed))
      f %<-% {
        setArchiveBaseUrl(self$baseUrl)
        tryCatch(
          expr = {
            get_pat(
              pas = pas,
              label = label,
              sd = sd,
              ed = ed, 
              ...
            )
          }, 
          error = function(err) { 
            logger.error(err)
            return(NULL)
          }) 
      }
      self$data$pat <- f
    }, 
    
    updatePwfsl = function(id, sd, ed) {
      logger.trace(paste("updating pwfsl -->", id, sd, ed))
      f %<-% {
        tryCatch(
          PWFSLSmoke::monitor_load(sd, ed, id), 
          error = function(err) { 
            logger.error(err)
            return(NULL)
          })
      }
      self$data$pwfsl <- f
    }, 
    
    updateLatest = function(pas, label, tz = 'US/Pacific') {
      logger.trace(paste("updating latest -->", label, tz))
      f %<-% {
        tryCatch(
          get_pat_latest(pas, label, tz), 
          error = function(err) { 
            logger.error(err)
            return(NULL)
          })
      }
      self$data$latest <- f
    }, 
    
    updateAnnual = function(pas, label, date) {
      self$data[['annual']] <- tryCatch(
        get_pat_annual(pas, label, date) %>% 
          AirSensor::pat_createAirSensor(), 
        error = function(err) { 
          logger.error(err)
          return(NULL)
        })
    }, 
    
    updateLastInput = function(t) {
      self$lastInput <- as.numeric(t)
    }, 
    
    setTz = function(timezone) {
      self$tz <- timezone
    }
  )
)