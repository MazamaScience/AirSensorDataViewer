#' The Client Data Object
NULL

#'@export
#'@importFrom future future
Client <- R6::R6Class(
  # For speed, make classless and set portability to false
  # https://r6.r-lib.org/articles/Performance.html
  class = FALSE, 
  portable = FALSE,
  "ClientObject",
  public = list(
    baseUrl = "http://data.mazamascience.com/PurpleAir/v1",
    token = NULL, 
    url = NULL,
    inter = NULL, 
    lastInput = NULL,
    
    #TODO Preinit for speeeeed
    selected = rv(),
    data = rv(
      pas = NULL, 
      sensors = NULL, 
      sensor = NULL, 
      pat = NULL, 
      pwfsl = NULL, 
      latest = NULL, 
      annual = NULL
    ),
    
    initialize = function(session) {
      logger.trace("init")
      token <<- session[['token']]
      setArchiveBaseUrl(baseUrl)
      
      data[['pas']] <<- tryCatch(
        expr = get_pas(), 
        error = function(err) { 
          logger.trace(err)
          return(NULL)
        })
      
      data[['sensors']] <<- tryCatch(
        get_sensors(today() - days(7), today()), 
        error = function(err) { 
          logger.trace(err)
          return(NULL)
        })
      
      inter <<- ipc::AsyncInterruptor$new()
      lastInput <<- as.numeric(Sys.time())
    },
    
    updatePas = function() {
      setArchiveBaseUrl(baseUrl)
      data[['pas']] <<- tryCatch(
        get_pas(), 
        error = function(err) { 
          logger.trace(err)
          return(NULL)
        })
    }, 
    
    updateSensors = function(sd, ed) {
      setArchiveBaseUrl(baseUrl)
      data[['sensors']] <<- tryCatch(
        get_sensors(sd, ed), 
        error = function(err) { 
          logger.trace(err)
          return(NULL)
        })
    }, 
    
    updateSensor = function(sensors, ...) {
      data[['sensor']] <<- tryCatch(
        AirSensor::sensor_filterMeta(sensors, ...),
        error = function(err) { 
          logger.trace(err)
          return(NULL)
        })
    }, 
    
    updatePat = function(pas, label, sd, ed, ...) {
      data[['pat']] <<- future({
        setArchiveBaseUrl(baseUrl)
        tryCatch(
          get_pat(
            pas = pas,
            label = label,
            sd = sd,
            ed = ed, 
            ...
          ), 
          error = function(err) { 
            logger.error(err)
            return(NULL)
          }) 
      })
    }, 
    
    updatePwfsl = function(id, sd, ed) {
      data[['pwfsl']] <<- tryCatch(
        PWFSLSmoke::monitor_load(sd, ed, id), 
        error = function(err) { 
          logger.error(err)
          return(NULL)
        })
    }, 
    
    updateLatest = function(pas, label, tz = 'UTC') {
      data[['latest']] <<- tryCatch(
        get_pat_latest(pas, label, tz), 
        error = function(err) { 
          logger.error(err)
          return(NULL)
        })
    }, 
    
    updateAnnual = function(pas, label, date) {
      data[['annual']] <<- tryCatch(
        get_pat_annual(pas, label, date) %>% 
          AirSensor::pat_createAirSensor(), 
        error = function(err) { 
          logger.error(err)
          return(NULL)
        })
    }, 
    
    updateLastInput = function(t) {
      lastInput <<- as.numeric(t)
    }
  )
)