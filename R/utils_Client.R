#' The Client Data Object
NULL

#'@export
Client <- R6::R6Class(
  "ClientObject",
  public = list(
    baseUrl = "http://data.mazamascience.com/PurpleAir/v1",
    token = NULL, 
    url = NULL,
    
    selected = rv(),
    data = rv(),
    
    initialize = function(session) {
      logger.trace("init")
      self$token <- session$token
      setArchiveBaseUrl(self$baseUrl)
      self$data$pas <- get_pas()
      self$data$sensors <- get_sensors(today() - days(7), today())
    },
    
    updatePas = function() {
      setArchiveBaseUrl(self$baseUrl)
      self$data$pas <- get_pas()
    }, 
    
    updateSensors = function(sd, ed) {
      setArchiveBaseUrl(self$baseUrl)
      self$data$sensors <- get_sensors(sd, ed)
    }, 
    
    updateSensor = function(sensors, ...) {
      self$data$sensor <- AirSensor::sensor_filterMeta(sensors, ...)
    }, 
    
    updatePat = function(pas, label, sd, ed) {
      setArchiveBaseUrl(self$baseUrl)
      self$data$pat <- get_pat(
        pas = pas,
        label = label,
        sd = sd,
        ed = ed
      )
    }, 
    
    updatePwfsl = function(id, sd, ed) {
      self$data$pwfsl <- PWFSLSmoke::monitor_load(sd, ed, id)
    }, 
    
    updateLatest = function(pas, label, tz = 'UTC') {
      self$data$latest <- get_pat_latest(pas, label, tz)
    }, 
    
    updateAnnual = function(pas, label, date) {
      self$data$annual <- get_pat_annual(pas, label, date) %>% 
        AirSensor::pat_createAirSensor()
    }
  )
)