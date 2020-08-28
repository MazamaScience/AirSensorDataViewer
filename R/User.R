#'@export
#'@importFrom future future value
#'@importFrom R6 R6Class
#'@importFrom AirSensor setArchiveBaseUrl
#'@import MazamaCoreUtils
User <- R6Class(
  "User", 
  
  active = list(
    pas = function() {
      private$rx_pas$depend()
      value(private$pas_promise)
    },
    sensors = function() {
      private$rx_sensors$depend()
      value(private$sensors_promise)
    },
    pat = function() {
      private$rx_pat$depend()
      value(private$pat_promise)
    }, 
    sensor = function() {
      private$rx_sensor$depend() 
      value(private$sensor_promise)
    }, 
    pwfsl = function() {
      private$rx_pwfsl$depend()
      value(private$pwfsl_promise)
    }, 
    latest = function() {
      private$rx_latest$depend() 
      value(private$latest_promise)
    }
  ),
  
  public = list(
    
    baseUrl = "http://data.mazamascience.com/PurpleAir/v1", 
    selected = NULL,
    url = NULL, 
    tz = NULL, 
    token = NULL,
    trigger = NULL,
    
    initialize = function(session) {
      
      self$token <- session$token
      
      self$selected <- reactiveValues(
        sensor = NULL,
        community = NULL,
        sd = NULL, 
        ed = NULL, 
        page = NULL, 
        tab = NULL
      )
      
      private$rx_pas <- reactiveTrigger()
      private$rx_sensors <- reactiveTrigger()
      private$rx_pat <- reactiveTrigger()
      private$rx_sensor <- reactiveTrigger()
      private$rx_pwfsl <- reactiveTrigger()
      private$rx_latest <- reactiveTrigger()
      
      private$pas_promise <- future({ 
        setArchiveBaseUrl(self$baseUrl)
        tryCatch(
          expr = {
            get_pas()
          }, 
          error = function(err) { 
            logger.error(err)
            return(NULL)
          }
        )
      })
      
      private$sensors_promise <- future({
        setArchiveBaseUrl(self$baseUrl)
        tryCatch(
          expr = { 
            get_sensors(today() - days(7), today())
          }, 
          error = function(err) { 
            logger.error(err)
            return(NULL)
          }
        )
      })
    }, 
    
    # -- Update the PAS promise
    updatePas = function() {
      logger.trace(paste("Updating pas ===>"))
      private$rx_pas$trigger()
      private$pas_promise <- future({
        setArchiveBaseUrl(self$baseUrl)
        tryCatch(
          expr = {
            get_pas()
          }, 
          error = function(err) { 
            logger.error(err)
            return(NULL)
          }
        )
      })
    }, 
    
    # -- Update the sensors promise
    updateSensors = function(sd, ed) {
      logger.trace(paste("Updating sensors ===>", sd, ed))
      private$rx_sensors$trigger()
      private$sensors_promise <- future({
        setArchiveBaseUrl(self$baseUrl)
        tryCatch(
          expr = {
            get_sensors(sd, ed)
          }, 
          error = function(err) { 
            logger.error(err)
            return(NULL)
          }
        )
      })
    },
    
    # -- Update the PAT promise
    updatePat = function(pas, label, sd, ed) {
      logger.trace(paste("Updating pat ===>", label, sd, ed))
      private$rx_pat$trigger()
      private$pat_promise <- future({
        setArchiveBaseUrl(self$baseUrl)
        tryCatch(
          expr = {
            get_pat(
              pas = pas,
              label = label,
              sd = sd,
              ed = ed
            )
          }, 
          error = function(err) { 
            logger.error(err)
            return(NULL)
          }
        ) 
      })
    }, 
    
    updateSensor = function(sensors, label) {
      logger.trace(paste("updating sensor ===>", label))
      private$rx_sensor$trigger()
      # Not really sure why, but this redef is absolutely neccesary. 
      lab <- label
      private$sensor_promise <- future({
        tryCatch(
          expr = {
            sensor_filterMeta(sensors, .data$label == lab)
          }, 
          error = function(err) {
            logger.error(err)
            NULL
          }
        )
      }
      )
      
    },
    
    updatePwfsl = function(id, sd, ed) {
      logger.trace(paste("Updating pwfsl ===>", id, sd, ed))
      private$rx_pwfsl$trigger()
      private$pwfsl_promise <- future({
        tryCatch(
          PWFSLSmoke::monitor_load(sd, ed, id), 
          error = function(err) { 
            logger.error(err)
            return(NULL)
          }
        )
      })
    }, 
    
    updateLatest = function(pas, label, tz = 'UTC') {
      logger.trace(paste("Updating latest ===>", label))
      private$rx_latest$trigger()
      private$latest_promise <- future({
        tryCatch(
          get_pat_latest(pas, label, tz), 
          error = function(err) { 
            logger.error(err)
            return(NULL)
          }
        )
      })
    }, 
    
    setTz = function(timezone) {
      logger.trace(paste("setting timezone ===>", timezone))
      self$tz <- timezone
    }
    
  ), 
  
  private = list(
    
    rx_pas = NULL,
    rx_sensors = NULL, 
    rx_pat = NULL,
    rx_sensor = NULL, 
    rx_pwfsl = NULL, 
    rx_latest = NULL,
    
    pas_promise = NULL,
    sensors_promise = NULL,
    pat_promise = NULL, 
    sensor_promise = NULL, 
    pwfsl_promise = NULL, 
    latest_promise = NULL
    
  ), 
)


#' R6 reactive trigger: https://gist.github.com/bborgesr/3350051727550cfa798cb4c9677adcd4
#'
#' @return
#' @export
#'
#' @examples
reactiveTrigger <- function() {
  counter <- reactiveVal(0)
  list(
    depend = function() {
      counter()
      invisible()
    },
    trigger = function() {
      counter( isolate(counter()) + 1 )
    }
  )
}
