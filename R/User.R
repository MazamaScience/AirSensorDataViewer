#' User R6 Class 
#' 
#' @description 
#' Create a user to act as a data and state storage object for AirSensor 
#' DataViewer Shiny Application.
#' 
#' @details 
#' Create an object that is responsible for housing the core state and data components of a 
#' Shiny applications user session. Each user is dependent on a Shiny session object. 
#' The methods and data of the object can be shared across modules without the requirment 
#' to create reactive-values. 
#'  
#' @export
#' @importFrom future future value
#' @importFrom AirSensor setArchiveBaseUrl
#' @import MazamaCoreUtils
User <- R6::R6Class(
  "User", 
  
  active = list(
    
    #' @details 
    #' \code{User$pas}: Return a future pa-synoptic object. 
    pas = function() {
      private$rx_pas$depend()
      return(private$pas_promise)
    },
    
    #' @details 
    #' \code{User$sensors}: Return a future airsensor object. 
    sensors = function() {
      private$rx_sensors$depend()
      return(private$sensors_promise)
    },
    
    #' @details 
    #' \code{User$pat}: Return a future pa_timeseries object. 
    pat = function() {
      private$rx_pat$depend()
      return(private$pat_promise)
    }, 
    
    #' @details 
    #' \code{User$sensor}: Return a future filtered airsensor object.
    sensor = function() {
      private$rx_sensor$depend() 
      return(private$sensor_promise)
    }, 
    
    #' @details 
    #' \code{User$pwfsl}: Return a future ws_monitor object from the PWFSLSmoke Package. 
    pwfsl = function() {
      private$rx_pwfsl$depend()
      return(private$pwfsl_promise)
    }, 
    
    #' @details 
    #' \code{User$latest}: Return a future latest pa-timeseries object (from \code{pat_createNew()}). 
    latest = function() {
      private$rx_latest$depend() 
      return(private$latest_promise)
    }, 
    
    #' @details 
    #' \code{User$annual}: Return an annual future airsensor object.
    annual = function() {
      private$rx_annual$depend()
      return(private$annual_promise)
    },
    
    #' @details 
    #' \code{User$noaa}: Return a future dataframe generated from NOAA Database. 
    noaa = function() {
      private$rx_noaa$depend()
      return(private$noaa_promise)
    }
  ),
  
  public = list(
    
    baseUrl = "http://data.mazamascience.com/PurpleAir/v1", 
    
    #' @details
    #' \code{User$selected$...}: Access Shiny Input reactive values.
    selected = NULL,
    
    #' @details 
    #' \code{User$url}: Access the current page URL for bookmarking state.
    url = NULL, 
    
    #' @details 
    #' \code{User$token}: Access the current session token ID. 
    token = NULL,
    
    #' @details 
    #' \code{User$url}: Access the {waiter} object for controling visual loading state.
    waiter = NULL,
    
    #' @details 
    #' Create a user. 
    #' @param session A Shiny session object.
    #' @param ... Additional options
    initialize = function(session, ...) {
      
      self$waiter <- waiter::Waitress$new(min = 0, max = 100)
      
      tz <- getOption('asdv.timezone')
      
      logger.debug(paste("START with session token:", session$token)) 
      self$token <- session$token
      
      # Create reactive values of the inputs
      self$selected <- reactiveValues(
        sensor = NULL,
        community = NULL,
        sd = NULL, 
        ed = NULL, 
        page = NULL, 
        tab = NULL, 
        year = 2020, 
        url = NULL
      )
      
      # Create the reactive trigger got each loaded value
      private$rx_pas <- reactiveTrigger()
      private$rx_sensors <- reactiveTrigger()
      private$rx_pat <- reactiveTrigger()
      private$rx_sensor <- reactiveTrigger()
      private$rx_pwfsl <- reactiveTrigger()
      private$rx_latest <- reactiveTrigger()
      private$rx_annual <- reactiveTrigger()
      private$rx_noaa <- reactiveTrigger()
      
      # Init load latest default pas and sensors
      private$pas_promise <- future({ 
        setArchiveBaseUrl(self$baseUrl)
        get_pas()
      })
      private$sensors_promise <- future({
        setArchiveBaseUrl(self$baseUrl)
        get_sensors(today(tzone=tz) - days(7), today(tzone=tz))
      })
    }, 
    
    #' @details 
    #' Update the users pa_synoptic data. 
    #' @param date YYYYmmdd.
    updatePas = function(date = NULL, tz = getOption("asdv.timezone")) {
      logger.debug("  ----- User::updatePas() -----")
      private$rx_pas$trigger()
      if ( !lubridate::ymd(date, tz = tz) < lubridate::ymd(20190505, tz = tz) ) {
        date <- strftime(date, "%Y%m%d", tz = tz) 
      } else {
        date <- 20190505
      }
      logger.trace("  date = %s", date)
      private$pas_promise <- future({
        setArchiveBaseUrl(self$baseUrl)
        get_pas(date)
      }, lazy = TRUE)
    }, 
    
    #' @details 
    #' Update the users airsensor object.
    #' @param sd startdate YYYYmmdd.
    #' @param ed enddate YYYYmmdd.
    #' @param ... additional params.
    updateSensors = function(sd, ed, ...) {
      logger.debug("  ----- User::updateSensors() -----")
      private$rx_sensors$trigger()
      private$sensors_promise <- future({
        setArchiveBaseUrl(self$baseUrl)
        get_sensors(sd, ed, ...)
      }, lazy = TRUE)
    },
    
    #' @details 
    #' Update the users pa_timeseries object.
    #' @param sd startdate YYYYmmdd.
    #' @param ed enddate YYYYmmdd.
    updatePat = function(label, sd, ed, tz = getOption("asdv.timezone")) {
      logger.debug("  ----- User::updatePat() -----")
      private$rx_pat$trigger()
      pas <- value(private$pas_promise)
      private$pat_promise <- future({
        setArchiveBaseUrl(self$baseUrl)
        get_pat(
          pas = pas,
          label = label,
          sd = sd,
          ed = ed
        )
      }, lazy = TRUE)
    }, 
    
    #' @details 
    #' Update the users filtered airsensor object. 
    #' Commonly used when a user selects a different sensor. 
    #' @param label the sensor label to filter the airsensor object by.
    updateSensor = function(label) {
      logger.debug("  ----- User::updateSensor() -----")
      logger.trace("  label = %s", label)
      private$rx_sensor$trigger()
      # Not really sure why, but this redef is absolutely necessary. 
      lab <- label
      sensors <- value(private$sensors_promise)
      private$sensor_promise <- future({
        sensor_filterMeta(sensors, .data$label == lab)
      }, lazy = TRUE)
      
    },
    
    #' @details 
    #' Update the users ws_monitor object.
    #' @param label ?
    #' @param sd startdate YYYYmmdd.
    #' @param ed enddate YYYYmmdd.
    updatePwfsl = function(label, sd, ed, tz = getOption("asdv.timezone")) {
      logger.debug("  ----- User::updatePwfsl() -----")
      private$rx_pwfsl$trigger()
      sensor <- value(private$sensor_promise)
      id <- sensor$meta$pwfsl_closestMonitorID
      private$pwfsl_promise <- future({
        get_pwfsl(sd, ed, id)
      }, lazy = TRUE)
    }, 
    
    #' @details 
    #' Update the users latest pa_timeseries object.
    #' @param label a label of the sensor.
    #' @param tz a specified timezone. (default: UTC)
    updateLatest = function(label, tz = getOption("asdv.timezone")) {
      logger.debug("  ----- User::updateLatest() -----")
      logger.trace("  tz = %s", tz)
      private$rx_latest$trigger()
      pas <- value(private$pas_promise)
      private$latest_promise <- future({
        get_pat_latest(pas = pas, label = label, tz = tz)
      }, lazy = TRUE)
    }, 
    
    #' @details 
    #' Update the users annual airsensor object.
    #' @param date a date to parse year from.
    updateAnnual = function(date, tz = getOption("asdv.timezone")) {
      logger.debug("  ----- User::updateAnnual() -----")
      private$rx_annual$trigger()
      # TODO:  Sort out precise datetimes to get a single year
      sd <- strftime(date, "%Y-01-02", tz = tz)
      ed <- strftime(date, "%Y-12-31", tz = tz)
      private$annual_promise <- future({
        sensor_load(
          collection = "scaqmd",
          startdate = sd, 
          enddate = ed,
          timezone = getOption("asdv.timezone"))
      }, lazy = TRUE)
    },
    
    #' @details 
    #' Update the users NOAA data.
    #' @param sd startdate YYYYmmdd.
    #' @param ed enddate YYYYmmdd.
    updateNoaa =  function(sd, ed) {
      logger.debug("  ----- User::updateNoaa() -----")
      private$rx_noaa$trigger()
      sensor <- value(private$sensor_promise)
      private$noaa_promise <- future({
        get_noaa(sensor, sd, ed)
      }, lazy = TRUE)
    }
    
  ), 
  
  # Private fields used to house the reactive trigger control and data promises
  private = list(
    rx_pas = NULL,
    rx_sensors = NULL, 
    rx_pat = NULL,
    rx_sensor = NULL, 
    rx_pwfsl = NULL, 
    rx_latest = NULL,
    rx_annual = NULL,
    rx_noaa = NULL,
    
    pas_promise = NULL,
    sensors_promise = NULL,
    pat_promise = NULL, 
    sensor_promise = NULL, 
    pwfsl_promise = NULL, 
    latest_promise = NULL, 
    annual_promise = NULL, 
    noaa_promise = NULL
  )
  
)


#' R6 reactive trigger: https://gist.github.com/bborgesr/3350051727550cfa798cb4c9677adcd4
#'
#' @export
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