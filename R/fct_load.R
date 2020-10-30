#' Get PurpleAir Synoptic Data
#' 
#' @param datestamp a datestamp (YYYYmmdd)
#' 
#' @return a pa_synoptic object
#' @export
get_pas <- function(datestamp = NULL) {
  logger.trace("  get_pas(%s)", datestamp)
  
  timezone <- getOption("asdv.timezone")
  
  tryCatch(
    pas_load(datestamp, timezone = timezone), 
    error = function(err) { catchError(err) }
  )
}

#' Get PurpleAir Timeseries Data
#'
#' @param pas a pas object.
#' @param label a sensor label
#' @param sd a startdate (YYYYmmdd)
#' @param ed a enddate (YYYYmmdd)
#' @param pat an optional pat to attempt filtering on. 
#'
#' @return a pa_timeseries object
#' @export
get_pat <- function(pas, label, sd, ed, pat = NULL) {
  
  timezone <- getOption("asdv.timezone")
  
  sd <- MazamaCoreUtils::parseDatetime(sd, timezone = timezone)
  ed <- MazamaCoreUtils::parseDatetime(ed, timezone = timezone)
  
  logger.trace(
    "  get_pat(pas, %s, %s, %s, pat)",
    label,
    strftime(sd, "%Y-%m-%d %H:00", tz = timezone, usetz = TRUE),
    strftime(ed, "%Y-%m-%d %H:00", tz = timezone, usetz = TRUE)
  )
  
  tryCatch( 
    expr = {
      
      # NOTE:  If the passed in pat already exists and covers the requeste date
      # NOTE:  range, use it.  Otherwise. Download a new pat.
      
      if ( !is.null(pat) &&
           ed <= max(pat$data$datetime, na.rm = TRUE) && 
           sd >= min(pat$data$datetime, na.rm = TRUE) ) {
          
        pat <- pat_filterDatetime(
          pat = pat,
          startdate = sd,
          enddate = ed,
          timezone = timezone
        )
        
      } else {
        
        pat <- pat_load(
          pas = pas, 
          label = label, 
          startdate = sd, 
          enddate = ed, 
          timezone = timezone
        ) 
        
      }
      
      return(pat)
      
    }, 
    
    error = function(err) {
      catchError(err)
      # Fallback: Attempt loading with the default parameters. 
      pat <- tryCatch(
        pat_load(pas = pas, label = label, timezone = timezone), 
        error = function(err) { catchError(err) }
      )
      return(pat)
    }
    
  ) # END tryCatch
  
}

#' Get A Sensor from Sensors 
#'
#' @param sensors an airsensor object
#' @param ... filtering argument.
#'
#' @return a univariate airsensor object
#' @export
get_sensor <- function(sensors, ...) {
  logger.trace("  get_sensor(...)")
  tryCatch(
    sensor_filterMeta(sensors, ...),
    error = function(err) { catchError(err) }
  )
}

#' Get Sensors for date range
#'
#' @param sd a startdate (YYYYmmdd)
#' @param ed an enddate (YYYYmmdd)
#' @param sensors an optional sensors object to attmept filtering by.
#'
#' @return an airsensor object
#' @export
#' @importFrom lubridate ymd_hms
get_sensors <- function(sd, ed, sensors = NULL) {
  
  tryCatch(
    
    expr = {
      
      timezone <- getOption("asdv.timezone")
      
      sd <- MazamaCoreUtils::parseDatetime(sd, timezone = timezone)
      ed <- MazamaCoreUtils::parseDatetime(ed, timezone = timezone)
      
      logger.trace(
        "  get_sensors(%s, %s)",
        strftime(sd, "%Y%m%d%H", tz = timezone, usetz = TRUE),
        strftime(ed, "%Y%m%d%H", tz = timezone, usetz = TRUE)
      )
      
      if ( is.null(sensors) ) {
        # logger.trace(paste(sd, ed, "loading sensors obj..."))
        sensors <- sensor_load(
          startdate = strftime(sd, "%Y%m%d", tz = timezone),
          enddate = strftime(ed, "%Y%m%d", tz = timezone), 
          timezone = timezone
        ) 
      } else {
        data_sd <- lubridate::ymd_hms(min(sensors$data$datetime), tz = timezone)
        data_ed <- lubridate::ymd_hms(max(sensors$data$datetime), tz = timezone)
        
        if ( sd %within% (data_sd %--% data_ed) ) {
          # logger.trace(paste("filter date to", sd, "--", ed))
          sensors <- sensor_filterDate(
            sensor = sensors,
            startdate = strftime(sd, "%Y%m%d", tz = timezone),
            enddate = strftime(ed, "%Y%m%d", tz = timezone)
          )
        } else {
          # logger.trace("reloading sensors obj...")
          sensors <- sensor_load(
            startdate = strftime(sd, "%Y%m%d", tz = timezone),
            enddate = strftime(ed, "%Y%m%d", tz = timezone), 
            timezone = timezone
          ) 
        }
      }
      return(sensors)
    }, 
    
    error = function(err) {
      catchError(err)
    }
    
  ) # END tryCatch
  
}

#' #' Get Annual 
#' #'
#' #' @param pas a pas obj
#' #' @param label a sensor label
#' #' @param date a date in the format 
#' #'
#' #' @return
#' #' @export
#' #'
#' #' @examples
#' get_pat_annual <- function(pas, label, date) {
#'   get_pat(pas, label, "20180101", "20181231")
#' }


#' Get Latest PurpleAir Timeseries Data
#'
#' @param pas a pas obj
#' @param label a sensor label
#' @param tz a timezone 
#'
#' @return a 'pat' object
#' @export
get_pat_latest <- function(pas, label, tz = "America/Los_Angeles") {
  logger.trace("  get_pat_latest(pas, %s, %s", label, tz)
  ed <- lubridate::now()
  sd <- ed - lubridate::days(2)
  tryCatch(
    pat_createNew(
      pas = pas, 
      label = label, 
      startdate = sd,
      enddate = ed
    ),
    error = function(err) catchError(err)
  )
}

#' Get NOAA Data From ISD
#'  
#'  A homebrew solution to the acquiring NOAA data from their database, following
#'  the worldmet importNOAA function.
#'
#' @param sensor A univariate airsensor object
#' @param sd A startdate. 
#' @param ed A enddate. 
#'
#' @return a data.frame 
#' @export
#' @importFrom lubridate ymd `%--%` `%within%` year ymd_hms round_date
#' @importFrom data.table fread data.table tstrsplit `%between%` setcolorder as.xts.data.table as.data.table
#' @importFrom geodist geodist
#' @importFrom xts period.apply endpoints
#' @importFrom worldmet getMeta importNOAA
get_noaa <- function(sensor, sd, ed) {
  
  timezone <- getOption("asdv.timezone")
  
  sd <- MazamaCoreUtils::parseDatetime(sd, timezone = timezone)
  ed <- MazamaCoreUtils::parseDatetime(ed, timezone = timezone)
  
  logger.trace(
    "  get_noaa(sensor, %s, %s)",
    strftime(sd, "%Y%m%d%H", tz = timezone, usetz = TRUE),
    strftime(ed, "%Y%m%d%H", tz = timezone, usetz = TRUE)
  )
  
  year <- strftime(sd, "%Y", tz = timezone)
  
  tryCatch(
    expr = {
      # TODO:  This is where we would like to have status messages sent to the
      # TODO:  UI to alert the user that the delay is NOAA's fault, not ours.
      logger.trace("  * worldmet::getMeta()")
      meta <- worldmet::getMeta(lat = sensor$meta$latitude, lon = sensor$meta$longitude, plot = FALSE, n = 1)
      logger.trace("  * worldmet::importNoaa()")
      data <- worldmet::importNOAA(code = meta$code, year = as.numeric(year), quiet = TRUE)
      logger.trace("  * worldmet::importNoaa() FINISHED")
      dataSubset <- data %>% dplyr::filter(date >= sd & date <= ed)
      return(dataSubset)
    }, 
    error = function(err) {
      catchError(err)
    }
  )
}

#' Get PWFSL Monitor Data
#'
#' @param sd a startdate (YYYYmmdd)
#' @param ed an enddate (YYYYmmdd)
#' @param id a valid PWFSL Monitor ID
#'
#' @return a ws_monitor object
#' @export
get_pwfsl <- function(sd, ed, id) {
  logger.trace("  get_pwfsl(%s, %s, %s)", sd, ed, id)
  tryCatch(
    PWFSLSmoke::monitor_load(sd, ed, id), 
    error = function(err) { catchError(err) }
  )
}
