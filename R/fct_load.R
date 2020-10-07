#' Get PurpleAir Synoptic Data
#' 
#' @param datestamp a datestamp (YYYYmmdd)
#' 
#' @return a pa_synoptic object
#' @export
get_pas <- function(datestamp = NULL) {
  # logger.trace("loading pas obj...")
  tryCatch(
    pas_load(datestamp), 
    error = function(err) catchError(err)
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
    "    get_pat(pas, %s, %s, %s, pat)",
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
          enddate = ed
        ) 
        
      }
      
      # }
      return(pat)
      
    }, 
    
    error = function(err) {
      catchError(err)
      # Fallback: Attempt loading with the default parameters. 
      pat <- tryCatch(
        pat_load(pas = pas, label = label), 
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
  # logger.trace("loading sensor obj...")
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
      
      ed <- ymd(ed)
      sd <- ymd(sd) 
      
      if ( is.null(sensors) ) {
        # logger.trace(paste(sd, ed, "loading sensors obj..."))
        sensors <- sensor_load(
          startdate = strftime(sd, "%Y%m%d"),
          enddate = strftime(ed, "%Y%m%d")
        ) 
      } else {
        data_sd <- lubridate::ymd_hms(min(sensors$data$datetime))
        data_ed <- lubridate::ymd_hms(max(sensors$data$datetime))
        
        if ( sd %within% (data_sd %--% data_ed) ) {
          # logger.trace(paste("filter date to", sd, "--", ed))
          sensors <- sensor_filterDate(
            sensor = sensors,
            startdate = strftime(sd, "%Y%m%d"),
            enddate = strftime(ed, "%Y%m%d")
          )
        } else {
          # logger.trace("reloading sensors obj...")
          sensors <- sensor_load(
            startdate = strftime(sd, "%Y%m%d"),
            enddate = strftime(ed, "%Y%m%d")
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
  # logger.trace(paste(label, "loading latest pat obj..."))
  tryCatch(
    pat_createNew(
      pas = pas, 
      label = label, 
      timezone = tz 
    ) %>% 
      pat_filterDate(lubridate::today(tzone = tz) - lubridate::days(2)),
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
get_noaa <- function(sensor, sd, ed) {
  # logger.trace("loading NOAA...")
  tryCatch(
    expr = {
      
      # Find wind data readings from the closest NOAA site
      yr <- year(ed)
      lon <- sensor$meta$longitude
      lat <- sensor$meta$latitude
      
      metaUrl <- "ftp://ftp.ncdc.noaa.gov/pub/data/noaa/isd-history.csv"
      meta <- fread(metaUrl)[ STATE == "CA"
      ][ (ymd(sd) %--% ymd(ed)) %within% (ymd(BEGIN) %--% ymd(END))
      ][, dist := geodist(cbind("Longitude" = LON, "Latitude" = LAT), cbind("Longitude" = lon, "Latitude" = lat), pad = TRUE) 
      ][ order(dist)
      ][, code := paste0(USAF, WBAN) ][1,]
      
      dataUrl <- paste0("https://www.ncei.noaa.gov/data/global-hourly/access/", yr, "/", meta$code, ".csv")
      # Install package bit64
      data <- fread(dataUrl)[, c("wd", "x", "y", "ws", "z") := tstrsplit(WND, ",")
      ][, wd := ifelse(as.numeric(wd) == 999, NA, as.numeric(wd))
      ][, ws := ifelse(as.numeric(ws) == 9999, NA, as.numeric(ws))/10 
      ][, c("air_temp", "flag_temp") := tstrsplit(TMP, ",")
      ][, air_temp := ifelse(as.numeric(air_temp) == 9999, NA, as.numeric(air_temp)/10)
      ][, c("dew_point", "flag_dew") := tstrsplit(DEW, ",")
      ][, dew_point := ifelse(as.numeric(dew_point) == 9999, NA, as.numeric(dew_point)/10)
      ][, date := ymd_hms(DATE) 
      ][, RH :=  100 * ((112 - 0.1 * air_temp + dew_point) / (112 + 0.9 * air_temp))^8 
      ][, c("date", "wd",  "ws", "air_temp", "RH") ]
      setcolorder(data, "date")
      
      dxts <- as.xts.data.table(data)
      ep <- endpoints(dxts, "hour")
      
      hourly <- as.data.table(do.call(
        cbind, 
        lapply(
          dxts, 
          function(x) { 
            period.apply(x, ep, function(x) { mean(x, na.rm = TRUE) }) 
          }
        )
      ))
      
      hourly[, date := round_date(index, "hour")
      ][, index := NULL 
      ][ date %between% c(sd, ed) ]
      
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
  tryCatch(
    PWFSLSmoke::monitor_load(sd, ed, id), 
    error = function(err) { catchError(err) }
  )
}
