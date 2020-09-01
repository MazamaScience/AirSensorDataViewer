#' Title
#'
#' @return
#' @export
#'
#' @examples
get_pas <- function(date = NULL) { 
  # logger.trace("loading pas obj...")
  tryCatch(
    pas_load(date), 
    error = function(err) catchError(err)
  )
}

#' Title
#'
#' @param pas 
#' @param label 
#' @param sd 
#' @param ed 
#'
#' @return
#' @export
#'
#' @examples
get_pat <- function(pas, label, sd, ed, pat = NULL) {
  
  # logger.trace(paste(label, sd, ed, "loading pat obj..."))
  
  sd <- ymd(sd)
  ed <- ymd(ed)
  
  tryCatch( 
    expr = {
      
      if ( is.null(pat) ) {
        pat <- pat_load(
          pas = pas, 
          label = label, 
          startdate = sd, 
          enddate = ed
        ) 
      } else {
        
        # Add label check
        data_sd <- ymd_hms(min(pat$data$datetime))
        data_ed <- ymd_hms(max(pat$data$datetime))
        
        if ( sd %within% (data_sd %--% data_ed) ) {
          # logger.trace(paste("pat filter date to", sd, "--", ed))
          pat <- pat_filterDate(
            pat = pat,
            startdate = strftime(sd, "%Y%m%d"),
            enddate = strftime(ed, "%Y%m%d")
          )
        } else {
          # logger.trace("reloading pat obj...")
          pat <- pat_load(
            pas = pas, 
            label = label, 
            startdate = sd, 
            enddate = ed
          ) 
        }
      }
      return(pat)
      
    }, 
    error = function(err) {
      catchError(err)
    }
  )
}

#' Title
#'
#' @param sensors 
#' @param label 
#' @param sd 
#' @param ed 
#'
#' @return
#' @export
#'
#' @examples
get_sensor <- function(sensors, ...) {
  # logger.trace("loading sensor obj...")
  tryCatch(
    sensor_filterMeta(sensors, ...),
    error = function(err) catchError(err)
  )
}

#' Title
#'
#' @param sd 
#' @param ed 
#'
#' @return
#' @export
#'
#' @examples
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
        data_sd <- ymd_hms(min(sensors$data$datetime))
        data_ed <- ymd_hms(max(sensors$data$datetime))
        
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
  )
}

#' Title
#'
#' @param pas 
#' @param label 
#' @param date 
#'
#' @return
#' @export
#'
#' @examples
get_pat_annual <- function(pas, label, date) {
  get_pat(pas, label, "20180101", "20181231")
}


#' Title
#'
#' @param pas 
#' @param label 
#' @param tz 
#'
#' @return
#' @export
#'
#' @examples
get_pat_latest <- function(pas, label, tz = 'UTC') {
  # logger.trace(paste(label, "loading latest pat obj..."))
  tryCatch(
    pat_createNew(
      pas = pas, 
      label = label, 
      timezone = tz 
    ), 
    error = function(err) catchError(err)
  )
}

#' Title
#'
#' @param sensor 
#'
#' @return
#' @export
#'
#' @examples
#' 
#' @importFrom worldmet importNOAA getMeta 
#' @importFrom lubridate ymd `%--%` `%within%` year ymd_hms round_date
#' @importFrom data.table fread data.table tstrsplit `%between%`
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
            period.apply(x, ep, function(x) { mean(x, na.rm=TRUE) }) 
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

#' Title
#'
#' @param sd 
#' @param ed 
#' @param id 
#'
#' @return
#' @export
#'
#' @examples
get_pwfsl <- function(sd, ed, id) {
  tryCatch(
    PWFSLSmoke::monitor_load(sd, ed, id), 
    error = function(err) catchError(err)
  )
}
