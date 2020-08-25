#' Title
#'
#' @return
#' @export
#'
#' @examples
get_pas <- function() { 
  logger.trace("loading pas obj...")
  pas_load()
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

  logger.trace(paste(label, sd, ed, "loading pat obj..."))
  
  sd <- ymd(sd)
  ed <- ymd(ed)
  
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
      logger.trace(paste("pat filter date to", sd, "--", ed))
      pat <- pat_filterDate(
        pat = pat,
        startdate = strftime(sd, "%Y%m%d"),
        enddate = strftime(ed, "%Y%m%d")
      )
    } else {
      logger.trace("reloading pat obj...")
      pat <- pat_load(
        pas = pas, 
        label = label, 
        startdate = sd, 
        enddate = ed
      ) 
    }
  }
  return(pat)
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
  logger.trace("loading sensor obj...")
  sensor_filter(sensors, ...) 
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
  
  ed <- ymd(ed)
  sd <- ymd(sd) 
  
  if ( is.null(sensors) ) {
    logger.trace(paste(sd, ed, "loading sensors obj..."))
    sensors <- sensor_load(
      startdate = strftime(sd, "%Y%m%d"),
      enddate = strftime(ed, "%Y%m%d")
    ) 
  } else {
    data_sd <- ymd_hms(min(sensors$data$datetime))
    data_ed <- ymd_hms(max(sensors$data$datetime))
    
    if ( sd %within% (data_sd %--% data_ed) ) {
      logger.trace(paste("filter date to", sd, "--", ed))
      sensors <- sensor_filterDate(
        sensor = sensors,
        startdate = strftime(sd, "%Y%m%d"),
        enddate = strftime(ed, "%Y%m%d")
      )
    } else {
      logger.trace("reloading sensors obj...")
      sensors <- sensor_load(
        startdate = strftime(sd, "%Y%m%d"),
        enddate = strftime(ed, "%Y%m%d")
      ) 
    }
  }
  return(sensors)
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
  logger.trace("loading annual pat obj...")
  # sd <- strftime(ymd(date), "%Y0101")
  # ed <- strftime(ymd(date), "%Y1231")
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
  logger.trace(paste(label, "loading latest pat obj..."))
  pat_createNew(
    pas = pas, 
    label = label, 
    timezone = tz 
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
get_noaa <- function(sensor) {
  logger.trace("loading NOAA...")
  sd <- min(ymd_hms(sensor$data$datetime))
  ed <- max(ymd_hms(sensor$data$datetime))
  # Find wind data readings from the closest NOAA site
  year <- year(ed)
  lon <- sensor$meta$longitude
  lat <- sensor$meta$latitude
  closestSite <- getMeta(lon = lon, lat = lat,n = 1, plot = FALSE)
  siteCode <- closestSite$code
  siteData <- importNOAA(code = siteCode, year = year, n.cores = 1) 
  return(siteData)
}

# filter_date <- function(x, sd, ed) {
#   UseMethod("filter_date", x)
# }
# filter_date.airsensor <- function(x, sd, ed) {
#   data_sd <- ymd_hms(min(x$data$datetime))
#   data_ed <- ymd_hms(max(x$data$datetime))
#   
#   ed <- ymd(input$date_range[1])
#   sd <- ymd(input$date_range[2])
#   
#   if ( sd %within% (data_sd %--% data_ed) ) {
#     sensor <- sensor_filterDate(
#       sensor = x, 
#       startdate = strftime(sd, "%Y%m%d"),
#       enddate = strftime(ed, "%Y%m%d")
#     )
#     # otherwise reload pat obj with domain selections
#   } else {
#     get_sensor()
#   }
#   return(sensor)
# }

# }
# filter_date.pa_timeseries <- function(x, sd, ed) {
#   
# }