#' Load NOAA data from Sensor Data
#'
#' @param sensor A sensor obj.
#' @param startdate Startdate ISO
#' @param enddate Enddate ISO
#' @param tz timezone
#'
#' @return NOAA data frame
#' @export
#' 
#' @importFrom lubridate year ymd ymd_hms
#' @importFrom worldmet getMeta importNOAA
#' @importFrom dplyr filter
loadNOAA <- function(sensor) {
    sd <- min(ymd_hms(sensor$data$datetime))
    ed <- max(ymd_hms(sensor$data$datetime))
    # Find wind data readings from the closest NOAA site
    year <- year(ed)
    lon <- sensor$meta$longitude
    lat <- sensor$meta$latitude
    closestSite <- getMeta(lon = lon, lat = lat, n = 1, plot = FALSE)[1,]
    siteCode <- closestSite$code
    siteData <- importNOAA(code = siteCode, year = year, n.cores = future::availableCores() - 1) 
    return(siteData)
}

