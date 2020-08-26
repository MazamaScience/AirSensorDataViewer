#' Title
#'
#' @param sensor 
#' @param pwfsl 
#'
#' @return
#' @export
#' @import leaflet
#' @examples
comparisonLeaflet <- function(sensor, pwfsl) {
  
  d <- sensor
  h <- pwfsl
  
  slab <- d$meta$label
  mdist <- signif(d$meta$pwfsl_closestDistance/1000, 2)
  mlab <- d$meta$pwfsl_closestMonitorID
  sensor <- PWFSLSmoke::monitor_toTidy(d)
  dates <- range(d$data$datetime)
  monitor <- PWFSLSmoke::monitor_toTidy(h)
  df <- dplyr::left_join(sensor, monitor, by = 'datetime', suffix = c('.pwfsl', '.pa'))
  
  leaflet(df) %>% 
    addTiles() %>% 
    addAwesomeMarkers(
      lng = ~unique(longitude.pwfsl), 
      lat = ~unique(latitude.pwfsl), 
      icon = makeAwesomeIcon(markerColor = "gray", icon = 'flag'), 
      popup = mlab
    ) %>% 
    addAwesomeMarkers(
      lng = ~unique(longitude.pa), 
      lat = ~unique(latitude.pa), 
      icon = makeAwesomeIcon(markerColor = "purple", icon = 'flag'), 
      popup = slab
    ) %>% 
    addPolylines(
      lng = c(unique(df$longitude.pwfsl), unique(df$longitude.pa)), 
      lat = c(unique(df$latitude.pwfsl), unique(df$latitude.pa)),
      dashArray = "12", 
      color = 'red', 
      popup = paste0(mdist, " km")
    )
}