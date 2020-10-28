#' Comparison Leaflet
#' 
#' @description Map an airsensor and ws_monitor object.
#'
#' @param sensor An airsensor object.
#' @param pwfsl A ws_monitor object.
#'
#' @return a leaflet map
#' @export
#' @import leaflet
comparisonLeaflet <- function(sensor, pwfsl) {
  
  d <- sensor
  h <- pwfsl
  
  slab <- d$meta$label
  mdist <- signif(d$meta$pwfsl_closestDistance/1000, 2)
  mlab <- h$meta$siteName
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
      label = mlab
    ) %>% 
    addAwesomeMarkers(
      lng = ~unique(longitude.pa), 
      lat = ~unique(latitude.pa), 
      icon = makeAwesomeIcon(markerColor = "purple", icon = 'flag'), 
      label = slab
    ) %>% 
    addPolylines(
      lng = c(unique(df$longitude.pwfsl), unique(df$longitude.pa)), 
      lat = c(unique(df$latitude.pwfsl), unique(df$latitude.pa)),
      dashArray = "12", 
      color = 'red', 
      label = paste0(mdist, " km")
    )
}
