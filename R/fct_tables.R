#' Monitor Sensor Comparison Table
#'
#' @param pat pa timeseries obh
#'
#' @return
#' @export
sensorMonitorCompTable <- function(pat) {
  
  # ----- Validate parameters --------------------------------------------------
  
  if ( !AirSensor::pat_isPat(pat) )
    stop("Parameter 'pat' is not a valid 'pa_timeseries' object.")
  
  if ( AirSensor::pat_isEmpty(pat) )
    stop("Parameter 'pat' has no data.")
  
  df <- pat$data
  
  # Fill the missing date rows with NA
  complete_df <-
    tidyr::complete(
      data = df,
      datetime = seq(
        from = head(df$datetime, n=1),
        to = tail(df$datetime, n=1),
        by = "1 min"
      )
    )
  
  comparisonTable <-
    data.frame(
      pat$meta$label,
      nrow(pat$data),
      nrow(pat$data)/nrow(complete_df)*100
    )
  
  names(comparisonTable) <-
    c( "Sensor",
       "Measurement Count",
       "Recovered (%)" )
  
  return(t(comparisonTable))
}

#' NOAA table constructor
#'
#' @param noaa 
#'
#' @return
#' @export
noaaTable <- function(noaa) {
  
  noaaTable <-
    data.frame(
      
      mean(noaa$ws, na.rm = TRUE),
      min(noaa$ws, na.rm = TRUE),
      max(noaa$ws, na.rm = TRUE),
      
      mean(noaa$wd, na.rm = TRUE),
      
      mean(noaa$air_temp, na.rm = TRUE),
      min(noaa$air_temp, na.rm = TRUE),
      max(noaa$air_temp, na.rm = TRUE),
      
      mean(noaa$RH, na.rm = TRUE),
      min(noaa$RH, na.rm = TRUE),
      max(noaa$RH, na.rm = TRUE)
    )
  
  names(noaaTable) <-
    c( "Average Wind Speed (m/s)",
       "Minimum Wind Speed (m/s)",
       "Maximum Wind Speed (m/s)",
       
       "Average Wind Direction (deg)",
       
       "Average Temperature (C)",
       "Minimum Temperature (C)",
       "Maximum Temperature (C)",
       
       "Average Humidity (%)",
       "Minimum Humidity (%)",
       "Maximum Humidity (%)" )
  
  return(t(noaaTable))
  
}

