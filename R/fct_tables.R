#' Monitor Sensor Comparison Table
#'
#' @param pat pa timeseries obh
#'
#' @return a data.frame
#' @export
#' 
#' @importFrom utils head tail
sensorMonitorCompTable <- function(pat) {
  
  logger.debug('----- sensorMonitorCompTable() -----')
  
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
        from = head(df$datetime, n = 1),
        to = tail(df$datetime, n = 1),
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
#' @param noaa a noaa data frame generated from get_noaa().
#'
#' @return a data.frame
#' @export
noaaTable <- function(noaa) {

  logger.debug('----- noaaTable() -----')


  base::suppressWarnings({

    noaaTable <-
      data.frame(
        
        mean_ws = mean(noaa$ws, na.rm = TRUE),
        min_ws = min(noaa$ws, na.rm = TRUE),
        max_ws = max(noaa$ws, na.rm = TRUE),
        
        mean_wd = mean(noaa$wd, na.rm = TRUE),
        
        mean_air_temp = mean(noaa$air_temp, na.rm = TRUE),
        min_air_temp = min(noaa$air_temp, na.rm = TRUE),
        max_air_temp = max(noaa$air_temp, na.rm = TRUE),
        
        mean_RH = mean(noaa$RH, na.rm = TRUE),
        min_RH = min(noaa$RH, na.rm = TRUE),
        max_RH = max(noaa$RH, na.rm = TRUE)
      )
      
  })  
  
  # Handle +/-Inf and NaN values that occur when a vector is all NAs
  # TODO:  Conversion of Inf/Nan to NA isn't currently working
  noaaTable$mean_ws[is.nan(noaaTable$mean_ws)] <- NA
  noaaTable$min_ws[is.infinite(noaaTable$min)] <- NA
  noaaTable$max_ws[is.infinite(noaaTable$max)] <- NA
  
  noaaTable$mean_wd[is.nan(noaaTable$mean_wd)] <- NA
   
  noaaTable$mean_air_temp[is.nan(noaaTable$mean_air)] <- NA
  noaaTable$min_air_temp[is.infinite(noaaTable$min)] <- NA
  noaaTable$max_air_temp[is.infinite(noaaTable$max)] <- NA
   
  noaaTable$mean_RH[is.nan(noaaTable$mean_RH)] <- NA
  noaaTable$min_RH[is.infinite(noaaTable$min)] <- NA
  noaaTable$max_RH[is.infinite(noaaTable$max)] <- NA
  
  
  names(noaaTable) <- c( 
    "Average Wind Speed (m/s)",
    "Minimum Wind Speed (m/s)",
    "Maximum Wind Speed (m/s)",
    
    "Average Wind Direction (deg)",
    
    "Average Temperature (C)",
    "Minimum Temperature (C)",
    "Maximum Temperature (C)",
    
    "Average Humidity (%)",
    "Minimum Humidity (%)",
    "Maximum Humidity (%)" 
  )
  
  return(t(noaaTable))
  
}

