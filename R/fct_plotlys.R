#' Channel Graph Plotly
#'
#' @param pat a pa_timeseries object
#' @param channel pm channel 
#'
#' @return a plotly
#' @export
#' 
#' @importFrom plotly plot_ly add_trace config
#' @importFrom lubridate with_tz
#' @importFrom stringr str_to_lower
channelPlotly <- function(pat, channel = "ab") {
  class(pat) <- c(str_to_lower(channel), class(pat))
  UseMethod("channelPlotly", pat)
}

#' @method channelPlotly a
channelPlotly.a <- function(pat, ...) {
  pat$data$datetime <- with_tz(pat$data$datetime, tzone = 'UTC')
  plot_ly(
    pat$data,
    x = ~datetime,
    y = ~pm25_A,
    type = "scatter",
    mode = "lines",
    line = list(color = "red"),#"#ba2e00"),
    name = "Channel A",
    opacity = 0.65 
  ) %>%
    config(displayModeBar = FALSE) %>%
    plotly::layout( 
      title = list(text = paste0(pat$meta$label, " Latest Data")),
      legend = list(orientation = 'h'),
      xaxis = list(title = "Date", titlefont = list(size = 14.5)),
      yaxis = list(title = "PM<sub>2.5</sub> (\u03bcg / m\u00b3)", 
                   titlefont = list(size = 14.5)) 
    )
}

#' @method channelPlotly b
channelPlotly.b <- function(pat, ...) {
  pat$data$datetime <- with_tz(pat$data$datetime, tzone = 'UTC')
  plot_ly(
    pat$data,
    x = ~datetime,
    y = ~pm25_A,
    type = "scatter",
    mode = "lines",
    line = list(color = "blue"),#"#ba2e00"),
    name = "Channel B",
    opacity = 0.65 
  ) %>%
    config(displayModeBar = FALSE) %>%
    plotly::layout( 
      title = list(text = paste0(pat$meta$label, " Latest Data")),
      legend = list(orientation = 'h'),
      xaxis = list(title = "Date", titlefont = list(size = 14.5)),
      yaxis = list(title = "PM<sub>2.5</sub> (\u03bcg / m\u00b3)", 
                   titlefont = list(size = 14.5)) 
    )
}

#' @method channelPlotly ab
channelPlotly.ab <- function(pat, ...) {
  pat$data$datetime <- with_tz(pat$data$datetime, tzone = 'UTC')
  plot_ly(
    pat$data,
    x = ~datetime,
    y = ~pm25_A,
    type = "scatter",
    mode = "lines",
    line = list(color = "red"),#"#ba2e00"),
    name = "Channel A",
    opacity = 0.65 
  ) %>%
    add_trace( 
      y = ~pm25_B,
      line = list(color = "blue"),#"#008cba"),
      name = "Channel B" 
    ) %>%
    config(displayModeBar = FALSE) %>%
    plotly::layout( 
      title = list(text = paste0(pat$meta$label, " Latest Data")),
      legend = list(orientation = 'h'),
      xaxis = list(title = "Date", titlefont = list(size = 14.5)),
      yaxis = list(title = "PM<sub>2.5</sub> (\u03bcg / m\u00b3)", 
                   titlefont = list(size = 14.5)) 
    )
}

#' Humidity Graph Plotly
#'
#' @param pat a pa_timeseries object
#'
#' @return a plotly
#' @export
#' 
#' @importFrom plotly plot_ly add_trace config
#' @importFrom lubridate with_tz
humidityPlotly <- function(pat) {
  pat$data$datetime <- with_tz(pat$data$datetime, tzone = 'UTC')
  plot_ly( 
    pat$data,
    x = ~datetime,
    y = ~humidity,
    type = "scatter",
    mode = "lines",
    line = list(color = "black"),
    opacity = 0.65 
  ) %>%
    config(displayModeBar = FALSE) %>%
    plotly::layout( 
      title = list(text = "Humidity"),
      xaxis = list(title = "Date", titlefont = list(size = 14.5)),
      yaxis = list(title = "RH (%)", titlefont = list(size = 14.5)) 
    )
}

#' Temperature Graph Plotly
#'
#' @param pat a pa_timeseries object
#'
#' @return a plotly
#' @export
#' 
#' @importFrom plotly plot_ly add_trace config
#' @importFrom lubridate with_tz
temperaturePlotly <- function(pat) {
  pat$data$datetime <- with_tz(pat$data$datetime, tzone = 'UTC')
  plot_ly( 
    pat$data,
    x = ~datetime,
    y = ~temperature,
    type = "scatter",
    mode = "lines",
    line = list(color = "black"),
    opacity = 0.65 
  ) %>%
    config(displayModeBar = FALSE) %>%
    plotly::layout( 
      title = list(text = "Temperature"),
      xaxis = list(title = "Date", titlefont = list(size = 14.5)),
      yaxis = list(title = "Temperature (F)", titlefont = list(size = 14.5)) 
    )
}


