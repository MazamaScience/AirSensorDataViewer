#' @export
#' @importFrom rlang .data
#' @importFrom stats lm
#' @import dplyr
#' @import graphics
#'
#' @title Linear model fitting of channel A and B time series data
#'
#' @param pat PurpleAir Timeseries \emph{pat} object.
#' @param showPlot Logical specifying whether to generate a model fit plot.
#' @param size Size of points.
#' @param a_color Color of time series channel A points.
#' @param b_color Color of time series channel B points.
#' @param alpha Opacity of points.
#' @param lr_shape Symbol to use for linear regression points.
#' @param lr_color Color of linear regression points.
#' @param lr_lwd Width of linear regression line.
#' @param lr_lcolor Color of linear regression line.
#' @param lr_lalpha Opacity of linear regression line.
#' @param ts_shape Symbol to use for time series points.
#' @param xylim Vector of (lo,hi) limits used as limits on the correlation plot
#' axes -- useful for zooming in.
#'
#' @description Uses a linear model to fit data from channel B to data from
#' channel A.
#'
#' A diagnostic plot is produced if `showPlot = TRUE`.
#'
#' @return A linear model, fitting the `pat` B channel readings to A channel
#' readings.
#'
#' @examples
#' \donttest{
#' asdv_internalFit(pat = example_pat)
#' }

asdv_internalFit <- function(
  pat = NULL,
  showPlot = TRUE,
  whichPlot = "lm",
  size = 1,
  a_color = "red",
  b_color = "blue",
  alpha = 0.25,
  lr_shape = 15,
  lr_color = "black",
  lr_lwd = 1.5,
  lr_lcolor = "tomato",
  lr_lalpha = 0.45,
  ts_shape = 1,
  xylim = NULL,
  tz = NULL
) {
  
  # ----- Validate parameters --------------------------------------------------
  
  MazamaCoreUtils::stopIfNull(pat)
  
  if ( !pat_isPat(pat) )
    stop("Parameter 'pat' is not a valid 'pa_timeseries' object.")
  
  if ( pat_isEmpty(pat) )
    stop("Parameter 'pat' has no data.")
  
  # Remove any duplicate data records
  pat <- pat_distinct(pat)
  pat$data$datetime <- lubridate::with_tz(pat$data$datetime, tzone = tz)
  
  # For easier access
  meta <- pat$meta
  data <- pat$data
  
  if ( is.null(xylim) ) {
    dataMin <- min(c(0, data$pm25_A, data$pm25_B), na.rm = TRUE)
    dataMax <- max(c(data$pm25_A, data$pm25_B), na.rm = TRUE)
    xylim <- c(dataMin, dataMax)
  }
  
  a_pm25 <-
    data %>%
    dplyr::select(.data$datetime, .data$pm25_A)
  
  b_pm25 <-
    data %>%
    dplyr::select(.data$datetime, .data$pm25_B)
  
  # Create a tidy dataframe appropriate for ggplot
  tidy_data <-
    dplyr::full_join(a_pm25, b_pm25, by = "datetime") %>%
    tidyr::gather("channel", "pm25", -.data$datetime)
  
  # ----- Linear model ---------------------------------------------------------
  
  # Model A as a function of B (data should lie on a line)
  model <- lm(data$pm25_A ~ data$pm25_B, subset = NULL, weights = NULL)
  
  slope <- as.numeric(model$coefficients[2])      # as.numeric() to remove name
  intercept <- as.numeric(model$coefficients[1])
  r_squared <- summary(model)$r.squared
  
  # Label for linear fit
  equationLabel <-
    ggplot2::annotate(
      geom = "text",
      x = 0.75 * xylim[2],
      y = c(0.4, 0.3, 0.2) * xylim[2],
      label = c(paste0("Slope = ", round(slope, digits = 2)),
                paste0("Intercept = ", round(intercept, digits = 1)),
                paste0("R\U00B2 = ", round(r_squared, digits = 3))) )
  
  # ----- Plot -----------------------------------------------------------------
  
  if ( showPlot ) {
    
    timezone <- tz
    year <- strftime(pat$data$datetime[1], "%Y", tz=timezone)
    
    # LH Linear regression plot
    lr_plot <-
      pat$data %>%
      ggplot2::ggplot(ggplot2::aes(x = .data$pm25_B, y = .data$pm25_A)) +
      ggplot2::geom_point(size = size,
                          shape = lr_shape,
                          color = lr_color,
                          alpha = alpha) +
      ggplot2::geom_smooth(method = "lm", size = 0, alpha = 0.45) +
      ggplot2::stat_smooth(geom = "line", color = lr_lcolor, alpha = lr_lalpha,
                           method = "lm", size = lr_lwd) +
      ggplot2::labs(title = "Channel Linear Regression",
                    x = "Channel B (\u03bcg / m\u00b3)",
                    y = "Channel A (\u03bcg / m\u00b3)") +
      ggplot2::theme_bw() +
      ggplot2::xlim(xylim) +
      ggplot2::ylim(xylim) +
      ggplot2::coord_fixed() +    # square aspect ratio
      equationLabel
    
    # Set time axis to sensor local time
    tidy_data$datetime <- lubridate::with_tz(tidy_data$datetime, tzone = timezone)
    
    ts_plot <-
      tidy_data %>%
      ggplot2::ggplot() +
      ggplot2::geom_point(ggplot2::aes(x = .data$datetime,
                                       y = .data$pm25,
                                       color = .data$channel),
                          size = size,
                          shape = ts_shape,
                          alpha = alpha) +
      ggplot2::scale_color_manual(values = c(a_color, b_color),
                                  name = "Channel",
                                  labels = c("A", "B")) +
      ggplot2::ylim(xylim) +
      ggplot2::ggtitle(label = "Channel A/B Overlay",
                       subtitle = expression("PM"[2.5])) +
      ggplot2::xlab(year) + ggplot2::ylab("\u03bcg / m\u00b3")
    
    
    # Gather and arrange the linear regression and time series plots with a banner title
    bannerText <- paste0("A / B Channel Comparision -- ", pat$meta$label)
    bannerGrob <- grid::textGrob(bannerText,
                                 just = "left",
                                 x = 0.025,
                                 gp = grid::gpar(fontsize = 18, col="grey50"))
    
    plot <- gridExtra::grid.arrange(bannerGrob, lr_plot, ts_plot,
                                    ncol = 1, heights = c(1, 6, 3))
    
    if ( whichPlot == "lm" ) {
      return(lr_plot)
    } else if ( whichPlot == "ab" ) {
      return(ts_plot)
    } else {
      return(plot)
    }
    
  }
  
  # # ----- Return ---------------------------------------------------------------
  #
  # return(invisible(model))
  
}

#' @title Instantiate a pm25 diurnal ggplot (Clone from AirMonitorPlots)
#'
#' @description
#' Create a plot using ggplot with default mappings and styling. Layers can then
#' be added to this plot using \code{ggplot2} syntax.
#'
#' @inheritParams custom_pm25DiurnalScales
#'
#' @param ws_data Default dataset to use when adding layers. Must be either a
#'   \code{ws_monitor} object or \code{ws_tidy} object.
#' @param startdate Desired startdate for data to include, in a format that can
#'   be parsed with \link{parseDatetime}.
#' @param enddate Desired enddate for data to include, in a format that can be
#'   parsed with \link{parseDatetime}.
#' @param timezone Timezone to use to set hours of the day
#' @param shadedNight add nighttime shading based on of middle day in selected
#'   period
#' @param mapping Default mapping for the plot
#' @param base_size Base font size for theme
#' @param ... Additional arguments passed on to
#'   \code{\link{custom_pm25DiurnalScales}}.
#'
#' @import ggplot2
#' @importFrom rlang .data
#' @importFrom dplyr filter
#' @importFrom MazamaCoreUtils parseDatetime dateRange
#' @importFrom lubridate hour minute
#' @importFrom PWFSLSmoke monitor_isMonitor monitor_toTidy monitor_isTidy timeInfo
#' @export
#'
asdv_pm25Diurnal <- function(
  ws_data,
  startdate = NULL,
  enddate = NULL,
  timezone = NULL,
  ylim = NULL,
  shadedNight = TRUE,
  mapping = aes_(x = ~hour, y = ~pm25),
  base_size = 11,
  ...
) {
  
  # ----- Validate Parameters --------------------------------------------------
  
  if ( !is.logical(shadedNight) )
    stop("shadedNight must be logical")
  
  if ( !is.numeric(base_size) )
    stop("base_size must be numeric")
  
  if ( monitor_isMonitor(ws_data) ) {
    ws_tidy <- monitor_toTidy(ws_data)
  } else if ( monitor_isTidy(ws_data) ) {
    ws_tidy <- ws_data
  } else {
    stop("ws_data must be either a ws_monitor object or ws_tidy object.")
  }
  
  # Determine the timezone (code borrowed from custom_pm25TimeseriesScales.R)
  if ( is.null(timezone) ) {
    if ( length(unique(ws_tidy$timezone) ) > 1) {
      timezone <- "UTC"
      xlab <- "Time of Day (UTC)"
    } else {
      timezone <- ws_tidy$timezone[1]
      xlab <- "Time of Day (Local)"
    }
  } else if ( is.null(xlab) ) {
    xlab <- paste0("Time of Day (", timezone, ")")
  }
  
  if ( !is.null(startdate) ) {
    startdate <- parseDatetime(startdate, timezone = timezone)
    if ( startdate > range(ws_tidy$datetime)[2] ) {
      stop("startdate is outside of data date range")
    }
  } else {
    startdate <- range(ws_tidy$datetime)[1]
  }
  
  if ( !is.null(enddate) ) {
    enddate <- parseDatetime(enddate, timezone = timezone)
    if ( enddate < range(ws_tidy$datetime)[1] ) {
      stop("enddate is outside of data date range")
    }
  } else {
    enddate <- range(ws_tidy$datetime)[2]
  }
  
  # ----- Prepare data ---------------------------------------------------------
  
  # MazamaCoreUtils::dateRange() was built for this!
  dateRange <- dateRange(startdate, enddate, timezone, ceilingEnd = TRUE)
  startdate <- dateRange[1]
  enddate <- dateRange[2]
  
  # Subset based on startdate and enddate
  ws_tidy <- ws_tidy %>%
    filter(.data$datetime >= startdate) %>%
    filter(.data$datetime <= enddate)
  
  # Add column for 'hour'
  ws_tidy$hour <- as.numeric(strftime(ws_tidy$datetime, "%H", tz = timezone))
  ws_tidy$day  <- strftime(ws_tidy$datetime, "%Y%m%d", tz = timezone)
  
  # ----- Create plot ----------------------------------------------------------
  
  gg <- ggplot(ws_tidy, mapping) +
    theme_pwfsl(base_size = base_size) +
    custom_pm25DiurnalScales(ws_tidy, xlab = xlab, ylim = ylim, ...)
  
  # Calculate day/night shading
  if (shadedNight) {
    # Get the sunrise/sunset information
    ti <- timeInfo(
      ws_tidy$datetime,
      longitude = ws_tidy$longitude[1],
      latitude = ws_tidy$latitude[1],
      timezone = ws_tidy$timezone[1]
    )
    
    # Extract the middle row
    ti <- ti[round(nrow(ti) / 2), ]
    
    # Get sunrise and sunset in units of hours
    sunrise <- hour(ti$sunrise) + (minute(ti$sunrise) / 60)
    sunset <- hour(ti$sunset) + (minute(ti$sunset) / 60)
    
    # Add shaded night
    scales <- layer_scales(gg)
    
    morning <- annotate(
      "rect",
      xmin = scales$x$limits[1],
      xmax = sunrise,
      ymin = scales$y$limits[1],
      ymax = scales$y$limits[2],
      fill = "black",
      alpha = 0.1
    )
    night <-   annotate(
      "rect",
      xmin = sunset,
      xmax = scales$x$limits[2],
      ymin = scales$y$limits[1],
      ymax = scales$y$limits[2],
      fill = "black",
      alpha = 0.1
    )
    
    gg <- gg + morning + night
  }
  
  # ----- Return ---------------------------------------------------------------
  
  return(gg)
  
}

#' @title Add hourly averages to a plot
#'
#' @description
#' This function calculates the mean y-value for each x-value. Should be used
#' only when \code{x} is discrete. The resulting mean can be mapped to any
#' aesthetic, specified with the \code{output} parameter.
#'
#' @param mapping Set of aesthetic mappings created by \code{aes()}. If
#'   specified and \code{inherit.aes = TRUE} (the default), it is combined with
#'   the default mapping at the top level of the plot. You must supply
#'   \code{mapping} if there is no plot mapping.
#' @param data The data to be displayed in this layer. There are three options:
#'   if \code{NULL}, the default, the data is inherited from the plot data. A
#'   \code{data.frame} or other object, will override the plot data. A
#'   \code{function} will be called with a single argument, the plot data. The
#'   return value must be a \code{data.frame}, and will be used as the layer
#'   data.
#' @param output "AQIColors", "mv4Colors", "scaqmd", "y"
#' @param input The value to find the mean of. If \code{NULL}, the default
#'   \code{y} value will be used.
#' @param geom The geometic object to display the data
#' @param position Position adjustment, either as a string, or the result of a
#'   call to a position adjustment function.
#' @param na.rm remove NA values from data
#' @param show.legend logical indicating whether this layer should be included
#'   in legends.
#' @param inherit.aes if \code{FALSE}, overrides the default aesthetics, rather
#'   than combining with them. This is most useful for helper functions that
#'   define both data and the aesthetics and shouldn't inherit behaviour from
#'   the default plot specificatino, eg \code{borders()}.
#' @param ... additional arguments passed on to \code{layer()}, such as
#'   aesthetics.
#'
#' @import ggplot2
#' @importFrom rlang parse_expr 
#' @importFrom dplyr group_by summarise
#' @export

stat_meanByHour <- function(
  mapping = NULL,
  data = NULL,
  input = NULL,
  output = "y",
  geom = "bar",
  position = "identity",
  na.rm = TRUE,
  show.legend = NA,
  inherit.aes = TRUE,
  ...
) {
  
  if (!is.null(input)) {
    if (is.null(mapping)) {
      mapping <- aes_string(input = input)
    } else {
      mapping$input <- parse_expr(input)
    }
  }
  
  list(
    layer(
      stat = StatMeanByGroup,
      data = data,
      mapping = mapping,
      geom = geom,
      position = position,
      show.legend = show.legend,
      inherit.aes = inherit.aes,
      params = list(
        output = output,
        input = input,
        na.rm = na.rm,
        ...
      )
    )
  )
  
}


StatMeanByGroup <- ggproto(
  "StatMeanByGroup",
  Stat,
  # BEGIN compute_group function
  compute_group = function(data,
                           scales,
                           params,
                           input,
                           output,
                           na.rm) {
    
    df <- data
    if (is.null(input)) df$input <- df$y
    
    means <- df %>%
      group_by(.data$x) %>%
      summarise(
        mean = mean(.data$input, na.rm = na.rm),
        mean_y = mean(.data$y, na.rm = TRUE)
      )
    
    # Set x and y
    data <- data.frame(
      x = means$x,
      y = means$mean_y
    )
    
    # Set output aesthetic
    if (output %in% c("AQIColors", "mv4Colors")) {
      
      # Add column for AQI level
      data$aqi <- .bincode(means$mean, AQI$breaks_24, include.lowest = TRUE)
      
      if (!"colour" %in% names(data)) {
        if (output == "mv4Colors") {
          data$colour <- AQI$mv4Colors[data$aqi]
        } else {
          data$colour <- AQI$colors[data$aqi]
        }
      }
      
      if (!"fill" %in% names(data)) {
        if (output == "mv4Colors") {
          data$fill <- AQI$mv4Colors[data$aqi]
        } else {
          data$fill <- AQI$colors[data$aqi]
        }
      }
      
    } else if (output == "scaqmd") {
      
      scaqmd_breaks <- c(0, 12, 35, 55, 75, 6000)
      scaqmd_colors <- c("#ABEBFF", "#3B8AFF", "#002ADE", "#9F00DE", "#6B0096")
      
      data$aqi <- .bincode(means$mean, breaks = scaqmd_breaks, include.lowest = TRUE)
      
      if (!"colour" %in% names(data)) {
        data$colour <- scaqmd_colors[data$aqi]
      }
      
      if (!"fill" %in% names(data)) {
        data$fill <- scaqmd_colors[data$aqi]
      }
      
    } else {
      # Map the mean to the correct aesthetic
      data[output] <- means$mean
    }
    
    return(data)
  }
  # END compute_group function
  
)

#' @title Theme for PWFSL plots
#'
#' @description
#' Applies the package standard theme to a \emph{ggplot} plot object.
#'
#' @param base_size Base font size.
#' @param base_family Base font family.
#'
#' @return A \emph{ggplot} theme.
#'
#' @import ggplot2
#' @export
theme_pwfsl <- function(
  base_size = 11,
  base_family = ""
) {
  
  theme_classic(
    base_size = base_size,
    base_family = base_family
  ) +
    
    theme(
      
      # All text is black
      text = element_text(color = "black"),
      
      # A little white space around the edges
      plot.margin = margin(
        unit(1.5 * base_size, "pt"),    # Top
        unit(1.0 * base_size, "pt"),    # Right
        unit(1.5 * base_size, "pt"),    # Bottom
        unit(1.0 * base_size, "pt")     # Left
      ),
      
      # Axes
      axis.title = element_text(
        size = 1.2 * base_size
      ),
      axis.text = element_text(
        size = 1.0 * base_size
      ),
      # Y-axis
      ###axis.line.y = element_blank(),
      axis.title.y = element_text(
        margin = margin(r = 1.0 * base_size)
      ),
      ###axis.ticks.y = element_blank(),
      axis.text.y = element_text(
        margin = margin(r = 0.5 * base_size)
      ),
      
      # X-axis
      ###axis.line.x = element_blank(),
      axis.title.x = element_text(
        margin = margin(t = 1.0 * base_size)
      ),
      ###axis.ticks.x = element_blank(),
      axis.text.x = element_text(
        margin = margin(t = 1.0 * base_size)
      ),
      
      # Legend
      legend.text = element_text(
        size = 1.0 * base_size,
        face = "italic",
        margin = margin(r = 50)
      ),
      
      # Box outline and grid lines
      panel.border = element_rect(fill = NA),
      
      panel.grid.major = element_line(
        linetype = "dotted",
        size = 0.3,
        colour = "grey"
      ),
      panel.grid.minor.x = element_line(
        linetype = "dotted",
        size = 0.1,
        colour = "grey"
      ),
      panel.grid.minor.y = element_blank(),
      
      # Title
      plot.title = element_text(
        color = "black",
        size = 1.5 * base_size,
        hjust = 0.5,
        vjust = 5,
        face = "bold"
      )
    )
}


#' @title PWFSL PM2.5 diurnal scales
#'
#' @description
#' Add PWFSL-style x-axis and y-axis scales suitable for a plot showing PM2.5
#' data as a funciton of hour of the day.
#'
#' @param data pm25 timeseries data. Should match the default dataset of the
#'   plot.
#' @param ylim custom y-axis limits. This function will apply a default limit
#'   depending on the data.
#' @param xlab Custom x-axis label. If \code{NULL} a default xlab will be
#'   generated.
#' @param ylab Custam y-axis label.
#' @param yexp Vector of range expansion constants used to add some padding
#'   around the data on the y-axis, to ensure that they are placed some distance
#'   away from the axes.
#' @param xexp Vector of range expansion constants used to add some padding
#'   around the data on the x-axis, to ensure that they are placed some distance
#'   away from the axes.
#' @param offsetBreaks if \code{TRUE}, x-axis ticks and guides are offset by
#'   0.5.
#'
#' @importFrom rlang .data
#' @importFrom PWFSLSmoke monitor_isMonitor monitor_toTidy monitor_isTidy
#' @importFrom dplyr case_when
#' @import ggplot2
#' @export
custom_pm25DiurnalScales <- function(
  data = NULL,
  ylim = NULL,
  xlab = NULL,
  ylab = "PM2.5 (\u00b5g/m3)",
  yexp = c(0.05, 0.05),
  xexp = c(0.05, 0.05),
  offsetBreaks = FALSE
) {
  
  
  # Validate parameters --------------------------------------------------------
  
  if (monitor_isMonitor(data)) {
    data <- monitor_toTidy(data)
  } else if (monitor_isTidy(data)) {
    data <- data
  } else {
    stop("data must be either a ws_monitor object or ws_tidy object.")
  }
  
  
  # Calculate axis limits ----------------------------------------------------
  
  # Default to well defined y-axis limits for visual stability
  if (is.null(ylim)) {
    ylo <- 0
    ymax <- max(data$pm25, na.rm = TRUE)
    
    yhi <- case_when(
      ymax <= 50   ~ 50,
      ymax <= 100  ~ 100,
      ymax <= 200  ~ 200,
      ymax <= 400  ~ 400,
      ymax <= 600  ~ 600,
      ymax <= 1000 ~ 1000,
      ymax <= 1500 ~ 1500,
      TRUE         ~ 1.05 * ymax
    )
    
  } else {
    # Standard y-axis limits
    ylo <- ylim[1]
    yhi <- ylim[2]
  }
  
  xmin <- 0 - (23 * xexp[1])
  xmax <- 23 + (23 * xexp[2])
  
  
  # Calculate breaks -----------------------------------------------------------
  
  ## NOTE:
  #  `ifelse` is not used, because the condition `offsetBreaks` is length 1,
  #  which means the output of `ifelse` would also be a 1 element vector.
  
  if (offsetBreaks) {
    breaks <- seq(-0.5, 22.5, by = 3)
  } else {
    breaks <- seq(0, 22, by = 3)
  }
  
  if (offsetBreaks) {
    minor_breaks <- seq(-0.5, 22.5, by = 1)
  } else {
    minor_breaks <- seq(0, 22, by = 1)
  }
  
  
  # Add scales -----------------------------------------------------------------
  
  list(
    scale_x_continuous(
      breaks = breaks,
      minor_breaks = seq(0, 23, by = 1),
      labels = c("midnight", "3am", "6am", "9am", "Noon", "3pm", "6pm", "9pm"),
      limits = c(xmin, xmax),
      expand = c(0, 0)
    ),
    scale_y_continuous(
      limits = c(ylo - (yexp[1] * yhi), yhi + (yexp[2] * yhi)),
      expand = c(0, 0)
    ),
    ylab(ylab),
    xlab(xlab
    )
  )
  
}