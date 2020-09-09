#' 4 Digit code to Community name
#'
#' @param X an unfriendly id string
#'
#' @return a friendly community string.
#' @export
id2com <- function(X) {
  # Helpful conversion list
  com_id  <-
    data.frame(
      "SCAP" = "Alhambra/Monterey Park",
      "SCBB" = "Big Bear Lake",
      "SCEM" = "El Monte",
      "SCIV" = "Imperial Valley",
      "SCNP" = "Nipomo",
      "SCPR" = "Paso Robles",
      "SCSJ" = "San Jacinto",
      "SCSB" = "Seal Beach",
      "SCAH" = "Oakland",
      "SCAN" = "Richmond",
      "SCUV" = "West Los Angeles",
      "SCSG" = "South Gate",
      "SCHS" = "Sycamore Canyon",
      "SCTV" = "Temescal Valley"
    )
  unlist(lapply(X, function(x) {ifelse(is.null(com_id[[x]]), x, levels(com_id[[x]]))}))
}

#' Community name to 4 digit code
#'
#' @param X a friendly community string.
#'
#' @return an unfriendly community string.
#' @export
com2id <- function(X) {
  # Helpful conversion list
  com_id  <-
    data.frame(
      "SCAP" = "Alhambra/Monterey Park",
      "SCBB" = "Big Bear Lake",
      "SCEM" = "El Monte",
      "SCIV" = "Imperial Valley",
      "SCNP" = "Nipomo",
      "SCPR" = "Paso Robles",
      "SCSJ" = "San Jacinto",
      "SCSB" = "Seal Beach",
      "SCAH" = "Oakland",
      "SCAN" = "Richmond",
      "SCUV" = "West Los Angeles",
      "SCSG" = "South Gate",
      "SCHS" = "Sycamore Canyon",
      "SCTV" = "Temescal Valley"
    )
  unlist(lapply(X, function(x) {i<-which(com_id == x); ifelse(length(i)!=0, names(com_id[i]), x)}))
}

#' Waiter wrapper function
#'
#' @param id an element ID to append to.
#'
#' @export
#' @noRd
#' 
#' @importFrom waiter Waiter spin_throbber transparent
waiter <- function(id = NULL) { 
  Waiter$new(
    id = id,  
    html = spin_throbber(), 
    color = transparent(.5)
  )
}

#' Waitress wrapper function
#' 
#' @export
#' @noRd
#' 
#' @importFrom waiter Waitress
waitress <- function() {
  Waitress$new(min = 0, max = 100)
}

#' A Waitress notification wrapper
#'
#' @param expr an expression to make loading notification for
#' @param msg a message to show the user.
#'
#' @export
#' @noRd
#' 
#' @importFrom stats runif
makeWaitress <- function(expr, msg) {
  w <- waitress()
  w$notify(msg, position = "bl")
  w$set(runif(1, min = 10, max = 33))
  expr
  w$close()
}

#' Error handling function 
#'
#' @param err an error string, try-error, etc.
#'
#' @return NULL
#' @export
catchError <- function(err) {
  logger.error(err)
  NULL
}

#' Animate Plot up
#' 
#' @export
plotUp <- function() {
  shinyjs::runjs("
    if(!$('#dem').hasClass('in')) {
      $('#collapse_btn').click();
    };
  ")
}

#' Animate plot down
#'
#' @export
plotDown <- function() {
  shinyjs::runjs("
    if($('#dem').hasClass('in')) {
      $('#collapse_btn').click();
    };
  ")
}

#' Animate the tiotemp map slider up
#'
#' @export
sliderUp <- function() {
  shinyjs::runjs("
    d3.select('#overview_ui_1-timeseriesMap')
        .select('.leaflet-bottom .leaflet-control')
        .transition()
        .duration(75)
        .style('margin-bottom', '20vh');
  ")
}

#' Create a hash-cache key for caching
#'
#' @param x an object, expression, anything. 
#'
#' @return a hash
#' @export
#' @importFrom digest digest
cacheKey <- function(...) {
  x <- list(...)
  digest(x)
}

#' A CSS loader feedback wrapper function
#'
#' @param el an element to append to.
#'
#' @export
#' @importFrom shinycssloaders withSpinner
withLoader <- function(el) {
  withSpinner(el, color = "#008cba", type = 7) 
}

#' A Shiny notification wrapper
#'
#' @param msg A message to display
#'
#' @return
#' @export
notify <- function(msg) {
  shiny::showNotification(
    HTML(paste0(
      "<b>Oops. Something has gone wrong!</b>
      <br>", msg
    )), 
    type = "warn", 
    duration = 6, 
    id = "caught"
  )
}