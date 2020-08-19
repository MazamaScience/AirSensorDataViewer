#' 4 Digit code to Community name
#'
#' @param X 
#'
#' @return
#' @export
#'
#' @examples
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
#' @param X 
#'
#' @return
#' @export
#'
#' @examples
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

#' Title
#'
#' @param id 
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

#' Title
#'
#' @return
#' @export
#' 
#' @noRd
#' 
#' @importFrom waiter Waitress
waitress <- function() {
  Waitress$new(min = 0, max = 100)
}

#' Title
#'
#' @param expr 
#' @param msg 
#'
#' @export
#'
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

#' Title
#'
#' @param err 
#'
#' @return
#' @export
#'
#' @examples
throwNiceError <- function(err) {
  logger.error(err)
  print("oops!")
}

#' Animate Plot up
#'
#' @return
#' @export
#'
#' @examples
plotUp <- function() {
  shinyjs::runjs(
    "if(!$('#dem').hasClass('in')) {
      $('#collapse_btn').click();
    };
    d3.select('#overview_ui_1-timeseriesMap')
      .select('.leaflet-bottom .leaflet-control')
      .transition()
      .duration(75)
      .style('margin-bottom', '20vh')")
}

#' Animate plot down
#'
#' @return
#' @export
#'
#' @examples
plotDown <- function() {
  
}