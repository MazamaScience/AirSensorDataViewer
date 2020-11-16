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
#' @export
notify <- function(msg) {
  logger.trace(msg)
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