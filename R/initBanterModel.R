#' @title Initialize BANTER model
#' @description Initialize a BANTER model with a data.frame of events.
#'
#' @param x a data.frame
#' 
#' @author Eric Archer \email{eric.archer@@noaa.gov}
#' 
#' @export
#' 
initBanterModel <- function(x) {
  banter_model(data = x, detectors = NULL, model = NULL)
}