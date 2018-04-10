#' @title Detector Names
#' @description Return names of detectors loaded in BANTER model.
#' 
#' @param x a \code{\link{banter_model}} object.
#' 
#' @return a vector of names.
#' 
#' @author Eric Archer \email{eric.archer@@noaa.gov}
#' 
#' @export
#' 
getDetectorNames <- function(x) names(x@detectors)