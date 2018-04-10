#' @title Sample Size
#' @description Return sample size used for a BANTER model.
#' 
#' @param x a \code{\link{banter_model}} object.
#' @param model name of model to extract. Default is \code{"event"} 
#'   to summarize the event-level model. Can also be name of a detector.
#' 
#' @return a vector of sample sizes.
#' 
#' @author Eric Archer \email{eric.archer@@noaa.gov}
#' 
#' @export
#' 
getSampSize <- function(x, model = "event") {
  if(model == "event") return(x@sampsize)
  .checkModelName(x, model)
  x@detectors[[model]]@sampsize
}