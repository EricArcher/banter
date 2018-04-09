#' @title Extract Random Forest Model
#' @description Extract BANTER event or detector Random Forest model.
#' 
#' @param x a \code{\link{banter_model}} object.
#' @param model name of model to extract. Default is \code{"event"} 
#'   to summarize the event-level model. Can also be name of a detector.
#' 
#' @return a \code{\link{randomForest}} model object.
#' 
#' @author Eric Archer \email{eric.archer@@noaa.gov}
#' 
#' @export
#' 
getBanterModel <- function(x, model = "event") {
  if(model == "event") return(x@model)
  .checkModelName(x, model)
  x@detectors[[model]]@model
}