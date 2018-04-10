#' @title Extract Random Forest Model Data
#' @description Extract BANTER event data used for the Random Forest model.
#' 
#' @param x a \code{\link{banter_model}} object.
#' 
#' @return a \code{\link{randomForest}} model object.
#' 
#' @author Eric Archer \email{eric.archer@@noaa.gov}
#' 
#' @export
#' 
getBanterModelData <- function(x) x@model.data