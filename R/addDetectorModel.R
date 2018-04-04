#' @name addDetectorModel
#' @title Add a Detector Model
#' @description Add a detector model to an event model
#'
#' @param x a \code{\link{event_model}} object.
#' @param name name of detector
#' @param data detector data.frame
#' @param ntree number of trees
#' @param sampsize number or fraction of samples to use in each tree
#' 
#' @author Eric Archer \email{eric.archer@@noaa.gov}
#' 
#' @importFrom rlang .data
#' @importFrom magrittr %>%
#' @export
#' 
addDetectorModel <- function(x, name, data, ntree, sampsize = 1) {
  event.df <- dplyr::select(x@data, .data$event.id, .data$species)
  x@detectors[[name]] <- detector_model(name, data, event.df, ntree, sampsize)
  x@model <- NULL
  x
}

#' @rdname addDetectorModel
#' @export
#' 
removeDetectorModel <- function(x, name) {
  x@detectors[[name]] <- NULL
  x@model <- NULL
  x
}