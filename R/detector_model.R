#' @title Detector classification model
#' @description Create classification model for a detector
#'
#' @param name detector name
#' @param detector.data measurement data for detections
#' @param event.data data.frame associating event.id with species
#' @param ntree number of trees in model
#' @param sampsize number or fraction of samples to use in each tree
#'
#' @slot name detector name
#' @slot ids data.frame of event.ids and call.ids for calls in detector
#' @slot model classification model
#'
#' @author Eric Archer \email{eric.archer@@noaa.gov}
#' 
#' @importFrom randomForest randomForest
#' @importFrom methods setClass setValidity new
#' @export detector_model
#' @export
#' 
detector_model <- methods::setClass(
  "detector_model",
  slots = c(
    name = "character",
    ids = "data.frame",
    model = "classifier"
  )
)

methods::setValidity(
  "detector_model",
  method = function(object) {
    valid <- NULL
    
    # Check @name -----------------------
    if(length(object@name) != 1) {
      valid <- c(valid, "slot '@name' must have length 1")
    }
    
    # Check @ids ------------------------
    if(!all(c("event.id", "call.id") %in% colnames(object@ids))) {
      valid <- c(valid, "slot '@ids' must have 'event.id' and 'call.id' columns")
    }
    
    if(is.null(valid)) TRUE else valid
  }
)

#' @importFrom magrittr %>%
#' @importFrom randomForest randomForest
#' @importFrom rlang .data
#' 
methods::setMethod(
  "initialize", 
  "detector_model",
  function(
    .Object,
    name, detector.data, event.data, 
    ntree, sampsize = 1, ...
  ) {
    df <- detector.data %>% 
      dplyr::inner_join(event.data, by = "event.id") %>% 
      dplyr::mutate(species = factor(.data$species)) %>% 
      as.data.frame
    
    rf <- randomForest::randomForest(
      species ~ ., 
      data = dplyr::select(df, -.data$event.id, -.data$call.id),
      ntree = ntree, 
      sampsize = getSampsize(df$species, sampsize), 
      replace = FALSE,
      importance = TRUE
    )
    
    .Object@name <- name
    .Object@model <- rf
    .Object@ids <- df[, c("event.id", "call.id")]
    .Object
  }
)