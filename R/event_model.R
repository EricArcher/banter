#' @title Event classification model
#' @description Create classification model for events
#'
#' @param detector.models measurement data for detections
#' @param event.data data.frame associating event.id with species
#' @param ntree number of trees in model
#' @param sampsize number or fraction of samples to use in each tree
#'
#' @slot data data.frame of event.ids and call.ids for calls in detector
#' @slot detectors list of \code{detector_model} objects
#' @slot model classification model
#'
#' @author Eric Archer \email{eric.archer@@noaa.gov}
#' 
#' @importFrom dplyr n
#' @importFrom magrittr %>%
#' @importFrom plyr .
#' @importFrom randomForest randomForest
#' @importFrom methods setClass setValidity new
#' @export event_model
#' @export
#' 
event_model <- methods::setClass(
  "event_model",
  slots = c(
    data = "data.frame",
    detectors = "list",
    model = "classifier"
  )
)

methods::setValidity(
  "event_model",
  method = function(object) {
    valid <- NULL
    
    # Check @ids ------------------------
    if(!all(c("event.id", "call.id") %in% colnames(object@data))) {
      valid <- c(valid, "slot '@data' must have 'event.id' and 'call.id' columns")
    }
    
    # Check @detectors
    if(length(object@detectors) > 0) {
      classes <- unlist(sapply(object@detectors, class))
      if(any(classes != "detector_model")) {
        valid <- c(valid, "all elements in slot '@detectors' must be a 'detector_model' object")
      }
    }
    
    if(is.null(valid)) TRUE else valid
  }
)

methods::setMethod(
  "initialize", 
  "event_model",
  function(.Object, event.data, ...) {
    .Object@data <- event.data
    .Object@detectors <- list()
    .Object@model <- NULL
    .Object
  }
)

methods::setMethod(
  "show",
  "event_model",
  function(object) {
    df <- object@data %>% 
      dplyr::group_by(.data$species) %>% 
      dplyr::summarize(events = n()) %>% 
      dplyr::ungroup() %>% 
      as.data.frame
    
    err.rate <- NULL
    if(length(object@detectors) > 0 & !is.null(object@detectors)) {    
      df <- df %>% 
        dplyr::left_join(
          object@data %>% 
            dplyr::select(.data$species, .data$event.id) %>% 
            dplyr::left_join(numCalls(object), by = "event.id") %>% 
            dplyr::select(-.data$event.id) %>% 
            tidyr::gather("detector", "n", -.data$species) %>% 
            dplyr::group_by(.data$species, .data$detector) %>% 
            dplyr::summarize(n = sum(n)) %>% 
            dplyr::ungroup() %>% 
            tidyr::spread("detector", "n"),
          by = "species"
        ) %>% 
        as.data.frame()
      
      err.rate <- sapply(object@detectors, function(x) {
        oob <- x@model$err.rate[, "OOB"]
        oob[length(oob)]
      })
      if(!is.null(object@model)) {
        oob <- object@model$err.rate[, "OOB"]
        err.rate <- c(event = oob[length(oob)], err.rate)
      }
    }
    
    print(df)
    if(!is.null(err.rate)) {
      cat("\nModel error rates:\n")
      print(round(err.rate, 3))
    }
  }
)