#' @title Events classification model
#' @description Create classification model for events
#'
#' @slot data data.frame of event.ids and call.ids for calls in detector
#' @slot detectors list of \code{detector_model} objects
#' @slot model classification model
#'
#' @author Eric Archer \email{eric.archer@@noaa.gov}
#' 
#' @importFrom dplyr n
#' @importFrom magrittr %>%
#' @importFrom methods setClass setValidity new setMethod
#' @importFrom plyr .
#' @importFrom randomForest randomForest
#' @importFrom rlang .data
#' 
#' @keywords internal
#' 
banter_model <- methods::setClass(
  "banter_model",
  slots = c(
    data = "data.frame",
    detectors = "listOrNull",
    model = "classifier"
  )
)

methods::setValidity(
  "banter_model",
  method = function(object) {
    valid <- NULL
    
    # Check @ids ------------------------
    if(!all(c("event.id", "species") %in% colnames(object@data))) {
      valid <- c(valid, "slot '@data' must have 'event.id' and 'species' columns")
    }
    
    # Check @detectors
    if(!is.null(object@detectors)) {
      if(length(obect@detectors) == 0) {
        valid <- c(valid, "slot '@detectors' can't be an empty list")
      } else {
        classes <- unlist(sapply(object@detectors, class))
        if(any(classes != "detector_model")) {
          valid <- c(valid, "all elements in slot '@detectors' must be a 'detector_model' object")
        }
      }
    }
    
    if(is.null(valid)) TRUE else valid
  }
)

methods::setMethod(
  "show",
  "banter_model",
  function(object) {
    df <- numEvents(object)
    
    err.rate <- NULL
    if(!is.null(object@detectors)) {    
      df <- df %>% 
        dplyr::left_join(numCalls(object), by = "species") %>% 
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