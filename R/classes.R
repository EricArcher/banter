#' @name classes
#' @title detector_model class
#' @description An S4 class storing a classification model for a detector.
#'
#' @slot name detector name
#' @slot model classification model
#'
#' @author Eric Archer \email{eric.archer@@noaa.gov}
#' 
setOldClass(c("randomForest", "rfPermute", "ranger"))
setClassUnion("classifier", c("randomForest", "rfPermute", "ranger", "NULL"))

#' @rdname classes
#' @importFrom methods setClass setValidity new
#' @export
#' 
detector_model <- methods::setClass(
  "detector_model",
  slots = c(
    name = "character",
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
    
    if(is.null(valid)) TRUE else valid
  }
)