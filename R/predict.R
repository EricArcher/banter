#' @name predict
#' @title Predict event data
#' @description Predict event data
#'
#' @param object a \code{\link{event_model}} object.
#' @param new.data a list of event and detector data.
#' @param ... unused.
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
#' @exportMethod predict
setGeneric("predict")

#' @name predict
#' @rdname predict
#' @method predict event_model
#' @export
predict.event_model <- function(object, new.data, ...) {
  detector.prop <- sapply(names(object@detectors), function(d) {
    new.data$detectors[[d]]$event.id
  }, simplify = FALSE) %>% 
    .propCalls()
  
  detector.votes <- sapply(names(object@detectors), function(d) {
    randomForest:::predict.randomForest(
      object@detectors[[d]]@model, 
      new.data$detectors[[d]], 
      type = "prob"
    ) %>% 
      as.data.frame() %>% 
      cbind(
        event.id = new.data$detectors[[d]]$event.id, 
        stringsAsFactors = FALSE
      )
  }, simplify = FALSE) %>% 
    .meanVotes()
  
  event.df <- new.data$event %>% 
    dplyr::left_join(detector.prop, by = "event.id") %>% 
    dplyr::left_join(detector.votes, by = "event.id") %>% 
    dplyr::filter(complete.cases(.))
  
  cbind(
    data.frame(event.id = event.df$event.id),
    predicted = predict(object@model, event.df, type = "response"),
    randomForest:::predict.randomForest(object@model, event.df, type = "prob"),
    stringsAsFactors = FALSE
  )
}

#' @name predict
#' @rdname predict
#' @aliases predict,event_model-method
methods::setMethod(
  "predict", 
  "event_model", 
  predict.event_model
) 
