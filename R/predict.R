#' @name predict
#' @title Predict BANTER events
#' @description Predict species of events for novel data from a BANTER model.
#'
#' @param object a \code{\link{banter_model}} object.
#' @param new.data a list of event and detector data.
#' @param ... unused.
#'
#' @author Eric Archer \email{eric.archer@@noaa.gov}
#' 
#' @importFrom magrittr %>%
#' @importFrom methods setGeneric setMethod
#' @importFrom plyr .
#' @importFrom randomForest randomForest
#' @importFrom rlang .data
#' 
#' @exportMethod predict
setGeneric("predict")

#' @name predict
#' @rdname predict
#' @method predict banter_model
#' @export
predict.banter_model <- function(object, new.data, ...) {
  detector.prop <- lapply(names(new.data$detectors), function(d) {
    new.data$events %>% 
      dplyr::left_join(new.data$detectors[[d]], by = "event.id") %>% 
      dplyr::group_by(.data$event.id) %>% 
      dplyr::summarize(n = sum(!is.na(.data$call.id))) %>% 
      dplyr::ungroup() %>% 
      dplyr::mutate(detector = paste0("prop.", d))
  }) %>%
    dplyr::bind_rows() %>% 
    dplyr::group_by(.data$event.id) %>% 
    dplyr::mutate(n = n / sum(n, na.rm = TRUE)) %>% 
    dplyr::ungroup() %>% 
    tidyr::spread("detector", "n") %>% 
    replace(is.na(.), 0) %>% 
    as.data.frame()
  
  detector.votes <- sapply(names(object@detectors), function(d) {
    predict(
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
  
  event.df <- new.data$events %>% 
    dplyr::left_join(detector.prop, by = "event.id") %>% 
    dplyr::left_join(detector.votes, by = "event.id") %>% 
    dplyr::filter(complete.cases(.))
  
  cbind(
    data.frame(event.id = event.df$event.id, stringsAsFactors = FALSE),
    predicted = as.character(predict(object@model, event.df, type = "response")),
    predict(object@model, event.df, type = "prob"),
    stringsAsFactors = FALSE
  )
}

#' @name predict
#' @rdname predict
#' @aliases predict,banter_model-method
methods::setMethod("predict", "banter_model", predict.banter_model) 