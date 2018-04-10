#' @name predict
#' @title Predict BANTER events
#' @description Predict species of events for novel data from a BANTER model.
#'
#' @param object a \code{\link{banter_model}} object.
#' @param new.data a list of event and detector data.
#' @param ... unused.
#' 
#' @return A data.frame with the predicted species (\code{"predicted"}) followed 
#'   by columns of assignment probabilities.
#'   
#' @author Eric Archer \email{eric.archer@@noaa.gov}
#' 
#' @importFrom dplyr n
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
  # Get number of calls in each detector for each event
  detector.num <- lapply(names(new.data$detectors), function(d) {
    new.data$events %>% 
      dplyr::left_join(new.data$detectors[[d]], by = "event.id") %>% 
      dplyr::group_by(.data$event.id) %>% 
      dplyr::summarize(n = sum(!is.na(.data$call.id))) %>% 
      dplyr::ungroup() %>% 
      dplyr::mutate(detector = paste0("prop.", d))
  }) %>%
    dplyr::bind_rows()
  
  # Convert number to proportion of calls
  detector.prop <- detector.num %>% 
    dplyr::group_by(.data$event.id) %>% 
    dplyr::mutate(n = n / sum(n, na.rm = TRUE)) %>% 
    dplyr::ungroup() %>% 
    tidyr::spread("detector", "n") %>% 
    replace(is.na(.), 0) %>% 
    as.data.frame()
  
  # Calculate mean votes for each event
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
  
  # Construct data.frame to predict
  df <- new.data$events %>% 
    dplyr::left_join(detector.prop, by = "event.id") %>% 
    dplyr::left_join(detector.votes, by = "event.id")
    
    # add call rate columns if duration is present and there is no missing data
    if("duration" %in% colnames(df)) {
      if(all(!is.na(df$duration))) {
        df <- df %>% 
          dplyr::left_join(
            detector.num %>%  
              dplyr::left_join(
                dplyr::select(df, "event.id", "duration"), 
                by = "event.id"
              ) %>% 
              dplyr::mutate(
                detector = gsub("prop.", "rate.", .data$detector),
                n = .data$n / .data$duration
              ) %>% 
              dplyr::select(-.data$duration) %>% 
              tidyr::spread("detector", "n"),
            by = "event.id"
          )
      }
    }
  
  df <- df %>% 
    dplyr::filter(complete.cases(.))
  
  list(
    events = df,
    predict.df = cbind(
      data.frame(event.id = df$event.id, stringsAsFactors = FALSE),
      predicted = as.character(predict(object@model, df, type = "response")),
      predict(object@model, df, type = "prob"),
      stringsAsFactors = FALSE
    )
  )
}

#' @name predict
#' @rdname predict
#' @aliases predict,banter_model-method
methods::setMethod("predict", "banter_model", predict.banter_model) 