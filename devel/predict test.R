setGeneric("predict")
predict.event_model <- function(object, new.data) {
  detector.prop <- sapply(names(object@detectors), function(d) {
    new.data$detectors[[d]]$event.id
  }, simplify = FALSE) %>% 
    .propCalls()
  
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
  
  event.df <- new.data$event %>% 
    dplyr::left_join(detector.prop, by = "event.id") %>% 
    dplyr::left_join(detector.votes, by = "event.id") %>% 
    dplyr::filter(complete.cases(.))
    
  cbind(
    data.frame(event.id = event.df$event.id),
    predicted = predict(object@model, event.df, type = "response"),
    predict(object@model, event.df, type = "prob"),
    stringsAsFactors = FALSE
  )
}
setMethod("predict", "event_model", predict.event_model) 