#' @name summary
#' @title BANTER Classifier Model Summary
#' @description Display summaries for event and detector models
#' 
#' @param object a \code{\link{banter_model}} object.
#' @param model name of model to summarize. Default is \code{"event"} 
#'   to summarize the event-level model. Can also be name of a detector.
#' @param n number of final iterations to summarize OOB error rate for. If 
#'   between 0 and 1 is taken as a proportion of chain.
#' @param ... ignored.
#'   
#' @importFrom rfPermute confusionMatrix  
#' 
#' @exportMethod summary
setGeneric("summary")

#' @name summary
#' @rdname summary
#' @method summary banter_model
#' @export 
#' 
summary.banter_model <- function(object, model = "event", n = 0.1, ...) {  
  print(object)
  rf <- getBanterModel(object, model)
  if(!is.null(rf)) {  
    cat("\nDistribution of percent correctly classified overall in last 'n' trees:\n")
    print(.rfPctCorrectSmry(rf, n))
    cat("\nSample inbag rate distribution:\n")
    print(round(summary(1 - (rf$oob.times / rf$ntree)), 3))
    cat("\nConfusion matrix:\n")
    print(rfPermute::confusionMatrix(rf))
    print(rfPermute::plotRFtrace(rf))
  }
}

#' @name summary
#' @rdname summary
#' @aliases summary,banter_model-method
methods::setMethod("summary", "banter_model", summary.banter_model) 

#' @rdname internals
#' @keywords internal
#' 
.rfPctCorrectSmry <- function(rf, n = 0.1) {
  x <- rf$err.rate
  if(dplyr::between(n, 0, 1)) n <- ceiling(n * nrow(x))
  correct <- 1 - x[(nrow(x) - n + 1):nrow(x), "OOB"]
  c(n = length(correct), round(summary(correct), 3))
}
