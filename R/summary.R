#' @name summary
#' @title BANTER Classifier Model Summary
#' @description Display summaries for event and detector models
#' 
#' @param object a \code{\link{banter_model}} object.
#' @param detector name of detector to summarize. Default is \code{"event"} 
#'   to summarize the event-level model.
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
summary.banter_model <- function(object, detector = "event", n = 0.1, ...) {  
  print(object)
  rf <- getBanterModel(object, detector)
  if(!is.null(rf)) {  
    cat("\n")
    print(rbind(
      oob.error = .rfErrorSmry(rf, n),
      inbag.pct = .inBagSmry(rf)
    ))
    cat("\n")
    print(rfPermute::confusionMatrix(rf))
  }
}

#' @name summary
#' @rdname summary
#' @aliases summary,banter_model-method
methods::setMethod("summary", "banter_model", summary.banter_model) 


#' @rdname internals
#' @keywords internal
#' 
.rfErrorSmry <- function(rf, n = 0.1) {
  x <- rf$err.rate
  if(dplyr::between(n, 0, 1)) n <- ceiling(n * nrow(x))
  oob <- x[(nrow(x) - n + 1):nrow(x), "OOB"]
  c(n = length(oob), round(summary(oob), 3))
}

#' @rdname internals
#' @keywords internal
#' 
.inBagSmry <- function(rf) {
  c(
    n = length(rf$oob.times), 
    round(summary(1 - (rf$oob.times / rf$ntree)), 3)
  )
}