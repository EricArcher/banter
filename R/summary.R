#' @name summary
#' @title BANTER Random Forest Model Summary
#' @description display summaries for event and detector models
#' 
#' @param object a \code{\link{event_model}} object.
#' @param detector name of detector to summarize.
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
#' @method summary event_model
#' @export 
#' 
summary.event_model <- function(object, detector = "event", n = 0.1, ...) {  
  print(object)
  rf <- getModel(object, detector)
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
#' @aliases summary,event_model-method
methods::setMethod("summary", "event_model", summary.event_model) 


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




