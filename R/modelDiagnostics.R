#' @title Random Forest Model Diagnostics
#' @description display diagnostics for event and detector models
#' 
#' @param x a \code{\link{event_model}} object.
#' @param detector name of detector. If \code{NULL}, returns event model
#'   summary.
#' @param n number of final iterations to summarize OOB error rate for. If 
#'   between 0 and 1 is taken as a proportion of chain.
#'   
#' @importFrom rfPermute confusionMatrix   
#' @export
#' 
modelDiagnostics <- function(x, detector = NULL, n = 0.1) {
  rf <- if(is.null(detector)) x@model else x@detectors[[detector]]@model
  smry <- rbind(
    oob.error = .rfErrorSmry(rf, n),
    inbag.pct = .inBagSmry(rf)
  )
  print(smry)
  cat("\n")
  print(rfPermute::confusionMatrix(rf))
}

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




