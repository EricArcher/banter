#' @title Get Balanced Sample Size
#' @description Produces a vector of equal sample sizes for balanced 
#'   Random Forest model.
#'   
#' @param x vector of discrete class values or table of frequencies as output 
#'   of \code{\link{table}}.
#' @param req.sampsize desired number of samples per class. If value is between 
#'   0 and 1, then it is treated as a percentage and sample size will be this 
#'   value times the smallest class frequency rounded to the nearest whole 
#'   number.
#'   
#' @details \code{req.sampsize} must be smaller than smallest class frequency. If
#'   \code{req.sampsize} is a percentage, then resulting rounded sample size 
#'   must be at least 1.
#' 
getSampsize <- function(x, req.sampsize) {
  if(req.sampsize < 0) stop("'req.sampsize' must be > 0")
  
  freq <- if(is.table(x)) x else table(x)
  
  n <- if(req.sampsize >= 1) {
    req.sampsize
  } else {
    round(min(freq) * req.sampsize, 0)
  }
  
  if(n < 1 | n >= min(freq)) {
    stop(paste0(
      "sampsize = ", n, 
      " (is < 1 or >= minimum frequency):\n",
      paste0(names(freq), ":", freq, "\n", collapse = "")
    ))
  }
  
  rep(n, length(freq))
}