#' @title Get Balanced Sample Size
#' @description return a vector of equal sample sizes for balanced Random Forest 
#'   model. Takes vector of discrete values or frequency table as well as 
#'   desired sample size percentage and minimum sample size.
#'   
#' @param x vector of classes as response values or table of frequencies 
#'   (output of \code{\link{table}}.
#' @param samp.pct desired percentage to use from smallest class (\code{0:1}).
#' @param min.samp.n minimum permitted sample size.
#' 
#' @details Returns vector of \code{floor(samp.pct * smallest class frequency)}
#'  that is as long as there are unique classes in \code{x}. Will throw an error 
#'  if this number is < \code{min.samp.n}.
#' @author Eric Archer \email{eric.archer@@noaa.gov}
#' 
#' @export
#' 
getSampsize <- function(x, samp.pct, min.samp.n = 1) {
  if(min.samp.n < 1) stop("min.samp.n must be >= 1")

  freq <- if(is.table(x)) x else table(x)
  
  # check that minimum frequency is greater than minimum sample size
  min.freq <- min(freq)
  if(any(freq <= min.samp.n)) {
    stop(paste0(
      "minimum frequency (", min.freq, ") <= min.samp.n (", min.samp.n, ")"
    ))
  }
  
  # check that sample size is same or greater than minimum sample size
  sampsize <- floor(min(freq * samp.pct))
  if(sampsize < min.samp.n) {
    min.pct <- min.samp.n / min.freq
    stop(paste0(
      "sample size (", sampsize, ") < min.samp.n (", min.samp.n, "), ",
      "Decrease min.samp.n, or raise samp.pct to ", min.pct, "\n",
      "Frequencies:\n",
      paste0(names(freq), ":", freq, "\n", collapse = "")
    ))
  }
  
  rep(sampsize, length(freq))
}
  