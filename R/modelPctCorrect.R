#' @title Model Percent Correct
#' @description Extract percent correctly classified by species for detector 
#'   and event models.
#' 
#' @param x a \code{\link{banter_model}} object.
#' 
#' @return a data.frame with the percent correctly classified for each model 
#'   in \code{x}.
#' 
#' @author Eric Archer \email{eric.archer@@noaa.gov}
#' 
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' 
#' @export
#' 
modelPctCorrect <- function(x) {
if(is.null(x@detectors)) return(NULL)
lapply(c(names(x@detectors), "event"), function(model) {
  rf <- getBanterModel(x, model) 
  if(is.null(rf)) return(NULL)
  conf.mat <- table(rf$y, rf$predicted)
  correct <- diag(conf.mat) / rowSums(conf.mat)
  correct <- c(correct, Overall = sum(diag(conf.mat)) / sum(conf.mat))
  data.frame(
    species = names(correct), 
    pct.correct = correct, 
    model = model,
    stringsAsFactors = FALSE
  )
}) %>% 
  dplyr::bind_rows() %>% 
  dplyr::mutate(
    model = factor(.data$model, levels = c(names(x@detectors), "event"))
  ) %>% 
  tidyr::spread("model", "pct.correct")
}