#' @title Initialize BANTER model
#' @description Initialize a BANTER model with event data.
#'
#' @param x a data.frame of events. Every row is a unique event. Must have 
#'   columns named \code{event.id} and \code{species}. All other 
#'   columns will be used as predictor variables for the BANTER event classifier 
#'   model.
#'   
#' @return a \code{\link{banter_model}} object without any detector models.
#' 
#' @author Eric Archer \email{eric.archer@@noaa.gov}
#' 
#' @examples
#' data(train.data)
#' # initialize BANTER model with event data
#' bant.mdl <- initBanterModel(train.data$events)
#' bant.mdl
#' 
#' @export
#' 
initBanterModel <- function(x) {
  banter_model(data = x, detectors = NULL, model = NULL)
}