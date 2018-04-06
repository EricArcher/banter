#' @name accessors
#' @title BANTER class accessor functions
#' @description Accessor functions for slots in BANTER classes
#' 
#' @param x a \code{\link{event_model}} object.
#' @param model model to retrieve.
#' 
#' @export
#' 
getModel <- function(x, model = "event") {
  if(model == "event") return(x@model)
  if(model %in% names(x@detectors)) return(x@detectors[[model]]@model)
  stop(paste0("detector model '", model, "' not found."))
}