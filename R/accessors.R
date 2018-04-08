#' @name accessors
#' @title BANTER class accessor functions
#' @description Accessor functions for components of BANTER models
#' 
#' @param x a \code{\link{banter_model}} object.
#' @param model model to retrieve.
#' 
#' @export
#' 
getBanterModel <- function(x, model = "event") {
  if(model == "event") return(x@model)
  if(model %in% names(x@detectors)) return(x@detectors[[model]]@model)
  stop(paste0("detector model '", model, "' not found."))
}