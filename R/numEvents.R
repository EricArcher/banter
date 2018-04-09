#' @title Number of Events
#' @description Number of events in BANTER model by species.
#' 
#' @param x a \code{\link{banter_model}} object.
#' 
#' @return a data.frame giving the number of events available for each species.
#' @param model name of model to extract. Default is \code{"event"} 
#'   to summarize the event-level model. Can also be name of a detector.
#' 
#' @author Eric Archer \email{eric.archer@@noaa.gov}
#' 
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' 
#' @export
#' 
numEvents <- function(x, model = "event") {
  .checkModelName(x, model)
  df <- if(model == "event") {
    x@data 
  } else {
    x@data %>% 
      dplyr::filter(.data$event.id %in% x@detectors[[model]]@ids$event.id)
  }
  spp.fac <- factor(df$species, levels = sort(unique(x@data$species)))
  table(species = spp.fac) %>% 
    as.data.frame() %>% 
    setNames(c("species", "num.events")) %>% 
    dplyr::mutate(species = as.character(.data$species)) 
}
