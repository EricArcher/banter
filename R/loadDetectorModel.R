#' @title Detector classification model
#' @description Create classification model for a detector
#' 
#' @param name detector name
#' @param detector.data measurement data for detections
#' @param event.species data.frame associating event.id with species
#' @param ntree number of trees in model
#' @param min.samp.n minimum number of samples per species for each tree
#' @param samp.pct fraction of samples to use in each tree
#' 
#' @author Eric Archer \email{eric.archer@@noaa.gov}
#' 
#' @importFrom rlang .data
#' @importFrom magrittr %>%
#' @export
#' 
loadDetectorModel <- function(
  name, detector.data, event.species, 
  ntree, samp.pct = 0.5, min.samp.n = 1
) {
  df <- detector.data %>% 
    dplyr::left_join(event.species, by = "event.id") %>% 
    dplyr::select(-.data$call.id)
  
  rf <- randomForest::randomForest(
    species ~ ., df, 
    ntree = ntree, 
    sampsize = getSampsize(df$species, samp.pct, min.samp.n), 
    importance = TRUE
  )
  
  detector_model(name = name, model = rf)
}