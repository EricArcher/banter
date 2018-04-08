#' @title Run BANTER Model
#' @description Build full event classifier model
#'
#' @param x a \code{\link{banter_model}} object.
#' @param ntree number of trees
#' @param sampsize number or fraction of samples to use in each tree
#' 
#' @author Eric Archer \email{eric.archer@@noaa.gov}
#' 
#' @importFrom magrittr %>%
#' @importFrom plyr .
#' @importFrom rlang .data
#' @importFrom stats complete.cases setNames
#' @export
#' 
runBanterModel <- function(x, ntree, sampsize = 1) {
  .stopIfNoDetectors(x)
  
  detector.prop <- propCalls(x, "event")
  
  detector.votes <- sapply(x@detectors, function(d) {
    cbind(
      event.id = d@ids$event.id, 
      as.data.frame(d@model$votes),
      stringsAsFactors = FALSE
    )
  }, simplify = FALSE) %>% 
    .meanVotes()
  
  df <- x@data %>% 
    dplyr::left_join(detector.prop, by = "event.id") %>% 
    dplyr::left_join(detector.votes, by = "event.id") %>% 
    dplyr::filter(complete.cases(.)) %>% 
    dplyr::mutate(species = as.character(.data$species)) 
  
  sampsize <- .getSampsize(df$species, sampsize, "Event model")
  
  df <- df %>% 
    dplyr::filter(.data$species %in% names(sampsize)) %>%
    dplyr::mutate(species = factor(.data$species)) %>% 
    tibble::column_to_rownames("event.id") %>% 
    as.data.frame() %>% 
    droplevels()
  
  x@model <- randomForest::randomForest(
    species ~ ., 
    data = df,
    ntree = ntree, 
    sampsize = sampsize, 
    replace = FALSE,
    importance = TRUE,
    proxmity = TRUE
  )
  
  x
}