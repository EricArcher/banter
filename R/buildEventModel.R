#' @title Build Event Model
#' @description Build full event classifier model
#'
#' @param x a \code{\link{event_model}} object.
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
buildEventModel <- function(x, ntree, sampsize = 1) {
  # Get proportion of detections by detector for each event
  detector.prop <- numCalls(x)
  detector.prop <- cbind(
    event.id = detector.prop$event.id,
    detector.prop %>% 
      dplyr::select(-.data$event.id) %>% 
      as.matrix() %>% 
      prop.table(1) %>% 
      as.data.frame() %>% 
      setNames(paste0(colnames(.), ".proportion"))
  )
  
  # Get vote means for each event
  vote.means <- lapply(x@detectors, function(d) {
    meanVotes(d) %>% 
      tidyr::gather("detector.species", "prob", -.data$event.id)
  }) %>% 
    dplyr::bind_rows() %>% 
    tidyr::spread("detector.species", "prob") %>% 
    replace(is.na(.), 0)
  
  df <- x@data %>% 
    dplyr::left_join(detector.prop, by = "event.id") %>% 
    dplyr::left_join(vote.means, by = "event.id") %>% 
    dplyr::filter(complete.cases(.))
  
  freq <- table(df$species)
  to.remove <- names(freq)[freq <= sampsize]
  if(length(to.remove) > 0) {
    warning(
      "some species have <= ", sampsize, " events, and have been removed:\n",
      paste0(names(freq[to.remove]), ":", freq[to.remove], "\n", collapse = "")
    )
  }
  
  df <- df %>% 
    dplyr::filter(!.data$species %in% to.remove) %>% 
    dplyr::mutate(species = factor(.data$species)) %>% 
    tibble::column_to_rownames("event.id") %>% 
    as.data.frame() %>% 
    droplevels()
  
  if(length(unique(df$species)) < 2) {
    stop("need at least two species to build event classifier")
  }
  
  x@model <- randomForest::randomForest(
    species ~ ., 
    data = df,
    ntree = ntree, 
    sampsize = getSampsize(df$species, sampsize), 
    replace = FALSE,
    importance = TRUE
  )
  
  x
}