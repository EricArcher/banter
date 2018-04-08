#' @name addBanterDetector
#' @title Add a BANTER Detector Model
#' @description Add a detector model to a BANTER classifier.
#'
#' @param x a \code{\link{banter_model}} object.
#' @param name name of detector
#' @param data detector data.frame
#' @param ntree number of trees
#' @param sampsize number or fraction of samples to use in each tree
#' 
#' @author Eric Archer \email{eric.archer@@noaa.gov}
#' 
#' @importFrom rlang .data
#' @importFrom magrittr %>%
#' @export
#' 
addBanterDetector <- function(x, data, ntree, sampsize = 1) {
  if(is.null(x@detectors)) x@detectors <- list()
  if(methods::is(data, "list")) {
    d <- lapply(data, function(detector.df) {
      name <- attr(detector.df, "name")
      .runDetectorModel(x, name, detector.df, ntree, sampsize)
    })
    d <- setNames(d, sapply(data, attr, which = "name"))
    x@detectors[names(d)] <- d
  } else {
    name <- attr(data, "name")
    x@detectors[[name]] <- .runDetectorModel(x, name, data, ntree, sampsize)
  }
  x@detectors <- x@detectors[order(names(x@detectors))]
  x@model <- NULL
  x
}

#' @rdname addBanterDetector
#' @export
#' 
removeBanterDetector <- function(x, name) {
  x@detectors[[name]] <- NULL
  if(length(x@detectors) == 0) x@detectors <- NULL
  x@model <- NULL
  x
}

.runDetectorModel <- function(x, name, data, ntree, sampsize = 1) {
  df <- x@data %>% 
    dplyr::select(.data$event.id, .data$species) %>% 
    dplyr::inner_join(data, by = "event.id") %>% 
    dplyr::mutate(species = as.character(.data$species)) %>% 
    as.data.frame
  
  sampsize <- .getSampsize(
    df$species, 
    sampsize, 
    paste0("Detector model (", name, ")")
  )
  
  df <- df %>% 
    dplyr::filter(.data$species %in% names(sampsize)) %>% 
    dplyr::mutate(species = factor(.data$species)) %>% 
    dplyr::mutate(id = paste0(.data$event.id, ".", .data$call.id)) %>% 
    tibble::column_to_rownames("id") %>% 
    as.data.frame() %>% 
    droplevels()
  
  rf <- randomForest::randomForest(
    species ~ ., 
    data = dplyr::select(df, -.data$event.id, -.data$call.id),
    ntree = ntree, 
    sampsize = sampsize, 
    replace = FALSE,
    importance = TRUE
  )
  
  new(
    "banter_detector",
    name = name,
    ids = df[, c("event.id", "call.id")], 
    model = rf
  ) 
}