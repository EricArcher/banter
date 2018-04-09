#' @name accessors
#' @title BANTER class accessor functions
#' @description Accessor functions for components of BANTER models
#' 
#' @param x a \code{\link{banter_model}} object.
#' @param model model to retrieve.
#' @param by return summary by \code{"species"} or \code{"event"}.
#' 
#' @export
#' 
getBanterModel <- function(x, model = "event") {
  if(model == "event") return(x@model)
  .checkModelName(x, model)
  x@detectors[[model]]@model
}

#' @rdname accessors
#' @export
#' 
getModelError <- function(x, model = "event") {
  rf <- getBanterModel(x, model) 
  mean(rf$y != rf$predicted)
}
  
#' @rdname accessors
#' @importFrom magrittr %>%
#' @importFrom rlang .data
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
    setNames(c("species", "events")) %>% 
    dplyr::mutate(species = as.character(.data$species)) 
}

#' @rdname accessors
#' @importFrom magrittr %>%
#' @importFrom plyr .
#' @importFrom rlang .data
#' @export
#' 
numCalls <- function(x, by = c("species", "event")) {
  if(is.null(x@detectors)) stop("no detectors loaded in model.")
  by <- switch(match.arg(by), species = "species", event = "event.id")
  lapply(names(x@detectors), function(d) {
    x@data %>% 
      dplyr::left_join(x@detectors[[d]]@ids, by = "event.id") %>% 
      dplyr::group_by(.dots = by) %>% 
      dplyr::summarize(n = sum(!is.na(.data$call.id))) %>% 
      dplyr::ungroup() %>% 
      dplyr::mutate(detector = paste0("num.", d))
  }) %>%
    dplyr::bind_rows() %>% 
    tidyr::spread("detector", "n") %>% 
    replace(is.na(.), 0) %>% 
    as.data.frame()
}

#' @rdname accessors
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export
#' 
propCalls <- function(x, by = c("species", "event")) {
  df <- numCalls(x, by)
  by <- colnames(df)[1]
  df %>% 
    tidyr::gather("detector", "n", -by) %>% 
    dplyr::group_by(.dots = by) %>%
    dplyr::mutate(prop = n / sum(n, na.rm = TRUE)) %>% 
    dplyr::ungroup() %>% 
    dplyr::select(-.data$n) %>% 
    dplyr::mutate(detector = gsub("num.", "prop.", .data$detector)) %>% 
    tidyr::spread("detector", "prop") %>% 
    as.data.frame()
}
