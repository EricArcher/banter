#' @title Number of Calls
#' @description Returns matrix of number of calls by each detector for each event
#' 
#' @param x event_model object
#' 
#' @importFrom magrittr %>%
#' @importFrom plyr .
#' @importFrom rlang .data :=
#' 
numCalls <- function(x) {
  lapply(x@detectors, function(d) {
    table(event.id = d@ids$event.id) %>% 
      as.data.frame(stringsAsFactors = FALSE) %>% 
      dplyr::right_join(dplyr::select(x@data, .data$event.id), by = "event.id") %>% 
      dplyr::rename(!!d@name := .data$Freq) %>% 
      tidyr::gather("detector", "freq", -.data$event.id)
  }) %>%
    dplyr::bind_rows() %>% 
    tidyr::spread("detector", "freq") %>% 
    replace(is.na(.), 0)
}