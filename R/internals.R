#' Produces a vector of equal sample sizes for balanced Random Forest model.
#' @name internals
#' @importFrom stats setNames
#' @keywords internal
#' 
.getSampsize <- function(x, n, warn.label) {
  if(n < 0) n <- 1
  freq <- if(is.table(x)) x else table(x)
  n <- if(n >= 1) n else round(min(freq) * n, 0)
  if(n == 0) n <- 1
  
  bad.freq <- freq[freq <= n]
  good.freq <- freq[freq > n]
  
  if(length(good.freq) < 2) {
    stop(paste0(
      warn.label, ":\n",
      "sampsize = ", n,
      " is >= too many species frequencies (need at least 2 for model):\n",
      paste0("  ", names(freq), ": ", freq, "\n", collapse = "")
    ))
  } else if(length(bad.freq) > 0) {
    warning(paste0(
      warn.label, ": sampsize = ", n, " is >= species frequencies:\n",
      paste0("  ", names(bad.freq), ": ", bad.freq, "\n", collapse = ""),
      "These species will be used in the model:\n",
      paste0("  ", names(good.freq), ": ", good.freq, "\n", collapse = "")
    ), call. = FALSE, immediate. = TRUE) 
    setNames(rep(n, length(good.freq)), names(good.freq))
  } else {
    setNames(rep(n, length(freq)), names(freq))
  }
}

#' Returns matrix of mean species vote percentage by event
#' @rdname internals
#' @keywords internal
.meanVotes <- function(x) {
  sapply(names(x), function(d) {
    x[[d]] %>% 
      tidyr::gather("species", "prob", -.data$event.id) %>% 
      dplyr::mutate(species = paste0(d, ".", .data$species)) %>% 
      dplyr::group_by(.data$event.id, .data$species) %>% 
      dplyr::summarize(prob.mean = mean(.data$prob)) %>% 
      dplyr::ungroup() 
  }, simplify = FALSE) %>% 
    dplyr::bind_rows() %>% 
    tidyr::spread("species", "prob.mean") %>% 
    replace(is.na(.), 0) 
}

#' Returns matrix of number of calls by each detector for each event
#' @rdname internals
#' 
#' @importFrom magrittr %>%
#' @importFrom plyr .
#' @importFrom rlang .data :=
#' 
#' @keywords internal
#' 
.numCalls <- function(x) {
  lapply(x@detectors, function(d) {
    table(event.id = d@ids$event.id) %>% 
      as.data.frame(stringsAsFactors = FALSE) %>% 
      dplyr::mutate(detector = d@name)
  }) %>%
    dplyr::bind_rows() %>% 
    tidyr::spread("detector", "Freq") %>% 
    replace(is.na(.), 0)
}

#' Returns matrix of number of calls by each detector for each event
#' @rdname internals
#' 
#' @importFrom magrittr %>%
#' @importFrom plyr .
#' @importFrom rlang .data :=
#' 
#' @keywords internal
#' 
.propCalls <- function(x) {
  lapply(names(x), function(d) {
    table(event.id = x[[d]]) %>% 
      as.data.frame(stringsAsFactors = FALSE) %>% 
      dplyr::mutate(detector = d)
  }) %>%
    dplyr::bind_rows() %>% 
    dplyr::group_by(.data$event.id) %>% 
    dplyr::mutate(
      prop = .data$Freq / sum(.data$Freq),
      detector = paste0(.data$detector, ".proportion")
    ) %>% 
    dplyr::ungroup() %>% 
    dplyr::select(-.data$Freq) %>% 
    tidyr::spread("detector", "prop") %>% 
    replace(is.na(.), 0) 
}