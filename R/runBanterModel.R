#' @title Run BANTER Model
#' @description Build full event classifier model
#'
#' @param x a \code{\link{banter_model}} object.
#' @param ntree number of trees.
#' @param sampsize number or fraction of samples to use in each tree.
#' 
#' @return a \code{\link{banter_model}} object with the complete BANTER model.
#' 
#' @author Eric Archer \email{eric.archer@@noaa.gov}
#' 
#' @examples
#' data(train.data)
#' # initialize BANTER model with event data
#' bant.mdl <- initBanterModel(train.data$events)
#' # add all detector models
#' bant.mdl <- addBanterDetector(
#'   bant.mdl, train.data$detectors, 
#'   ntree = 50, sampsize = 1, num.cores = 1
#' )
#' # run BANTER event model
#' bant.mdl <- runBanterModel(bant.mdl, ntree = 1000, sampsize = 1)
#' summary(bant.mdl)
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
    votes <- d@model$votes %>% 
      prop.table(1) %>% 
      as.data.frame() 
    cbind(event.id = d@ids$event.id, votes, stringsAsFactors = FALSE)
  }, simplify = FALSE) %>% 
    .meanVotes()
  
  df <- x@data
  to.remove <- sapply(df, function(i) any(is.na(i)))
  if(any(to.remove)) {
    warning(
      "missing data found in the following columns: ",
      paste(colnames(df)[to.remove], collapse = ", "), "\n",
      "these columns will not be used in BANTER event model."
    )
    df <- df[, !to.remove]
  }
  
  df <- df %>% 
    dplyr::left_join(detector.prop, by = "event.id") %>% 
    dplyr::left_join(detector.votes, by = "event.id") 
  
  if("duration" %in% colnames(df)) {
    if(all(!is.na(df$duration))) {
      df <- df %>% 
        dplyr::left_join(
          numCalls(x, "event") %>% 
            tidyr::gather("detector", "num", -.data$event.id) %>% 
            dplyr::left_join(
              dplyr::select(df, "event.id", "duration"), 
              by = "event.id"
            ) %>% 
            dplyr::mutate(
              detector = gsub("num.", "rate.", .data$detector),
              num = .data$num / .data$duration
            ) %>% 
            dplyr::select(-.data$duration) %>% 
            tidyr::spread("detector", "num"),
          by = "event.id"
        )
    }
  }
      
  df <- df %>% 
    dplyr::filter(complete.cases(.)) %>% 
    dplyr::mutate(species = as.character(.data$species)) 
  
  sampsize <- .getSampsize(df$species, sampsize, "Event model")
  
  x@model.data <- df %>% 
    dplyr::filter(.data$species %in% names(sampsize)) %>%
    dplyr::mutate(species = factor(.data$species)) %>% 
    tibble::column_to_rownames("event.id") %>% 
    as.data.frame() %>% 
    droplevels()
  
  x@model <- randomForest::randomForest(
    species ~ ., 
    data = x@model.data,
    ntree = ntree, 
    sampsize = sampsize, 
    replace = FALSE,
    importance = TRUE,
    proximity = TRUE
  )
  x@sampsize <- sampsize
  
  x@timestamp <- Sys.time()
  
  x
}