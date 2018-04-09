#' @name addBanterDetector
#' @title Add a BANTER Detector Model
#' @description Add a detector model to a BANTER classifier.
#'
#' @param x a \code{\link{banter_model}} object.
#' @param name name of detector.
#' @param data detector data.frame.
#' @param ntree number of trees.
#' @param sampsize number or fraction of samples to use in each tree.
#' @param num.cores number of cores to use for Random Forest model.
#' 
#' @return a \code{\link{banter_model}} object with the detector model added or 
#'   removed.
#' 
#' @author Eric Archer \email{eric.archer@@noaa.gov}
#' 
#' @examples
#' data(train.data)
#' # initialize BANTER model with event data
#' bant.mdl <- initBanterModel(train.data$events)
#' # add the 'bp' (burst pulse) detector model
#' bant.mdl <- addBanterDetector(
#'   bant.mdl, train.data$detectors$bp, 
#'   ntree = 50, sampsize = 1, num.cores = 1
#' )
#' bant.mdl
#' 
#' # remove the 'bp' detector model
#' bant.mdl <- removeBanterDetector(bant.mdl, "bp")
#' bant.mdl
#' 
#' @importFrom rlang .data
#' @importFrom magrittr %>%
#' @export
#' 
addBanterDetector <- function(x, data, ntree, sampsize = 1, num.cores = NULL) {
  if(is.null(x@detectors)) x@detectors <- list()
  if(methods::is(data, "list")) {
    d <- lapply(data, function(detector.df) {
      name <- attr(detector.df, "name")
      .runDetectorModel(x, name, detector.df, ntree, sampsize, num.cores)
    })
    d <- setNames(d, sapply(data, attr, which = "name"))
    x@detectors[names(d)] <- d
  } else {
    name <- attr(data, "name")
    x@detectors[[name]] <- .runDetectorModel(x, name, data, ntree, sampsize, num.cores)
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

#' @importFrom parallel detectCores makeCluster clusterExport clusterEvalQ parLapply stopCluster mclapply
#' @importFrom randomForest randomForest combine
#' @importFrom utils sessionInfo
#' @keywords internal
#' 
.runDetectorModel <- function(x, name, data, ntree, sampsize = 1, num.cores = NULL) {
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

  params <- list(
    predictors = df %>% 
      dplyr::select(-.data$event.id, -.data$call.id, -.data$species),
    response = df$species,
    sampsize = sampsize
  )
  
  if(is.null(num.cores)) num.cores <- parallel::detectCores() - 1
  rf <- if(num.cores == 1) {
    params$ntree <- ntree
    .rfFuncDetector(params) 
  } else {
    .clRF <- function(i, params) .rfFuncDetector(params)
    params$ntree <- ceiling(ntree / num.cores)
    
    rf.list <- if(Sys.info()[["sysname"]] %in% c("Linux", "Darwin")) {
      parallel::mclapply(
        1:num.cores, .clRF, params = params, mc.cores = num.cores
      )
    } else {
      tryCatch({
        cl <- parallel::makeCluster(num.cores)
        parallel::clusterEvalQ(cl, require(randomForest))
        parallel::clusterExport(cl, "params", environment())
        parallel::parLapply(cl, 1:num.cores, .clRF, params = params)
      }, finally = parallel::stopCluster(cl))
    }
    do.call(randomForest::combine, rf.list)
  }
  
  new(
    "banter_detector",
    name = name,
    ids = df[, c("event.id", "call.id")], 
    model = rf
  ) 
}