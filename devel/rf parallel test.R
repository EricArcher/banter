library(parallel)
library(tidyverse)
library(randomForest)
library(banter)

sampsize <- 5
ntree <- 1000
num.cores <- 3
event.df <- survey.train$events
detector.df <- survey.train$detectors[["bp"]]

df <- event.df %>% 
  dplyr::select(.data$event.id, .data$species) %>% 
  dplyr::inner_join(detector.df, by = "event.id") %>% 
  dplyr::mutate(species = as.character(.data$species)) %>% 
  as.data.frame

sampsize <- banter:::.getSampsize(
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

predictors <- dplyr::select(df, -.data$event.id, -.data$call.id, -.data$species)
response <- df$species

if(is.null(num.cores)) num.cores <- parallel::detectCores() - 1
rf <- if(num.cores == 1) {
  randomForest::randomForest(
    x = predictors, y = response, ntree = cl.ntree, sampsize = sampsize, 
    replace = FALSE, importance = FALSE, proximity = FALSE
  )
} else {
  rfFunc <- function(i, predictors, response, cl.ntree, sampsize) {
    randomForest::randomForest(
      x = predictors, y = response, ntree = cl.ntree, sampsize = sampsize, 
      replace = FALSE, importance = FALSE, proximity = FALSE
    )
  } 
  
  rf.list <- tryCatch({
    # Create clusters
    cl.ntree <- ceiling(ntree / num.cores)
    cl <- parallel::makeCluster(num.cores)
    env <- environment()
    
    # Export and load the loaded packages to the clusters
    loaded.packages <- c(
      utils::sessionInfo()$basePkgs, 
      names(utils::sessionInfo()$otherPkgs)
    )
    parallel::clusterExport(cl = cl, varlist = "loaded.packages", envir = env)
    parallel::clusterCall(cl, function(i) {
      lapply(loaded.packages, require, character.only = TRUE)
    })
    parallel::clusterExport(
      cl, c("predictors", "response", "cl.ntree"), envir = env
    )
    
    # Run the model on the clusters
    parallel::parLapply(
      cl = cl, X = 1:num.cores, fun = rf.func, 
      predictors = predictors, response = response, 
      cl.ntree = cl.ntree, sampsize = sampsize
    )
  }, finally = parallel::stopCluster(cl))
  do.call(randomForest::combine, rf.list)
}
