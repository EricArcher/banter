#' @title Mean votes
#' @description Returns matrix of mean species vote percentage by event
#' 
#' @param x detector_model object
#' 
meanVotes <- function(x) {
  df <- do.call(
    rbind,
    tapply(1:nrow(x@model$votes), x@ids$event.id, function(i) {
      colMeans(x@model$votes[i, , drop = FALSE])
    })
  ) 
  df <- as.data.frame(df)
  colnames(df) <- paste0(x@name, ".", colnames(df))
  cbind(event.id = rownames(df), df, stringsAsFactors = FALSE)
}