#' @title Mean votes
#' @description mean votes
#' 
#' @param x detector_model object
#' 
meanVotes <- function(x) {
  df <- do.call(
    rbind,
    tapply(1:nrow(model(x)$votes), ids(x)$event.id, function(i) {
      colMeans(model(x)$votes[i, ])
    })
  ) 
  df <- as.data.frame(df)
  colnames(df) <- paste0("<", name(x), ">", colnames(df))
  cbind(event.id = rownames(df), df, stringsAsFactors = FALSE)
}