distill <- function(x, n, lt = NULL) {
  stopifnot(require(densityClust))
  # distills matrix or data frame with many records to subset with typical signatures

  # Arguments
  # x: object to be distilled
  # n: number of rows to which to distill
  # lt: logical vector of variables to log-transform

  orig.x <- x
  x <- x[!duplicated(x), , drop = FALSE]   # eliminate duplicate rows
  if(nrow(x) <= n) {
    cat("dim=", nrow(x), "x", ncol(x), "\n", sep = "")
    return(x)   # return all rows if <= nclusters
  }

  # only use variables with more than one value
  to.use <- apply(x, 2, function(v) length(unique(v)) > 1)
  x <- x[, to.use, drop = FALSE]
  lt <- if(is.null(lt)) rep(FALSE, ncol(x)) else lt[to.use]

  # log-transform specified vars
  x[, lt] <- apply(x[, lt], 2, function(v) {
    if(all(v <= 0)) v <- -v
    if(any(v < 0)) v <- v + min(v)
    if(any(v == 0)) v <- v + min(v[v > 0])
    log(v)
  })

  cat("dim=", nrow(x), "x", ncol(x), "\n", sep = "")

#   half.x <- ceiling(nrow(x) / 2)
#   sampsize <- if(half.x > (n * 2)) half.x else nrow(x)
#   i <- clara(x, k = n, stand = TRUE, samples = 100,
#         sampsize = sampsize, medoids.x = FALSE,
#         rngR = TRUE, pamLike = TRUE)$i.med
#   orig.x[i, ]

  x.dist <- dist(x)
  call.clust <- densityClust(x.dist)
  call.clust <- findClusters(call.clust, k = n)
  orig.x[names(call.clust$peaks), ]
}


spp.calls <- function(spp, ct, ac.df, ev.list) {
  # return matrix of calls for species across events
  ac.ids <- subset(ac.df, species == spp)$ac.id
  do.call(rbind, sapply(ac.ids, function(i) {
    ct.mat <- ev.list[[i]][[ct]]
    if(is.null(ct.mat)) NULL else ct.mat
  }, simplify = FALSE))
}

pred.ct <- function(df, rf) {
  # predict species for call types, but only use trees where sample was not inbag
  if(is.null(df)) return(NULL)
  if(nrow(df) == 0) return(NULL)

  stopifnot(require(randomForest))
  votes <- predict(rf, df, type = "vote")
  in.rf <- rownames(votes)[rownames(votes) %in% names(rf$predicted)]
  votes[in.rf, ] <- rf$votes[in.rf, ]
  votes
}

pred.ct.list <- function(ct.list, ct.rf) {
  result <- lapply(names(ct.list), function(ct) {
    probs <- pred.ct(ct.list[[ct]], ct.rf[[ct]])
    if(is.null(probs)) {
      cols <- levels(ct.rf[[ct]]$y)
      null.probs <- rep(0, length(cols))
      names(null.probs) <- paste(ct, cols, sep = ".")
      null.probs
    } else {
      colnames(probs) <- paste(ct, colnames(probs), sep = ".")
      colMeans(probs)
    }
  })
  result <- do.call(c, result)
  freqs <- do.call(c, lapply(names(ct.list), function(ct) {
    if(is.null(ct.list[[ct]])) 0 else nrow(ct.list[[ct]])
  }))
  freqs <- freqs / sum(freqs)
  names(freqs) <- paste(names(ct.list), ".pct", sep = "")
  c(freqs, result)
}

call.freq.smry <- function(df, ev.list) {
  sapply(c("bp", "dw", "ec"), function(ct) {
    sapply(sort(unique(df$species)), function(spp) {
      mat <- spp.calls(spp, ct, df, ev.list)
      if(is.null(mat)) return(0)
      nrow(mat)
    })
  })
}