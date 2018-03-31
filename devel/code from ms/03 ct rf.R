rm(list = ls())
library(randomForest)
library(parallel)
options(mc.cores = 1)
source("ac class funcs.r")
load("event list filtered.rdata")

ran.n <- 10000 # maximum number of calls to randomly sample
n <- 1000 # number of representative calls for distillation
train.n <- 80 # number of samples used to train each tree in random forest
ntree <- 10000 # number of trees in random forest model

# list of logicals determining if column needs to be log-transformed (lt)
#   for distillation
lt.dw = c(rep(TRUE, 18), rep(FALSE, 4), rep(TRUE, 3), rep(FALSE, 7),
            rep(TRUE, 6), rep(FALSE, 3), TRUE, FALSE, FALSE, TRUE,
            rep(FALSE, 5))
lt.bp <- c(lt.dw, FALSE, FALSE)
lt.ec = c(TRUE, FALSE, FALSE, TRUE, FALSE, TRUE, TRUE, FALSE, TRUE, TRUE, FALSE)
lt.list <- list(bp = lt.bp, dw = lt.dw, ec = lt.ec)

train.ac.df <- subset(ac.df, training)

# subset call type data for each species
ct.sub <- sapply(c("bp", "dw", "ec"), function(ct) {
  do.call(rbind, mclapply(unique(train.ac.df$species), function(spp) {
    cat(ct, as.character(spp), "\n")
    mat <- spp.calls(spp, ct, train.ac.df, ev.list)
    if(is.null(mat)) return(NULL)
    # subsample entire matrix before distilling
    mat <- mat[sample(1:nrow(mat), min(nrow(mat), ran.n)), ]
    # choose n representative rows
    mat <- distill(mat, n, lt.list[[ct]])
    spp <- data.frame(spp = rep(spp, nrow(mat)), stringsAsFactors = FALSE)
    df <- cbind(spp, mat)
    rownames(df) <- rownames(mat)
    df
  }))
}, simplify = FALSE)


# call type classifier
ct.rf <- mclapply(ct.sub, function(df) {
  df <- na.omit(df)
  spp <- factor(df$spp, levels = intersect(levels(ac.df$species), df$spp))
  df <- df[, -1]
  spp.freq <- table(spp)
  # half of each sample size
  sampsize <- ceiling(spp.freq / 2)
  # set sampsize to smallest half that is greater than minimum train size
  #sampsize <- ifelse(sampsize < train.n, sampsize, min(sampsize[sampsize >= train.n]))
  # set sampsize to either the smallest half or train.n
  min.size <- max(min(sampsize), train.n)
  sampsize[sampsize > min.size] <- min.size
  randomForest(
    df, spp, ntree = ntree, sampsize = sampsize, replace = FALSE,
    keep.forest = TRUE, keep.inbag = TRUE, do.trace = 500, importance = TRUE
  )
})

save(ct.sub, ct.rf, file = "ct rf filtered.rdata")

# write importance scores
for(ct in names(ct.rf)) {
  write.csv(ct.rf[[ct]]$importance, file = paste(ct, "importance scores.csv"))
}

# write confusion matrices
for(x in names(ct.rf)) {
  write.csv(ct.rf[[x]]$confusion, file = paste(x, "confusion matrix.csv"))
  cat("\n", x, "\n")
  print(table(ct.sub[[x]]$spp))
  print(ct.rf[[x]])
}

source("03a ct rf summary.r")
source("04 event rf.r")
