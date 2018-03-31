rm(list = ls())
library(rfPermute)
source("ac class funcs.r")
load("event list filtered.rdata")
load("ct rf filtered.rdata")

train.ac.df <- subset(ac.df, training)
spp.ev.freq <- table(train.ac.df$species)
spp.to.use <- names(spp.ev.freq)[spp.ev.freq > 1]
train.ac.df <- subset(train.ac.df, species %in% spp.to.use)

# collect call probabilities for each event
event.probs <- sapply(rownames(train.ac.df), function(ac.id) {
  cat(ac.id, "\n")
  pred.ct.list(ev.list[[ac.id]], ct.rf)
}, simplify = FALSE)

# compile event data.frame
cols <- unique(unlist(lapply(event.probs, names)))
event.mat <- do.call(rbind, lapply(event.probs, function(x) {
  missing.cols <- setdiff(cols, names(x))
  missing.vec <- rep(0, length(missing.cols))
  names(missing.vec) <- missing.cols
  rbind(c(x, missing.vec)[cols])
}))
rownames(event.mat) <- rownames(train.ac.df)

# get whistle rate
dw.rate <- sapply(rownames(event.mat), function(ac.id) {
  dw <- ev.list[[ac.id]]$dw
  dw.n <- if(is.null(dw)) 0 else nrow(dw)
  dw.n / ac.df[ac.id, "ev.duration"]
})
event.mat <- cbind(event.mat, dw.rate = dw.rate)

to.keep <- apply(event.mat, 1, function(x) all(!is.nan(x)))
event.mat <- event.mat[to.keep, ]

# get species for each ac.id
spp <- droplevels(ac.df[rownames(event.mat), "species"])
spp.freq <- table(spp)
sampsize <- rep(min(ceiling(spp.freq / 2)), length(spp.freq))

# run RF
event.rf <- rfPermute(
  event.mat, spp, sampsize = sampsize, replace = FALSE,
  importance = TRUE, ntree = 15000, proximity = TRUE, do.trace = 500,
  nrep = 1000, num.cores = 4
)

# write confusion matrix
write.csv(event.rf$confusion, file = "event confusion matrix.csv")

save(event.mat, spp, event.rf, file = "event rf filtered.rdata")

randomForest:::plot.randomForest(event.rf)
proximity.plot(event.rf, legend = "topright")
print(event.rf)


source("04a event rf summary.r")
source("05 predict unknowns.r")
