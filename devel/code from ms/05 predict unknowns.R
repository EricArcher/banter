rm(list = ls())
library(parallel)
source("ac class funcs.r")
load("event list filtered.rdata")
load("event rf filtered.rdata")
load("ct rf filtered.rdata")
options(mc.cores = 4)

# prediction function
pred.ev <- function(ev, ev.list, ct.rf, event.rf, ac.df) {
  stopifnot(require(randomForest))
  call.probs <- pred.ct.list(ev.list[[ev]], ct.rf)
  if(any(is.nan(call.probs))) return(NULL)

  # get whistle rate
  dw <- ev.list[[ev]]$dw
  dw.n <- if(is.null(dw)) 0 else nrow(dw)
  dw.rate <- dw.n / ac.df[ev, "ev.duration"]
  if(is.nan(dw.rate) | is.na(dw.rate)) dw.rate <- 0
  call.probs <- c(call.probs, dw.rate = dw.rate)

  votes <- predict(event.rf, call.probs, type = "vote")
  spp <- colnames(votes)[which.max(votes)]
  list(votes = votes, spp = spp)
}

# predict all unknowns
test.df <- subset(ac.df, !training)
test.pred <- mclapply(test.df$ac.id, function(ev) {
  #cat(ev, "\n")
  pred.ev(ev, ev.list, ct.rf, event.rf, ac.df)
})
names(test.pred) <- test.df$ac.id
test.pred <- test.pred[!sapply(test.pred, is.null)]

# summarize predictions
votes <- do.call(rbind, sapply(test.pred, function(x) x$votes, simplify = FALSE))
rownames(votes) <- names(test.pred)
pred.spp <- sapply(test.pred, function(x) x$spp)
spp <- test.df[names(pred.spp), "species..vis"]
pred.spp <- factor(pred.spp, levels = colnames(votes))

votes.df <- cbind(data.frame(orig.spp = spp, pred.spp = pred.spp), votes)

save.image("unk prediction.rdata")

write.csv(table(votes.df$orig.spp, votes.df$pred.spp), file = "unknown predictions.csv")

source("05a plot vote probability.r")
