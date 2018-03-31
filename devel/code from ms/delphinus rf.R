rm(list = ls())
library(rfPermute)
load("event rf filtered.rdata")
load("ac.df.rdata")

spp <- factor(ac.df[rownames(event.mat), "species..vis"])

delph <- spp %in% c("16", "17")
delph.mat <- event.mat[delph, ]
delph.spp <- droplevels(spp[delph])
spp.freq <- table(delph.spp)

delph.rf <- rfPermute(
  delph.mat, delph.spp, sampsize = spp.freq - 1, replace = FALSE,
  importance = TRUE, ntree = 5000, proximity = TRUE, do.trace = 250, nrep = 1000
)
delph.rf
randomForest:::plot.randomForest(delph.rf)
proximity.plot(delph.rf, legend = "topright")
plot(rp.importance(delph.rf))

save(delph.rf, delph.mat, delph.spp, file = "delphinus rf.rdata")
