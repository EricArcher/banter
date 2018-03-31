rm(list = ls())
load("ct rf filtered.rdata")

measures <- lapply(ct.rf, function(x) names(x$forest$xlevels))
for(x in names(measures)) {
  write.csv(measures[[x]], file = paste(x, "predictors.csv"), row.names = F)
}