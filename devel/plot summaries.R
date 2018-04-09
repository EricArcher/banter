library(tidyverse)
library(gridExtra)
library(rfPermute)
library(banter)

load("data/test ws.rdata")


predictedProbDist <- function(rf) {
  p <- rf$votes %>% 
    as.data.frame %>% 
    cbind(
      species = as.character(rf$y),
      predicted = as.character(rf$predicted)
    ) %>% 
    gather(prob.spp, prob, -species, -predicted) %>% 
    filter(predicted == prob.spp) %>% 
    mutate(correct = species == predicted) %>% 
    ggplot(aes(prob)) +
    geom_histogram(aes(fill = species), bins = 20) +
    facet_wrap(~ predicted) +
    ggtitle("Predictions")
  print(p)
}
predictedProbDist(getBanterModel(mdl))



rfSummary <- function(rf) {
  rf.trace <- plotRFtrace(rf, plot = FALSE)
  rf.votes <- plotVotes(rf, plot = FALSE)
  rf.conf <- plotConfMat(rf, plot = FALSE)
  rf.varimp <- impHeatmap(rf, n = 20, plot = FALSE)
  grid.arrange(rf.trace, rf.votes, rf.conf, rf.varimp, ncol = 2)
}

rfSummary(rf)
