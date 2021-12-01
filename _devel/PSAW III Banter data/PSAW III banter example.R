rm(list = ls())
library(banter)
library(rfPermute)

train <- readRDS("PSAW_Banter_Train.rds")
test <- readRDS("PSAW_Banter_Test.rds")

train$detectors <- subsampleDetections(train$detectors, 100)
psaw.b <- initBanterModel(train$events) %>% 
  addBanterDetector(train$detectors, ntree = 10000, sampsize = 1, num.cores = 6) %>% 
  runBanterModel(ntree = 10000, sampsize = 1)

psaw.rf <- getBanterModel(psaw.b)

classPriors(psaw.rf, sampsize = rep(1, length(unique(train$events$species))))

            