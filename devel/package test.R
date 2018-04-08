rm(list = ls())
library(tidyverse)
library(banter)
load("data/calcurceas data.rdata")

ntree <- 100
sampsize <- 10

# test full CalCurCEAS data
mdl <- initBanterModel(survey.train$events)
mdl <- addBanterDetector(mdl, survey.train$detectors$bp, ntree, sampsize) 
mdl <- addBanterDetector(mdl, survey.train$detectors$dw, ntree, sampsize) 
mdl <- addBanterDetector(mdl, survey.train$detectors$ec, ntree, sampsize) 
mdl

mdl <- runBanterModel(mdl, 5000, 3)
summary(mdl)

test.pred <- survey.test$events %>% 
  select(event.id, species) %>% 
  left_join(predict(mdl, survey.test), by = "event.id")


# test package example data
data(train.data)
ex.mdl <- initBanterModel(train.data$events) %>% 
  addBanterDetector(train.data$detectors, ntree, 1) %>% 
  runBanterModel(5000, 1)
summary(ex.mdl)

data(test.data)
predict(ex.mdl, test.data)

save.image("data/test ws.rdata")
