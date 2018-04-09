rm(list = ls())
library(tidyverse)
library(banter)
load("data/calcurceas data.rdata")

ntree <- 1000
sampsize <- 1
num.cores <- NULL

# test full CalCurCEAS data
mdl <- initBanterModel(survey.train$events)
mdl <- addBanterDetector(mdl, survey.train$detectors$bp, ntree, sampsize, num.cores) 
mdl <- addBanterDetector(mdl, survey.train$detectors$dw, ntree, sampsize, num.cores) 
mdl <- addBanterDetector(mdl, survey.train$detectors$ec, ntree, sampsize, num.cores) 
mdl <- runBanterModel(mdl, 1000, 2)
summary(mdl)

test.pred <- survey.test$events %>% 
  select(event.id, species) %>% 
  left_join(predict(mdl, survey.test), by = "event.id")
test.pred

## test package example data
data(train.data)
ex.mdl <- initBanterModel(train.data$events) %>%
  addBanterDetector(train.data$detectors, ntree = 200, sampsize = 1) %>%
  runBanterModel(ntree = 1000, sampsize = 1)
summary(ex.mdl)

data(test.data)
predict(ex.mdl, test.data)

#save.image("data/test ws.rdata")
