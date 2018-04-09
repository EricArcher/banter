rm(list = ls())
library(tidyverse)
library(banter)

## test package example data

data(train.data)
ex.mdl <- initBanterModel(train.data$events) %>%
  addBanterDetector(train.data$detectors, ntree = 200, sampsize = 1) %>%
  runBanterModel(ntree = 1000, sampsize = 1)
summary(ex.mdl)

data(test.data)
predict(ex.mdl, test.data)





## test CalCurCEAS data

load("data/calcurceas data.rdata")

ntree <- 50
sampsize <- 1
num.cores <- 4

mdl <- initBanterModel(survey.train$events)

mdl <- addBanterDetector(
  mdl, 
  list(bp = survey.train$detectors$bp, dw = survey.train$detectors$dw), 
  ntree = ntree,
  sampsize = sampsize,
  num.cores = num.cores
)

mdl <- addBanterDetector(mdl, survey.train$detectors$ec, "ec", ntree, sampsize, num.cores)

mdl <- runBanterModel(mdl, 200, 1)
summary(mdl)

predict(mdl, survey.test)

test.pred <- survey.test$events %>% 
  select(event.id, species) %>% 
  left_join(predict(mdl, survey.test), by = "event.id")
test.pred

#save.image("data/test ws.rdata")
