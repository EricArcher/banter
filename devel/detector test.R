rm(list = ls())
library(tidyverse)
library(banter)

readDetector <- function(file, name) {
  df <- read.csv(file, stringsAsFactors = FALSE)
  attr(df, "name") <- name
  df
}

event.df <- read.csv("data/event.df.csv", stringsAsFactors = FALSE)
detectors <- list(
  bp = readDetector("data/detector - bp.csv", "bp"),
  dw = readDetector("data/detector - dw.csv", "dw"),
  ec = readDetector("data/detector - ec.csv", "ec")
)

test.data <- list(
  event = event.df %>% 
    filter(!training) %>% 
    select(event.id, species) %>% 
    mutate(species = gsub(" ", "", species)),
  detectors = sapply(detectors, function(x) {
    filter(x, event.id %in% filter(event.df, !training)$event.id)
  })
)

ntree <- 5000
sampsize <- 100

b.mdl <- event.df %>% 
  filter(training) %>% 
  select(event.id, species) %>% 
  mutate(species = gsub(" ", "", species)) %>% 
  initBanterModel()

b.mdl <- addBanterDetector(b.mdl, detectors$bp, ntree, sampsize) 
b.mdl <- addBanterDetector(b.mdl, detectors$dw, ntree, sampsize) 
b.mdl <- addBanterDetector(b.mdl, detectors$ec, ntree, sampsize) 
b.mdl

b.mdl <- runBanterModel(b.mdl, 5000, 3)
summary(b.mdl)

test.pred <- predict(b.mdl, test.data)
test.pred

test.data$event %>% 
  select(event.id, species) %>% 
  right_join(test.pred, by = "event.id")

# b.mdl2 <- addDetectorModel(b.mdl, detectors, ntree, sampsize)
# b.mdl2 <- buildEventModel(b.mdl2, 50, 3)
# summary(b.mdl2)
# 
# test.pred2 <- predict(b.mdl2, test.data)
# test.pred2

save.image("data/test ws.rdata")
