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

ntree <- 1000
sampsize <- 5

ev.mdl <- event.df %>% 
  filter(training) %>% 
  select(event.id, species) %>% 
  mutate(species = gsub(" ", "", species)) %>% 
  event_model()

ev.mdl <- addDetectorModel(ev.mdl, detectors$bp, ntree, sampsize) 
ev.mdl <- addDetectorModel(ev.mdl, detectors$dw, ntree, sampsize) 
ev.mdl <- addDetectorModel(ev.mdl, detectors$ec, ntree, sampsize) 
ev.mdl

ev.mdl <- buildEventModel(ev.mdl, 1000, 3)
summary(ev.mdl)

test.pred <- predict(ev.mdl, test.data)
test.pred

# ev.mdl2 <- addDetectorModel(ev.mdl, detectors, ntree, sampsize)
# ev.mdl2 <- buildEventModel(ev.mdl2, 50, 3)
# summary(ev.mdl2)
# 
# test.pred2 <- predict(ev.mdl2, test.data)
# test.pred2

save.image("data/test ws.rdata")
