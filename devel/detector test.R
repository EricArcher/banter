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

ntree <- 50
sampsize <- 100

ev.mdl <- event.df %>% 
  filter(training) %>% 
  select(event.id, species) %>% 
  mutate(species = gsub(" ", "", species)) %>% 
  event_model()

ev.mdl <- addDetectorModel(ev.mdl, detectors$bp, ntree, sampsize) 
ev.mdl <- addDetectorModel(ev.mdl, detectors$dw, ntree, sampsize) 
ev.mdl <- addDetectorModel(ev.mdl, detectors$ec, ntree, sampsize) 
ev.mdl

ev.mdl <- buildEventModel(ev.mdl, 50, 3)
ev.mdl

test.data <- list(
  event = event.df %>% 
    filter(!training) %>% 
    select(event.id, species) %>% 
    mutate(species = gsub(" ", "", species)),
  detectors = sapply(detectors, function(x) {
    filter(x, event.id %in% filter(event.df, !training)$event.id)
  })
)

predict(ev.mdl, test.data)

# ev.mdl2 <- addDetectorModel(ev.mdl, detectors, ntree, sampsize)
# ev.mdl2 <- buildEventModel(ev.mdl2, 5000, 3)
# ev.mdl2

save.image("data/test ws.rdata")
