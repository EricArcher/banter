rm(list = ls())
library(banter)

survey <- list(
  events = read.csv("data/event.df.csv", stringsAsFactors = FALSE),
  detectors = list(
    bp = read.csv("data/detector - bp.csv", stringsAsFactors = FALSE),
    dw = read.csv("data/detector - dw.csv", stringsAsFactors = FALSE),
    ec = read.csv("data/detector - ec.csv", stringsAsFactors = FALSE)
  )
)

ntree <- 1000
sampsize <- 1

ev.mdl <- survey$events %>% 
  filter(training) %>% 
  select(event.id, species) %>% 
  mutate(species = gsub(" ", "", species)) %>% 
  event_model() %>% 
  addDetectorModel("bp", survey$detectors$bp, ntree, sampsize) %>% 
  addDetectorModel("dw", survey$detectors$dw, ntree, sampsize) %>% 
  addDetectorModel("ec", survey$detectors$ec, ntree, sampsize)

mdl <- buildEventModel(ev.mdl, 1000, 2)
