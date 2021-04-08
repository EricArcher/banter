rm(list = ls())
library(tidyverse)
load("data/calcurceas data.rdata")

# training data ------------------
train.data <- list(
  events = survey$events %>% 
    filter(training) %>% 
    # group_by(species) %>% 
    # do(sample_n(., min(nrow(.), 5))) %>% 
    # ungroup() %>% 
    mutate(species = gsub(" ", "", species)) %>%
    select(event.id, species, duration) %>% 
    as.data.frame
)
train.data$detectors <- sapply(survey$detectors, function(d) {
  df <- d %>% 
    filter(event.id %in% train.data$events$event.id) %>% 
    group_by(event.id) %>% 
    do(sample_n(., min(nrow(.), 50))) %>% 
    ungroup() %>% 
    as.data.frame
  attr(df, "name") <- attr(d, "name")
  df
}, simplify = FALSE)


# test data -----------------------
test.data <- list(
  events = survey$events %>% 
    filter(!training & !is.na(species)) %>% 
    group_by(species) %>% 
    do(sample_n(., min(nrow(.), 1))) %>% 
    ungroup() %>% 
    mutate(species = gsub(" ", "", species)) %>% 
    select(event.id, species, duration) %>% 
    as.data.frame
)
test.data$detectors <- sapply(survey$detectors, function(d) {
  df <- d %>% 
    filter(event.id %in% test.data$events$event.id) %>% 
    group_by(event.id) %>% 
    do(sample_n(., min(nrow(.), 50))) %>% 
    ungroup() %>% 
    as.data.frame
  attr(df, "name") <- attr(d, "name")
  df
}, simplify = FALSE)


save(train.data, file = "../data/train.data.rdata")
save(test.data, file = "../data/test.data.rdata")