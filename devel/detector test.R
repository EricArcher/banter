rm(list = ls())
library(tidyverse)
library(banter)

bp <- read.csv("detector - bp.csv", stringsAsFactors = FALSE)
dw <- read.csv("detector - dw.csv", stringsAsFactors = FALSE)
ec <- read.csv("detector - ec.csv", stringsAsFactors = FALSE)
event.df <- read.csv("event.df.csv", stringsAsFactors = FALSE)

ev.sp.df <- event.df %>% 
  filter(training) %>% 
  select(event.id, species)

ntree <- 50
sampsize <- 1

bp.mdl <- detector_model("bp", bp, ev.sp.df, ntree, sampsize)
dw.mdl <- detector_model("dw", dw, ev.sp.df, ntree, sampsize)
ec.mdl <- detector_model("ec", ec, ev.sp.df, ntree, sampsize)
