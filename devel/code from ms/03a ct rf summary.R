rm(list = ls())
library(rfPermute)
library(reshape2)
library(ggplot2)
library(gridExtra)
library(grid)
load("ct rf filtered.rdata")

n <- 1

imp.rank <- lapply(c("bp", "dw", "ec"), function(ct) {
  imp <- read.csv(paste(ct, "importance scores.csv"), row.names = 1, check.names = FALSE)
  imp$Measure <- rownames(imp)
  imp <- imp[order(imp$MeanDecreaseAccuracy), ]
  measures <- imp$Measure
  spp <- levels(ct.rf[[ct]]$predicted)
  for(x in spp) imp[[x]] <- rank(-imp[[x]], ties = "first")
  num.vars <- min(c(n, nrow(imp)))
  top.imp <- sapply(spp, function(x) {
    rownames(imp)[which(imp[, x] <= num.vars)]
  })
  top.imp <- unique(unlist(top.imp))
  imp <- imp[top.imp, spp]
  imp$Measure <- rownames(imp)
  imp <- melt(imp[, c("Measure", spp)], id.vars = "Measure",
              variable.name = "Species", value.name = "Rank")
  imp$Species <- factor(imp$Species, levels = spp)
  measures <- union(measures, imp$Measure)
  imp$Measure <- factor(imp$Measure, levels = measures)
  title <- switch(ct, bp = "A) Burst Pulses",
                  dw = "B) Dolphin Whistles",
                  ec = "C) Echolocation Clicks")

  ggplot(imp, aes(Species, Measure)) +
    geom_raster(aes(fill = Rank)) +
    scale_fill_gradient2(low = "#a50026", mid = "#ffffbf", high = "#313695",
                         midpoint = mean(range(imp$Rank))) +
    geom_text(aes(label = Rank, fontface = ifelse(Rank == 1, "bold", "plain"))) +
    ggtitle(title) +
    theme(
      axis.title = element_blank(),
      axis.text = element_text(size = 14),
      axis.text.x = element_text(angle = 45, hjust = 1, face = "italic"),
      plot.title = element_text(hjust = 0),
      legend.position = "none"
    )
})



pdf("Call Classification Variable Importance Rankings.pdf", height = 11, width = 8.5)

do.call(grid.arrange, imp.rank)
for(ct in names(ct.rf)) {
  title <- switch(ct, bp = "Burst Pulses",
                  dw = "Dolphin Whistles",
                  ec = "Echolocation Clicks")
  g <- impHeatmap(ct.rf[[ct]], plot = F) +
    ggtitle(title) +
    theme(
      text = element_text(size = 14),
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "none")
  print(g)
}

dev.off()


png("Figure 2 - call type heatmap.png", width = 8.5, height = 11, units = "in", res = 600)
do.call(grid.arrange, imp.rank)
dev.off()


for(i in 1:length(ct.rf)) {
  title <- switch(names(ct.rf)[i],
                  bp = "Burst Pulses",
                  dw = "Dolphin Whistles",
                  ec = "Echolocation Clicks")
  png(paste("Figure Supplemental - ", title, ".png", sep = ""),
      width = 8.5, height = 11, units = "in", res = 600)
  g <- impHeatmap(ct.rf[[i]], plot = F) +
    ggtitle(title) +
    theme(
      text = element_text(size = 14),
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "none")
  print(g)
  dev.off()
}

