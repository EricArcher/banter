library(ggplot2)
library(reshape2)
library(rfPermute)
source("ac class funcs.r")
load("event list filtered.rdata")
load("ct rf filtered.rdata")
load("event rf filtered.rdata")

train.ac.df <- subset(ac.df, training)

# compile data.frame of misclassified events
misclass <- event.rf$predicted != event.rf$y
misclass.df <- data.frame(event.mat[misclass, ])
predicted <- event.rf$predicted[misclass]
misclass.df <- cbind(ac.df[rownames(misclass.df), ], predicted = predicted, misclass.df)
write.csv(misclass.df, file = "misclass filtered.csv")

# record whether or not assignment was correct and probability of assignment to predicted species
correct.prob <- data.frame(spp = event.rf$y, pred = event.rf$predicted)
correct.prob$prob <- sapply(1:nrow(correct.prob), function(i) {
  event.rf$votes[i, correct.prob$pred[i]]
})
correct.prob$correct <- correct.prob$spp == correct.prob$pred

# collect votes
votes <- cbind(spp = event.rf$y, data.frame(event.rf$votes))
colnames(votes) <- gsub("X", "", colnames(votes))
votes[, 1] <- factor(votes[, 1], levels = sort(unique(votes[, 1]), dec = T))

# importance scores
imp <- rp.importance(event.rf)
write.csv(imp, file = "event importance.csv")

# plot summary figures
pdf("event rf summaries.pdf", height = 8.5, width = 11)

randomForest:::plot.randomForest(event.rf)

pp <- proximity.plot(event.rf, plot = FALSE)
g <- pp$g +
  scale_color_brewer(palette = "Set2") +
  theme(
    text = element_text(size = 16),
    legend.text = element_text(face = "italic")
  )
print(g)

plot(imp, sig.only = FALSE, type = c(levels(event.rf$y), "MeanDecreaseAccuracy"))
plot(imp, sig.only = TRUE, type = c(levels(event.rf$y), "MeanDecreaseAccuracy"))

for(x in rownames(imp)) {
  df <- cbind(event.rf$y, data.frame(event.mat[, x]),
              factor(correct.prob$correct), rownames(event.mat))
  colnames(df) <- c("Species", x, "Correct", "Event")
  df$jit <- jitter(as.numeric(df$Species))
  print(
    ggplot(df, aes_string(x = "Species", y = paste("`", x, "`", sep = ""))) +
      geom_violin(colour = "grey100") +
      geom_point(aes_string(x = "jit", colour = "Correct")) +
      geom_text(
        data = subset(df, !as.logical(Correct)),
        aes_string(x = "jit", y = paste("`", x, "`", sep = ""), label = "Event")
      ) +
      theme(text = element_text(size = 12), axis.text.x = element_text(face = "italic"))
  )
}

# plot distribution of training data votes
train.votes <- cbind(data.frame(spp = event.rf$y), event.rf$votes)
sort.order <- do.call(order, lapply(c(1:ncol(train.votes)), function(i) train.votes[, i]))
train.votes <- train.votes[sort.order, ]
train.votes$id <- 1:nrow(train.votes)
train.votes <- melt(train.votes, id.vars = c("id", "spp"), variable.name = "Species")
ggplot(train.votes, aes(x = id, y = value)) + geom_area(aes(fill = Species)) +
  scale_fill_brewer(palette = "Accent") +
  facet_wrap(~ spp, scales = "free_x") + scale_x_continuous("Individual") +
  scale_y_continuous("Membership Probability") +
  theme(
    text = element_text(size = 12), legend.text = element_text(face = "italic"),
    axis.text.x = element_blank(), axis.ticks.x = element_blank(),
    strip.text = element_text(face = "italic")
  ) +
  ggtitle("Training")

ggplot(correct.prob, aes(spp, prob)) +
  geom_boxplot(alpha = 0.5) +
  geom_jitter() +
  facet_grid(~ correct) +
  theme(
    text = element_text(size = 12),
    axis.text.x = element_text(face = "italic")
  )

g <- impHeatmap(event.rf, plot = FALSE) +
  theme(
    text = element_text(size = 12),
    axis.text.x = element_text(face = "italic", angle = 45, hjust = 1),
    legend.position = "none"
  )
print(g)


dev.off()


png("Figure 3 - proximity plot.png", width = 8.5, height = 11, units = "in", res = 600)
pp <- proximity.plot(event.rf, point.size = 3, circle.size = 9, plot = FALSE)
g <- pp$g +
  #scale_color_brewer(palette = "Set2") +
  theme(
    text = element_text(size = 18),
    legend.text = element_text(face = "italic")
  )
print(g)
dev.off()

png("Figure 4 - event heatmap.png", width = 8.5, height = 11, units = "in", res = 600)
g <- impHeatmap(event.rf, plot = FALSE) +
  theme(
    text = element_text(size = 14),
    axis.text.x = element_text(face = "italic", angle = 45, hjust = 1),
    legend.position = "none"
  )
print(g)
dev.off()
