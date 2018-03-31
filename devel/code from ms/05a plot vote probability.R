library(reshape2)
library(ggplot2)
load("unk prediction.rdata")

# sort data.frames
plot.df <- votes.df
plot.df$pred.spp <- NULL
sort.order <- do.call(order, lapply(c(1:ncol(plot.df)), function(i) plot.df[, i]))
plot.df <- plot.df[sort.order, ]
plot.df$id <- 1:nrow(plot.df)

# identify which codes only have 1 event
spp.freq <- table(plot.df$orig.spp)
singles <- names(spp.freq)[spp.freq == 1]

pdf("unknown vote probability distributions.pdf", height = 8.5, width = 11)

# plot all multiple event code types
to.plot <- subset(plot.df, !orig.spp %in% singles)
to.plot <- melt(to.plot, id.vars = c("id", "orig.spp"), variable.name = "Species")
ggplot(to.plot, aes(x = id, y = value)) + geom_area(aes(fill = Species)) +
  scale_fill_brewer(palette = "Accent") +
  facet_wrap(~ orig.spp, scales = "free_x") + scale_x_continuous("Individual") +
  scale_y_continuous("Membership Probability") +
  ggtitle("Unknown - Multiple Events") +
  theme(
    text = element_text(size = 16), legend.text = element_text(face = "italic"),
    axis.text.x = element_blank(), axis.ticks.x = element_blank()
  )

# plot single event code types (coordinates are flipped for readability of speciess codes)
singles.df <- subset(plot.df, orig.spp %in% singles)
singles.df <- melt(singles.df, id.vars = c("id", "orig.spp"), variable.name = "Species")
ggplot(singles.df, aes(x = orig.spp, y = value)) +
  geom_bar(aes(fill = Species), stat = "identity") + coord_flip() +
  scale_fill_brewer(palette = "Accent") + scale_x_discrete("Species Code") +
  scale_y_continuous("Membership Probability") +
  ggtitle("Unknown - Single Event") +
  theme(
    text = element_text(size = 16), legend.text = element_text(face = "italic"),
    axis.text.x = element_blank(), axis.ticks.x = element_blank()
  )

dev.off()

