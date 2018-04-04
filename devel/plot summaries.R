library(tidyverse)
library(gridExtra)

plotRFtrace <- function(rf, plot = TRUE) {
  df <- rf$err.rate %>% 
    as.data.frame()
  lvls <- colnames(df)
  p <- df %>% 
    mutate(trees = 1:nrow(.)) %>% 
    gather(class, error, -trees) %>% 
    mutate(class = factor(class, levels = lvls)) %>% 
    ggplot(aes(trees, error, color = class)) +
    geom_line() +
    scale_color_discrete("")
  
  if(plot) print(p)
  invisible(p)
}

rfConfMat <- function(rf) {
  x <- table(rf$y, rf$predicted)
  cbind(x, class.error = 1 - (diag(x) / rowSums(x)))
}

plotConfMat <- function(rf, title = NULL, plot = TRUE) {
  library(tidyverse)
  library(viridis)
  conf.mat <- rfConfMat(rf)
  conf <- conf.mat[, -ncol(conf.mat)] 
  pct.correct <- (100 * sum(diag(conf)) / sum(conf)) %>% 
    round(0) %>% 
    paste0("% correct")
  title <- if(is.null(title)) pct.correct else paste0(title, " (", pct.correct, ")")
  freq <- rowSums(conf)
  rownames(conf) <- paste0(names(freq), " (", freq, ")")
  
  conf <- conf %>% 
    prop.table(1) %>% 
    as.data.frame %>% 
    rownames_to_column("observed") %>% 
    gather(predicted, prop, -observed) %>% 
    mutate(
      #prop = ifelse(prop == 0, NA, prop),
      observed = factor(observed),
      observed = reorder(observed, desc(observed)),
      predicted = factor(predicted)
    )
  
  p <- ggplot(conf, aes(predicted, observed)) +
    geom_raster(aes(fill = prop)) +
    scale_fill_viridis(option = "magma", direction = -1, limits = c(0, 1)) +
    scale_x_discrete(position = "top") +
    labs(x = "Predicted", y = "True", title = title) +
    guides(fill = guide_colorbar(title = "Proportion")) +
    theme(
      axis.text.x.top = element_text(angle = 45, hjust = 0),
      panel.background = element_blank()
    )
  
  if(plot) print(p)
  invisible(p)
}

rfSummary <- function(rf) {
  rf.trace <- plotRFtrace(rf, plot = FALSE)
  rf.votes <- plotVotes(rf, plot = FALSE)
  rf.conf <- plotConfMat(rf, plot = FALSE)
  rf.varimp <- impHeatmap(rf, n = 20, plot = FALSE)
  grid.arrange(rf.trace, rf.votes, rf.conf, rf.varimp, ncol = 2)
}

rfSummary(rf)
