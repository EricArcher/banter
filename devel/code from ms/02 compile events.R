rm(list = ls())
source("01 format ac.df.r")

# load bp data and create delta variables
folder <- dir("Event Data/bp", pattern = ".rdata", full.names = T)
bp.delta.mat <- do.call(rbind, lapply(folder, function(f) {
  cat(f, "\n")
  load(f)
  if(nrow(mat) < 2) return(NULL)
  delta.mat <- diff(mat[, c("bp.FREQBEG", "bp.FREQCENTER")])
  colnames(delta.mat) <- c("bp.delta.FREQBEG", "bp.delta.FREQCENTER")
  rownames(delta.mat) <- rownames(mat)[-1]
  return(delta.mat)
}))

folder <- "Event Data"
events <- file.path(folder, "events")
unlink(events, recursive = TRUE, force = TRUE)
dir.create(events)

# create event list from call types
ev.list <- sapply(rownames(ac.df), function(i) {
  cat(i, "\n")
  calls <- sapply(c("bp", "dw", "ec"), function(ct) {
    f <- file.path(folder, ct, paste(ct, i, "rdata", sep = "."))
    if(file.exists(f)) {
      load(f)
      if(ct == "bp") {
        mat <- mat[mat[, "bp.FREQBEG"] > 20000, , drop = FALSE]
        mat <- merge(mat, bp.delta.mat, by = "row.names", all.x = TRUE)
        rn <- mat[, 1]
        mat <- as.matrix(mat[, -1, drop = FALSE])
        rownames(mat) <- as.character(rn)
        mat <- na.omit(mat)
      }
      if(ct == "ec") {
        mat <- mat[mat[, "ec.DURATION"] <= 0.002, , drop = FALSE]
      }
      mat
    } else NULL
  }, simplify = FALSE)
  f <- file.path(events, paste("event", i, "rdata", sep = "."))
  ac.id <- i
  save(ac.id, calls, file = f)
  calls
}, simplify = FALSE)

save(ac.df, ev.list, file = "event list filtered.rdata")
