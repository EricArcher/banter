library(randomForest)

test <- sample_n(iris, 5)
train <- iris[!rownames(iris) %in% rownames(test), ]


rf.list <- mclapply(1:3, function(i) {
  randomForest(Species ~ ., train, ntree = 1000, norm.votes = FALSE)
}, mc.cores = 3)
rf.c <- do.call(combine, rf.list)

rf <- randomForest(Species ~ ., train, ntree = 3000, norm.votes = FALSE)

pred.c <- cbind(Species = test$Species, predicted = predict(rf.c, test))
pred <- cbind(Species = test$Species, predicted = predict(rf, test))

pred.c
pred
