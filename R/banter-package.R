#' banter
#' 
#' @docType package
#' @name banter-package
#' @aliases banter
#' @title BioAcoustic EveNT ClassifiER
#' @keywords package
NULL

setOldClass(c("randomForest", "randomForest.formula", "rfPermute", "ranger"))
setClassUnion("classifier", c("randomForest", "randomForest.formula", "rfPermute", "ranger", "NULL"))
setClassUnion("listOrNull", c("list", "NULL"))
