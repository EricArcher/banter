#' @name accessors
#' @title Accessors
#' @description Class accessors
#' 
#' @param x detector_model object
#' 
#' @export
#' 
name <- function(x) x@name

#' @rdname accessors
#' @export
#' 
ids <- function(x) x@ids

#' @rdname accessors
#' @export
#' 
model <- function(x) x@model
