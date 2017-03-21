#' @title The classiFunc package
#'
#' @description This package implements a knn estimator in \code{\link{classiKnn}}
#' for functional data using efficiently implemented semimetrics.
#' Currently supported distance measures are all \code{metrics} implemented in
#' \code{\link[proxy]{dist}}
#' and all semimetrics suggested in
#' Fuchs etal. 2015, Nearest neighbor ensembles for functional data with
#' interpretable feature selection,
#' (\url{http://www.sciencedirect.com/science/article/pii/S0169743915001100})
#' Additionally, all (semi-)metrics can be used on an arbitrary order of derivation.
#' @import BBmisc checkmate proxy
#' @rdname classiFunc
#' @name classiFunc
NULL

# library("BBmisc")
# library("checkmate")
# library("fda")
# library("proxy")
# library("testthat")
