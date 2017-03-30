#' @title The classiFunc package
#'
#' @description This package implements methods for functional data
#' classification. The main functions of this package are
#' a k nearest neighbor estimator \code{\link{classiKnn}}
#' and a kernel estimator \code{\link{classiKernel}}
#' for functional data using efficiently implemented semimetrics.
#' Currently supported distance measures are all \code{metrics} implemented in
#' \code{\link[proxy]{dist}}
#' and all semimetrics suggested in
#' Fuchs etal. 2015, Nearest neighbor ensembles for functional data with
#' interpretable feature selection,
#' (\url{http://www.sciencedirect.com/science/article/pii/S0169743915001100})
#' Additionally, all (semi-)metrics can be used on an arbitrary order of derivation.
#'
#' @import BBmisc checkmate proxy
#' @importFrom fda Data2fd deriv.fd eval.fd
#' @importFrom fdasrvf elastic.distance
#' @import dtw
#'
#' @rdname classiFunc
#' @name classiFunc
NULL
