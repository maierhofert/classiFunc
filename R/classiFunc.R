#' @title The classiFunc package
#'
#' @description This package implements methods for functional data
#' classification. The main functions of this package are
#' \code{\link{classiKnn}}, a k nearest neighbor estimator for functional data,
#' and \code{\link{classiKernel}}, a kernel estimator
#' for functional data. The package uses efficiently implemented semimetrics to
#' create the distance matrix of the functional observations in the function
#' \code{\link{computeDistMat}}.
#' Currently supported distance measures are all \code{methods} implemented in
#' \code{\link[proxy]{dist}}
#' and all semimetrics suggested in Fuchs et al. (2015).
#' Additionally, all (semi-)metrics can be used on a derivative of arbitrary
#' order of the functional observations.
#' This is a new package, please report all bugs and issues at
#' \url{https://github.com/maierhofert/classiFunc}.
#'
#' @import BBmisc checkmate proxy
#' @importFrom fda Data2fd deriv.fd eval.fd
#' @importFrom fdasrvf elastic.distance f_to_srvf
#' @import dtw
#'
#' @references
#' Fuchs, K., J. Gertheiss, and G. Tutz (2015):
#' Nearest neighbor ensembles for functional data with interpretable feature selection.
#' Chemometrics and Intelligent Laboratory Systems 146, 186 - 197.
#'
#' @author
#' Thomas Maierhofer
#'
#' @rdname classiFunc
#' @name classiFunc
NULL
