# global function naming all implemented (semi-)metrics
#' Name all metrics
#'
#' \code{metric.choices} is a function returning all (semi-)metrics that are
#' currently implemented in the \code{link{classiFunc}}-package.
#'
#' @export
metric.choices = function() {
  proxy.list = proxy::pr_DB$get_entries()
  is.metric = unlist(BBmisc::extractSubList(proxy.list, element = "type")) == "metric"
  proxy.metric.names = unlist(BBmisc::extractSubList(proxy.list[is.metric], element = "names"))
  return(c(proxy.metric.names,
  "shortEuclidean", "mean", "relAreas",
  "jump", "globMax", "globMin",
  "points", "custom.metric"))
}

#' @title Compute a distance matrix for functional observations
#'
#' @description This mainly internal function offers a unified framework to acces the
#' \code{\link[proxy]{dist}} function from the \code{proxy} package and additional
#' (semi-)metrics. For implemented methods see \code{\link{classiKnn}}.
#'
#' @param x matrix containing observations as rows
#' @param y see \code{x}. The default \code{NULL} uses \code{y = x}.
#' @param method character string specifying the (semi-)metric to be used.
#'     on the domain of \code{x}, used in different (semi-)metrics.
#' @param dmin,dmax,dmin1,dmax1,dmin2,dmax2 indizes used to define subspaces for
#'     \code{method \%in\% c("shortEuclidean", "relAreas")}
#' @param t1,t2 indizes of the points for which to compare the jump heights in
#'     \code{method = "jump"}.
#' @param .poi integer vector giving the indizes of the points of interest for
#'     \code{method = "points"}.
#' @param ... additional parameters to the (semi-)metrics.
#'
#' @export
computeDistMat = function(x, y = NULL,
                          method = "Euclidean",
                          dmin = 1L, dmax = ncol(x),
                          dmin1 = 1L, dmax1 = ncol(x),
                          dmin2 = 1L, dmax2 = ncol(x),
                          t1 = 1L, t2 = ncol(x),
                          .poi = 1:ncol(x),
                          custom.metric = function(x, y, ...) {
                            return(sqrt(sum((x - y)^2)))},
                          ...) {
  # print(method)
  requirePackages("proxy")

  assertChoice(method, choices = metric.choices())

  if(method %in% proxy.set) {
    return(as.matrix(proxy::dist(x, y, method = method, ...)))
  }


  if(method == "custom.metric") {
    return(as.matrix(proxy::dist(x, y, method = custom.metric, ...)))
  }

  # new semimetrics from fuchs etal 2015
  # they need two matrices as x and y
  if(is.null(y)) y = x

  if(method == "shortEuclidean") {
    assertIntegerish(dmin, lower = 1L, upper = ncol(x), len = 1L, any.missing = FALSE)
    assertIntegerish(dmax, lower = 1L, upper = ncol(x), len = 1L, any.missing = FALSE)
    stopifnot(dmin <= dmax)
    return(computeDistMat(x[,dmin:dmax], y[,dmin:dmax], "Euclidean"))
  }

  if(method == "mean") {
    return(abs(outer(rowMeans(x), rowMeans(y), "-")))
  }

  if(method == "relAreas") {
    assertIntegerish(dmin1, lower = 1L, upper = ncol(x), len = 1L, any.missing = FALSE)
    assertIntegerish(dmax1, lower = 1L, upper = ncol(x), len = 1L, any.missing = FALSE)
    assertIntegerish(dmin2, lower = 1L, upper = ncol(x), len = 1L, any.missing = FALSE)
    assertIntegerish(dmin2, lower = 1L, upper = ncol(x), len = 1L, any.missing = FALSE)
    stopifnot(dmin1 <= dmax1)
    stopifnot(dmin2 <= dmax2)

    relArea.x = abs(rowMeans(x[,dmin1:dmax1]) /  rowMeans(x[,dmin2:dmax2]))
    relArea.y = abs(rowMeans(y[,dmin1:dmax1]) /  rowMeans(y[,dmin2:dmax2]))
    return(abs(outer(relArea.x, relArea.y, "-")))
  }

  if(method == "jump") {
    assertIntegerish(t1, lower = 1L, upper = ncol(x), len = 1L, any.missing = FALSE)
    assertIntegerish(t2, lower = 1L, upper = ncol(x), len = 1L, any.missing = FALSE)

    jump.x = x[,t2] - x[,t1]
    jump.y = y[,t2] - y[,t1]

    return(abs(outer(jump.x, jump.y, "-")))

  }

  if(method == "globMax") {
    max.x = apply(x, 1,  max)
    max.y = apply(y, 1,  max)
    return(abs(outer(max.x, max.y, "-")))
  }

  if(method == "globMin") {
    min.x = apply(x, 1,  min)
    min.y = apply(y, 1,  min)
    return(abs(outer(min.x, min.y, "-")))
  }

  if(method == "points") {
    assertIntegerish(poi, lower = 1L, upper = ncol(x), any.missing = FALSE,
                     min.len = 1L, max.len = ncol(x), unique = TRUE)
    return(computeDistMat(x[,.poi], y[,.poi], method = "Manhattan"))
  }

}
