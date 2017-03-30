# global function naming all implemented (semi-)metrics
#' Name all metrics
#'
#' \code{metric.choices} is a function returning the names of  all
#' (semi-)metrics that are currently implemented in the
#' \code{link{classiFunc}}-package and can be used for the argument
#' \code{method} in \code{\link{computeDistMat}} or the argument
#' \code{metric} in \code{\link{classiKnn}} and \code{\link{classiKernel}}
#' respectively.
#'
#' @param proxy.only [logical(1)]\cr
#'     should only the metrics of the proxy package be returned? Defaults to
#'     \code{FALSE}, which results in returning the names of all allowed metrics
#'     for \code{\link{computeDistMat}}.
#'
#' @export
metric.choices = function(proxy.only = FALSE) {
  proxy.list = proxy::pr_DB$get_entries()
  is.metric = unlist(BBmisc::extractSubList(proxy.list, element = "type")) == "metric"
  proxy.metric.names = unlist(BBmisc::extractSubList(proxy.list[is.metric], element = "names"))
  if (proxy.only) {
    return(proxy.metric.names)
  } else {
    return(c(proxy.metric.names,
             "shortEuclidean", "mean", "relAreas",
             "jump", "globMax", "globMin",
             "points", "custom.metric",
             "SRV", "elastic"))
  }
}
