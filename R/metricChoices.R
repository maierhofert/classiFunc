# global function naming all implemented (semi-)metrics
#' List the names of all metrics
#'
#' \code{metricChoices} is a function returning the names of  all
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
metricChoices = function(proxy.only = FALSE) {
  proxy.list = proxy::pr_DB$get_entries()
  is_metric = unlist(BBmisc::extractSubList(proxy.list, element = "type")) == "metric"
  proxy_metric_names = unlist(BBmisc::extractSubList(proxy.list[is_metric],
    element = "names"))
  if (proxy.only) {
    return(proxy_metric_names)
  } else {
    additional_metric_names = c("shortEuclidean", "mean", "relAreas",
      "jump", "globMax", "globMin", "points", "custom.metric",
      "amplitudeDistance", "phaseDistance", "FisherRao", "elasticMetric",
      "elasticDistance", "dtwPath", "rucrdtw", "rucred")
    names(additional_metric_names) = additional_metric_names
    choices = c(proxy_metric_names, additional_metric_names)
    # Make sure we have unique names
    return(choices[unique(names(choices))])
  }
}
