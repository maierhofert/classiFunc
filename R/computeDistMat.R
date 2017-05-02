#' @title Compute a distance matrix for functional observations
#'
#' @description This mainly internal function offers a unified framework to acces the
#' \code{\link[proxy]{dist}} function from the \code{proxy} package and additional
#' (semi-)metrics. For implemented methods see \code{\link{classiKnn}}.
#'
#' @param x [\code{matrix}]\cr
#'     matrix containing observations as rows
#' @param y [\code{matrix}]\cr
#'     see \code{x}. The default \code{NULL} uses \code{y = x}.
#' @param method [\code{character(1)}]\cr
#'     character string specifying the (semi-)metric to be used.
#'     on the domain of \code{x}.
#' @param dmin,dmax,dmin1,dmax1,dmin2,dmax2 [\code{integer(1)}]\cr
#'     encode the indizes used to define subspaces for
#'     \code{method \%in\% c("shortEuclidean", "relAreas")}
#'     as numeric values between 0 and 1 by
#'     \code{index = round(quantile(1:ncol(x), probs = dmin))}.
#' @param t1,t2 [\code{numeric(1)}]\cr
#'     encode the index of the points for which to compare the jump heights in
#'     \code{method = "jump"} as numeric values between 0 and 1 by
#'     \code{index = round(quantile(1:ncol(x), probs = t1))}.
#' @param .poi [\code{numeric(1 to ncol(x))}]\cr
#'     numeric vector of length between 1 and \code{ncol(x)} taking numeric values
#'     between 0 and 1, denoting the
#'     index of the points of interest by
#'     \code{index = round(quantile(1:ncol(x), probs = .poi))}
#'     for \code{method = "points"}.
#' @param custom.metric [\code{function(x, y, ...)}]\cr
#'     a function specifying how to compute the distance between
#'     two functional observations (= numeric vectors of the same length)
#'     \code{x} and \code{y}. It can handle additional arguments in \code{...}.
#'     The default is the Euclidean distance (equals Minkwoski distance
#'     with \code{lp = 2}). Used if \code{method = "custom.metric"}.
#' @param a,b,c [\code{numeric(1)}]\cr
#'     weigths of the amplitude distance (\code{a}) and the phase distance (\code{b})
#'     in a semimetric that combines them by addition.
#'     Used for \code{method == 'elasticDistance'}.
#' @param lambda [\code{numeric(1)}]\cr
#'     penalization parameter for the warping allowed before calculating the
#'     elastic distance.
#'     Default value is 0. Large values imply less (no) warping, small values
#'     imply more warping.
#'     Used for \code{method \%in\% c('elastic', 'SRV')}.
#'
#' @param ... additional parameters to the (semi-)metrics.
#'
#' @export
computeDistMat = function(x, y = NULL,
                          method = "Euclidean",
                          dmin = 0, dmax = 1,
                          dmin1 = 0, dmax1 = 1,
                          dmin2 = 0, dmax2 = 1,
                          t1 = 0, t2 = 1,
                          .poi = seq(0, 1, length.out = ncol(x)),
                          custom.metric = function(x, y, lp = 2, ...) {
                            return(sum(abs(x - y) ^ lp) ^ (1 / lp))},
                          a = NULL, b = NULL, c = NULL, lambda = 0,
                          ...) {

  assertChoice(method, choices = metric.choices())

  if(method %in% metric.choices(proxy.only = TRUE)) {
    return(as.matrix(proxy::dist(x, y, method = method, ...)))
  }


  if(method == "custom.metric") {
    return(as.matrix(proxy::dist(x, y, method = custom.metric, ...)))
  }

  # new semimetrics from fuchs etal 2015
  # they need two matrices as x and y
  if(is.null(y)) y = x

  if(method == "shortEuclidean") {
    assertNumeric(dmin, lower = 0, upper = 1, len = 1L, any.missing = FALSE)
    assertNumeric(dmax, lower = 0, upper = 1, len = 1L, any.missing = FALSE)
    stopifnot(dmin <= dmax)

    # convert to index
    dmin = round(quantile(1:ncol(x), probs = dmin))
    dmax = round(quantile(1:ncol(x), probs = dmax))
    return(computeDistMat(x[,dmin:dmax],
                          y[,dmin:dmax], "Euclidean"))
  }

  if(method == "mean") {
    return(abs(outer(rowMeans(x), rowMeans(y), "-")))
  }

  if(method == "relAreas") {
    assertNumeric(dmin1, lower = 0, upper = 1, len = 1L, any.missing = FALSE)
    assertNumeric(dmax1, lower = 0, upper = 1, len = 1L, any.missing = FALSE)
    assertNumeric(dmin2, lower = 0, upper = 1, len = 1L, any.missing = FALSE)
    assertNumeric(dmax2, lower = 0, upper = 1, len = 1L, any.missing = FALSE)
    stopifnot(dmin1 <= dmax1)
    stopifnot(dmin2 <= dmax2)

    # convert to index
    dmin1 = round(quantile(1:ncol(x), probs = dmin1))
    dmax1 = round(quantile(1:ncol(x), probs = dmax1))
    dmin2 = round(quantile(1:ncol(x), probs = dmin2))
    dmax2 = round(quantile(1:ncol(x), probs = dmax2))

    relArea.x = abs(rowMeans(x[,dmin1:dmax1]) /  rowMeans(x[,dmin2:dmax2]))
    relArea.y = abs(rowMeans(y[,dmin1:dmax1]) /  rowMeans(y[,dmin2:dmax2]))
    return(abs(outer(relArea.x, relArea.y, "-")))
  }

  if(method == "jump") {
    assertNumeric(t1, lower = 0, upper = 1, len = 1L, any.missing = FALSE)
    assertNumeric(t2, lower = 0, upper = 1, len = 1L, any.missing = FALSE)

    # convert to index
    t1 = round(quantile(1:ncol(x), probs = t1))
    t2 = round(quantile(1:ncol(x), probs = t2))

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
    assertNumeric(.poi, lower = 0, upper = 1, any.missing = FALSE,
                     min.len = 1L, max.len = ncol(x), unique = TRUE)
    # convert to index
    .poi = round(quantile(1:ncol(x), probs = .poi))

    return(computeDistMat(x[,.poi], y[,.poi], method = "Manhattan"))
  }

  # New semimetric from TM
  # TODO check if this is correct
  if(method == "dtwPath") {
    # define the difference of dtw paths
    dtwPath = function(x, y, ...) {
      dist = dtw::dtw(x, y, ...)
      mean(abs(dist$index1 - dist$index2))
    }
    return(as.matrix(proxy::dist(x, y, method = dtwPath, ...)))
  }

  # Metrics from the SRV framework
  # amplitude distance
  if(method %in% c("amplitudeDistance")) {
    return(computeDistMat(x, y, method = "elasticDistance",
                          a = 1, b = 0, lambda = lambda))
  }
  # phase distance
  if(method %in% c("phaseDistance")) {
    return(computeDistMat(x, y, method = "elasticDistance",
                          a = 0, b = 1, lambda = lambda))
  }
  # TODO check if this is mathematically correct
  # Fisher-Rao metric
  if(method %in% c("FisherRao", "elasticMetric")) {
    q1 = fdasrvf::f_to_srvf(t(x), time = 1:nrow(x))
    q2 = fdasrvf::f_to_srvf(t(y), time = 1:nrow(x))
    return(computeDistMat(t(q1), t(q2), method = "Euclidean"))
  }
  # elastic distance from the square root velocity framework,
  # see Srivastava etal 2011
  # I do not know how this is related to the elastic metric,


  # this method returns the phase and the amplitude distance
  # I do not know how it relates to the elastic metric
  if(method %in% c("elasticDistance")) {
    # input checking
    assertNumeric(a, lower = 0, len = 1L, null.ok = TRUE)
    assertNumeric(b, lower = 0, len = 1L, null.ok = TRUE)
    assertNumeric(c, lower = 0, len = 1L, null.ok = TRUE)
    assertNumeric(lambda, len = 1L)
    if(lambda < 0)
      warning(paste("Please do not specify lambda as < 0",
                    "It think it does not make sense.",
                    "The execution continues with your specified value."))

    # handling default values
    # TODO: As I do not know how this relates to the elastic metric,
    # I do not know if the default values make sense
    # probably not
    if(is.null(a) & is.null(b) & is.null(c)) {
      c = 1
      a = 0.5
      b = 1
    } else if (!is.null(a) & !is.null(b) & is.null(c)) {
      # pass on
      c = b / (2 * a)
    } else if (is.null(a) & is.null(b) & !is.null(c)) {
      b = c
      a = (1 / 2) * c
    } else if (!is.null(a) & !is.null(b) & !is.null(c)) {
      assert(all.equal(c, b / (2 * a)))
    } else {
      stop("Please specify the parameters 'a and b' or just 'c'.")
    }
    # this should hold now
    assert(all.equal(c, b / (2 * a)))

    # elastic distance function
    el.dist = function(x, y, a, b, lambda) {
      el.d = fdasrvf::elastic.distance(x, y,
                                       time = 1:length(x),
                                       lambda = lambda)
      weighted.el.d = a^2 * el.d$Dy + b^2 * el.d$Dx
      return(weighted.el.d)
    }
    return(as.matrix(proxy::dist(x, y, method = el.dist, a = a, b = b, lambda = lambda)))
  }

}

