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
#'     indizes used to define subspaces for
#'     \code{method \%in\% c("shortEuclidean", "relAreas")}.
#' @param t1,t2 [\code{integer(1)}]\cr
#'     indizes of the points for which to compare the jump heights in
#'     \code{method = "jump"}.
#' @param .poi [\code{integer(1 to ncol(x))}]\cr
#'     integer vector giving the indizes of the points of interest for
#'     \code{method = "points"}.
#' @param custom.metric [\code{function(x, y, ...)}]\cr
#'     a function specifying how to compute the distance between
#'     two functional observations (= numeric vectors of the same length)
#'     \code{x} and \code{y}. It can handle additional arguments in \code{...}.
#'     The default is the Euclidean distance (equals Minkwoski distance
#'     with \code{lp = 2}). Used if \code{method = "custom.metric"}.
#' @param a,b,c [\code{numeric(1)}]\cr
#'     the penalization parameters used in the elastic distance of the
#'     sqare root velocity of the curves.
#'     \code{a} and \code{b} specify the weight of the integrals measuring
#'     the amount of bending and stretching. The stretching (movement in x) is
#'     penalized by \code{a^2}, the bending (movement in y) is penalized by \code{b^2}.
#'     Alternatively, the argument \code{c} can be specified, where
#'     \code{c = b / (2a)}, as the resulting metric is only influenced by
#'     the ratio of \code{a} and \code{b}.
#'     Default values are \code{a = 1/2, b = 1}.
#'     Used for \code{method \%in\% c('elastic', 'SRV')}
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
                          dmin = 1L, dmax = ncol(x),
                          dmin1 = 1L, dmax1 = ncol(x),
                          dmin2 = 1L, dmax2 = ncol(x),
                          t1 = 1L, t2 = ncol(x),
                          .poi = 1:ncol(x),
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
    assertIntegerish(.poi, lower = 1L, upper = ncol(x), any.missing = FALSE,
                     min.len = 1L, max.len = ncol(x), unique = TRUE)
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

  # elastic distance from the square root velocity framework,
  # see Srivastava etal 2011
  if(method %in% c("elastic", "SRV")) {
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

