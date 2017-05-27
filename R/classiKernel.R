#' Create a kernel estimator for functional data classification
#'
#' @description Creates an efficient kernel estimator for functional data
#' classification. Currently
#' supported distance measures are all \code{metrics} implemented in \code{\link[proxy]{dist}}
#' and all semimetrics suggested in Fuchs et al. (2015).
#' Additionally, all (semi-)metrics can be used on a derivative of arbitrary
#' order of the functional observations.
#' For kernel functions all kernels implemented in \code{\link[fda.usc]{fda.usc}}
#' are available as well as custom kernel functions.
#'
#' @inheritParams classiKnn
#' @param h [numeric(1)]\cr
#'     controls the bandwidth of the kernel function. All kernel functions \code{ker} should be
#'     implemented to have bandwidth = 1. The bandwidth is controlled via \code{h}
#'     by using \code{K(x) = ker(x/h)} as the kernel function.
#' @param ker [numeric(1)]\cr
#'     character describing the kernel function to use. Available are
#'     amongst others all kernel functions from \code{\link[fda.usc]{Kernel}}.
#'     For the full list execute \code{\link{kerChoices}()}.
#'     The usage of customized kernel function is symbolized by
#'     \code{ker = "custom.ker"}. The customized function can be specified in
#'     \code{custom.ker}
#' @param custom.ker [function(u)]\cr
#'     customized kernel function. This has to be a function with exactly one parameter
#'     \code{u}, returning the numeric value of the kernel function
#'     \code{ker(u)}. This function is only used if \code{ker == "custom.ker"}.
#'     The bandwidth should be constantly equal to 1 and is controlled via \code{h}.
#'
#' @importFrom fda.usc Ker.norm Ker.cos Ker.epa Ker.tri Ker.quar Ker.unif
#' AKer.norm AKer.cos AKer.epa AKer.tri AKer.quar AKer.unif
#' @importFrom stats aggregate dnorm
#'
#'
#' @return \code{classiKernel} returns an object of class \code{'classiKernel'}.
#' This is a list containing  at least the
#' following components:
#'  \describe{
#'   \item{\code{classes}}{a factor of length nrow(fdata) coding the response of
#'   the training data set.}
#'   \item{\code{fdata}}{the raw functional data as a matrix with the individual
#'   observations as rows.}
#'   \item{\code{proc.fdata}}{the preprocessed data (missing values interpolated,
#'   derived and evenly spaced). This data is \code{this.fdataTransform(fdata)}.
#'   See \code{this.fdataTransform} for more details.}
#'   \item{\code{grid}}{numeric vector containing the grid on which \code{fdata}
#'   is observed)}
#'   \item{\code{h}}{numeric value giving the bandwidth to be used in the kernel function.}
#'   \item{\code{ker}}{character encoding the kernel function to use.}
#'   \item{\code{metric}}{character string coding the distance metric to be used
#'   in \code{\link{computeDistMat}}.}
#'   \item{\code{nderiv}}{integer giving the order of derivation that is applied
#'   to fdata before computing the distances between the observations.}
#'   \item{\code{this.fdataTransform}}{preprocessing function taking new data as
#'   a matrix. It is used to transform \code{fdata} into \code{proc.fdata} and
#'   is required to preprocess new data in order to predict it. This function
#'   ensures, that preprocessing (derivation, respacing and interpolation of
#'   missing values) is done in the exact same way for the original
#'   training data set and future (test) data sets.}
#'   \item{\code{call}}{the original function call.}
#'  }
#'
#' @references
#' Fuchs, K., J. Gertheiss, and G. Tutz (2015):
#' Nearest neighbor ensembles for functional data with interpretable feature selection.
#' Chemometrics and Intelligent Laboratory Systems 146, 186 - 197.
#'
#' @examples
#' # How to implement your own kernel function
#' data("ArrowHead")
#' classes = ArrowHead[,"target"]
#'
#' set.seed(123)
#' train_inds = sample(1:nrow(ArrowHead), size = 0.8 * nrow(ArrowHead), replace = FALSE)
#' test_inds = (1:nrow(ArrowHead))[!(1:nrow(ArrowHead)) %in% train_inds]
#'
#' ArrowHead = ArrowHead[,!colnames(ArrowHead) == "target"]
#'
#' # custom kernel
#' myTriangularKernel = function(u) {
#'   return((1 - abs(u)) * (abs(u) < 1))
#' }
#'
#' # create the model
#' mod1 = classiKernel(classes = classes[train_inds], fdata = ArrowHead[train_inds,],
#'                     ker = "custom.ker", h = 2, custom.ker = myTriangularKernel)
#'
#' # calculate the model predictions
#' pred1 = predict(mod1, newdata = ArrowHead[test_inds,], predict.type = "response")
#'
#' # prediction accuracy
#' mean(pred1 == classes[test_inds])
#'
#' # create another model using an existing kernel function
#' mod2 = classiKernel(classes = classes[train_inds], fdata = ArrowHead[train_inds,],
#'                     ker = "Ker.tri", h = 2)
#'
#' # calculate the model predictions
#' pred2 = predict(mod1, newdata = ArrowHead[test_inds,], predict.type = "response")
#'
#' # prediction accuracy
#' mean(pred2 == classes[test_inds])
#' @seealso predict.classiKernel
#' @export
classiKernel = function(classes, fdata, grid = 1:ncol(fdata), h = 1,
                        metric = "Euclidean", ker = "Ker.norm",
                        nderiv = 0L, derived = FALSE,
                        deriv.method = "base.diff",
                        custom.metric = function(x, y, ...) {
                          return(sqrt(sum((x - y) ^ 2)))
                        },
                        custom.ker = function(u) {
                          return(dnorm(u))
                        },
                        ...) {
  # check inputs
  if (class(fdata) == "data.frame")
    fdata = as.matrix(fdata)
  assert_numeric(fdata)
  assertClass(fdata, "matrix")

  if (is.numeric(classes))
    classes = factor(classes)
  assertFactor(classes, any.missing = FALSE, len = nrow(fdata))
  assertNumeric(grid, any.missing = FALSE, len = ncol(fdata))
  assertNumeric(h, lower = 0, len = 1L)
  assertIntegerish(nderiv, lower = 0L)
  assertFlag(derived)
  assertChoice(deriv.method, c("base.diff", "fda.deriv.fd"))
  assertChoice(ker, choices = kerChoices())
  assertChoice(metric, choices = metricChoices())


  # check if data is evenly spaced  -> respace
  evenly.spaced = all.equal(grid, seq(grid[1], grid[length(grid)],
                                      length.out = length(grid)))
  no.missing = !checkmate::anyMissing(fdata)

  # TODO write better warning message
  if (!no.missing) {
    warning("There are missing values in fdata. They will be filled using a spline representation!")
  }

  # create a model specific preprocessing function for the data
  # here the data will be derived, respaced equally and missing values will be filled
  this.fdataTransform = fdataTransform(fdata = fdata, grid = grid,
                                       nderiv = nderiv, derived = derived,
                                       evenly.spaced = evenly.spaced,
                                       no.missing = no.missing,
                                       deriv.method = deriv.method, ...)
  proc.fdata = this.fdataTransform(fdata)

  # delete the custom.metric function from output if not needed
  if (metric != "custom.metric")
    custom.metric = character(0)
  if (ker != "custom.ker")
    custom.ker = character(0)

  ret = list(classes = classes,
             fdata = fdata,
             proc.fdata = proc.fdata,
             grid = grid,
             h = h,
             metric = metric,
             ker = ker,
             custom.metric = custom.metric,
             custom.ker = custom.ker,
             nderiv = nderiv,
             this.fdataTransform = this.fdataTransform,
             call = as.list(match.call(expand.dots = FALSE)))
  class(ret) = "classiKernel"

  return(ret)
}


#' predict a classiKernel object
#'
#' predict function for a classiKnn object.
#'
#' @param object [\code{classiKernel}]\cr
#'   object of class classiKernel to get predictions from
#' @param newdata [\code{data.frame}]\cr
#'   (optional) new data to predict from with observations as rows. Do not derive this data,
#'   this will be done automatically if required by the model. If \code{NULL},
#'   the training data is predicted, currently without using a leave-one-out prediction.
#' @param predict.type [\code{character(1)}]\cr
#'   one of 'response' or 'prob', indicating the type of prediction. Choose
#'   'response' to return a vector of length \code{nrow(newdata)} containing the
#'   most predicted class.
#'   Choose 'prob' to return a matrix with \code{nrow(newdata)} rows containing
#'   the probabilities for the classes as columns.
#' @param ... [\code{list}]\cr
#'   additional arguments to \link{computeDistMat}.
#' @seealso classiKernel
#' @export
predict.classiKernel = function(object, newdata = NULL, predict.type = "response", ...) {
  # input checking
  if (!is.null(newdata)) {
    if (class(newdata) == "data.frame")
      newdata = as.matrix(newdata)
    assertClass(newdata, "matrix")
    newdata = object$this.fdataTransform(newdata)
  }
  assertChoice(predict.type, c("response", "prob"))

  # create distance metric
  # note, that additional arguments from the original model are handed over
  # to computeDistMat using object$call$...
  dist.mat = do.call("computeDistMat", c(list(x = object$proc.fdata, y = newdata,
                                              method = object$metric,
                                              custom.metric = object$custom.metric, ...),
                                         object$call$...))
  # apply kernel function
  if (object$ker == "custom.ker") {
    this.ker = object$custom.ker
  } else {
    this.ker = object$ker
  }

  # Apply distance function after dividing by bandwidth
  dist.kernel = apply(dist.mat / object$h, c(1, 2), this.ker)

  raw.result = aggregate(dist.kernel, by = list(classes = object$classes), sum, drop = FALSE)

  if (predict.type == "response") {
    # return class with highest probability
    if (ncol(dist.mat) == 1L) {
      # exactly one observation in newdata
      result = raw.result$classes[which.max(raw.result[, -1])]
    } else {
      result = raw.result$classes[apply(raw.result[, -1], 2, which.max)]
    }
  } else if (predict.type == "prob") {
    # probabilities for the classes
    if (ncol(dist.mat) == 1L) { # exactly one observation in newdata
      result = raw.result[, -1] / sum(raw.result[, -1])
      names(result) = raw.result$classes
    } else {
      result = t(apply(raw.result[, -1], 2, function(x) {
        if (sum(x) > 0) x / sum(x)
        else rep(1 / length(x), length(x)) # if all ker(dist(x,y)) == 0
      }))
      colnames(result) = raw.result$classes
    }
  }
  return(result)
}

#' @export
print.classiKernel = function(x, ...) {
  cat("\n")
  cat("\t classiKernel object \n")
  cat("\n")
  cat("data: \n")
  cat("", length(levels(x$classes)), "classes:", levels(x$classes), "\n")
  cat("", nrow(x$fdata), "observations of length", ncol(x$fdata), "\n")
  cat("algorithm: \n")
  cat(" kernel =", x$ker, "\n")
  cat(" k = ", x$knn, "\n")
  cat(" nderiv =", x$nderiv, "\n")
  cat("\n")
}


#' @export
summary.classiKernel = function(object, ...) {
  print(object, ...)
}
