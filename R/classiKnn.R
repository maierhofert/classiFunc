
#' @title Create a knn estimator for functional data classification.
#'
#' @description Creates an efficient knn estimator for functional data
#' classification. Currently supported distance measures are all \code{metrics}
#' implemented in \code{\link[proxy]{dist}}
#' and all semimetrics suggested in
#' Fuchs etal. 2015, Nearest neighbor ensembles for functional data with
#' interpretable feature selection,
#' (\url{http://www.sciencedirect.com/science/article/pii/S0169743915001100})
#' Additionally, all (semi-)metrics can be used on an arbitrary order of derivation.
#'
#' @param classes [\code{factor(nrow(fdata))}]\cr
#'   factor of length \code{nrow(fdata)} containing the classes of the observations.
#' @param fdata [\code{matrix}]\cr
#'   matrix containing the functional observations as rows.
#' @param grid [\code{numeric(ncol(fdata))}]\cr
#'   numeric vector containing the grid on which the functional observations were
#'   evaluated.
#' @param knn [\code{integer(1)}]\cr
#' number of nearest neighbors to use in knn algorithm.
#' @param metric [\code{character(1)}]\cr
#' character string describing the distance function to be used.
#'  \describe{
#'   \item{\code{Euclidean}}{equals \code{Lp} with \code{p = 2}. This is the default.}
#'   \item{\code{Lp, Minkowski}}{the distance for an Lp-space.}
#'   \item{\code{Manhattan}}{equals \code{Lp} with \code{p = 1}.}
#'   \item{\code{supremum, max, maximum}}{equals \code{Lp} with \code{p = Inf}.
#'   The supremal pointwise difference between the curves.}
#'   \item{\code{...}}{all other available measures for \code{\link[proxy]{dist}}.}
#'   \item{\code{shortEuclidean}}{Euclidean distance on a limited part of the domain.
#'   Additional arguments \code{dmin} and \code{dmax} can be specified, giving
#'   the index of the first and the last point to use of an evenly spaced
#'   sequence from \code{grid[1]} to \code{grid[length(grid)]}.
#'   The default values are \code{dmin = 1L} and \code{dmax = length(grid)},
#'   which results in the Euclidean distance on the entire domain.}
#'   \item{\code{mean}}{the absolute similarity of the overall mean values of
#'   the observations.}
#'   \item{\code{relAreas}}{the difference of the relation of two areas on parts
#'   of the domain given by \code{dmin1} to \code{dmax1} and \code{dmin2} to
#'   \code{dmax2}. They are definded analougously to \code{dmin} and \code{dmax}
#'   and take the same default values.}
#'   \item{\code{jump}}{the similarity of jump heights at points \code{t1} and \code{t2}.
#'   The points \code{t1} and \code{t2} are the indices  in an evenly spaced sequence
#'   from \code{grid[1]} to \code{grid[length(grid)]} of which to compare the
#'   jump height. The default values are \code{t1 = 1} and \code{t2 = length(grid)}.}
#'   \item{\code{globMax}}{the difference of the curves global maxima.}
#'   \item{\code{globMin}}{the difference of the curves global minima}
#'   \item{\code{points}}{the mean absolute differences at certain observation
#'   points \code{poi}, also  called "points of impact". These are specified as
#'   a vector of indices of an evenly spaced sequence from \code{grid[1]}
#'   to \code{grid[length(grid)]}.
#'   The default value is \code{1:length(grid)}, which results in the Manhattan
#'   distance.}
#'   \item{\code{custom.metric}}{your own semimetric will be used. Specify your
#'   own distance function in the argument \code{custom.metric}}
#'   \item{\code{elastic, SRV}}{the elastic distance of the square root velocity
#'   of the curves as described in
#'   Srivastava etal 2011, 'Shape analysis of elastic curves in Euclidean spaces'
#'   and implemented in \code{\link[fdasrvf]{elastic.distance}}.
#'   Additional argument are the numeric the penalization parameters \code{a,b,c}
#'   for the amount of bending (\code{a^2}) and stretching (\code{b^2}).
#'   The default values are \code{a = 1/2, b = 1}
#'   Alternatively \code{c} denotes the ratio of \code{2*a} and \code{b}.
#'   \code{lambda} is the additional penalization parameter for the warping
#'   allowed before calculating the elastic distance. The default is 0.
#'  }}
#' @param nderiv [\code{integer(1)}]\cr
#'   The order of derivation on which the metric shall be computed.
#'   The default is 0L.
#' @param derived [\code{logical(1)}]\cr
#' Is the data given in \code{fdata} already derived? Defaults to \code{FALSE},
#' which will lead to numerical derivation if \code{nderiv >= 1L} by applying
#' \code{\link[fda]{deriv.fd}} on a \code{\link[fda]{Data2fd}} representation of
#' \code{fdata}.
#' @param deriv.method [\code{character(1)}]\cr
#' character indicate which method should be used for derivation. Currently
#' implemented are \code{"base.diff"}, the default, and \code{"fda.deriv.fd"}.
#' \code{"base.diff"} uses the method \code{base::\link[base]{diff}} for equidistant measures
#' without missing values, which is faster than transforming the data into the
#' class \code{\link[fda]{fd}} and deriving this using \code{fda::\link[fda]{deriv.fd}}.
#' The second variant implies smoothing, which can be preferable for calculating
#' high order derivatives.
#' @param custom.metric [\code{function(x, y, ...)}]\cr
#' only used if \code{deriv.method = "custom.method"}.
#' A function of functional observations
#' \code{x} and \code{y} returning their distance.
#' The default is the Euclidean distance.
#' See how to implement your distance function in \code{\link[proxy]{dist}}
#' @param ...
#' further arguments to and from other methods. Hand over additional arguments to
#' \code{\link{computeDistMat}}, usually additional arguments for the specified
#' (semi-)metric. Also, if \code{deriv.method == "fda.deriv.fd"} or
#' \code{fdata} is not observed on a regular grid, additional arguments to
#' \code{\link{fdataTransform}} can be specified which will be passed on to
#' \code{\link[fda]{Data2fd}}.
#'
#' @return \code{classiKnn} returns an object of class \code{"classiKnn"}.
#' This is a  list containing  at least the
#' following components:
#'  \describe{
#'   \item{\code{call}}{the original function call.}
#'   \item{\code{classes}}{a factor of length nrow(fdata) coding the response of
#'   the training data set.}
#'   \item{\code{fdata}}{the raw functional data as a matrix with the individual
#'   observations as rows.}
#'   \item{\code{grid}}{numeric vector containing the grid on which \code{fdata}
#'   is observed)}
#'   \item{\code{proc.fdata}}{the preprocessed data (missing values interpolated,
#'   derived and evenly spaced). This data is \code{this.fdataTransform(fdata)}.
#'   See \code{this.fdataTransform} for more details.}
#'   \item{\code{knn}}{integer coding the number of nearest neighbors used in the
#'   k nearest neighbor classification algorithm.}
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
#'  }
#'
#'
#' @examples
#' # Classification of the Phoneme data
#' data(Phoneme)
#' classes = Phoneme[,"target"]
#'
#' set.seed(123)
#' # Use 80% of data as training set and 20% as test set
#' train_inds = sample(1:nrow(Phoneme), size = 0.8 * nrow(Phoneme), replace = FALSE)
#' test_inds = (1:nrow(Phoneme))[!(1:nrow(Phoneme)) %in% train_inds]
#'
#' # create functional data as matrix with observations as rows
#' fdata = Phoneme[,!colnames(Phoneme) == "target"]
#'
#' # create 3 nearest neighbor classifier with Euclidean distance (default) of the
#' # first order derivative of the data
#' mod = classiKnn(classes = classes[train_inds], fdata = fdata[train_inds,],
#'                  nderiv = 1L, knn = 3L)
#'
#' # predict the model for the test set
#' pred = predict(mod, newdata =  fdata[test_inds,], predict.type = "prob")
#' @export
classiKnn = function(classes, fdata, grid = 1:ncol(fdata), knn = 1L,
                     metric = "Euclidean", nderiv = 0L, derived = FALSE,
                     deriv.method = "base.diff",
                     custom.metric = function(x, y, ...) {
                       return(sqrt(sum((x - y)^2)))},
                     ...) {
  # check inputs
  if(class(fdata) == "data.frame")
    fdata = as.matrix(fdata)
  assert_numeric(fdata)
  assertClass(fdata, "matrix")
 if(is.numeric(classes))
    classes = factor(classes)
  assertFactor(classes, any.missing = FALSE, len = nrow(fdata))
  assertNumeric(grid, any.missing = FALSE, len = ncol(fdata))
  assertIntegerish(knn, lower = 1L, upper = nrow(fdata), len = 1)
  assertChoice(metric, choices = metric.choices())
  assertIntegerish(nderiv, lower = 0L)
  assertFlag(derived)
  assertChoice(deriv.method, c("base.diff", "fda.deriv.fd"))

  # check if data is evenly spaced  -> respace
  evenly.spaced = all.equal(grid, seq(grid[1], grid[length(grid)], length.out = length(grid)))
  no.missing = !anyMissing(fdata)

  # TODO write better warning message
  if(!no.missing) {
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

  ret = list(classes = classes,
             fdata = fdata,
             proc.fdata = proc.fdata,
             grid = grid,
             knn = knn,
             metric = metric,
             custom.metric = custom.metric,
             nderiv = nderiv,
             this.fdataTransform = this.fdataTransform,
             call = as.list(match.call(expand.dots = FALSE)))
  class(ret) = "classiKnn"

  return(ret)
}


#' @export
predict.classiKnn = function(object, newdata = NULL, predict.type = "response", ...) {
  # input checking
  if(!is.null(newdata)) {
    if(class(newdata) == "data.frame")
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

  # matrix containing which nearest neighbor the training observation is
  # for the new observation
  nn.mat = apply(dist.mat, 2, order)


  if (predict.type == "response") {
    result = apply(nn.mat, 2, function(x) {
      names(which.max(table(object$classes[x][1:object$knn])))
    })
    result = factor(result, levels = levels(object$classes))
  } else {
    # probabilities for the classes
    result = t(apply(nn.mat, 2, function(x) {
      table(object$classes[x][1:object$knn]) / object$knn
    }))
  }
  return(result)
}
