
#' @title Create a knn estimator for functional data.
#'
#' @description Creates an efficient knn estimator for functional data. Currently
#' supported distance measures are all \code{metrics} implemented in \code{\link[proxy]{dist}}
#' and all semimetrics suggested in
#' Fuchs etal. 2015, Nearest neighbor ensembles for functional data with
#' interpretable feature selection,
#' (\url{http://www.sciencedirect.com/science/article/pii/S0169743915001100})
#' Additionally, all (semi-)metrics can be used on an arbitrary order of derivation.
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
#'   \item{\code{points}}{the differences at certain observation points, also
#'   called "points of impact". These are specified as a vector of indices
#'   of an evenly spaced sequence from \code{grid[1]} to \code{grid[length(grid)]}.
#'   The default value is \code{1L}.}
#'   \item{\code{custom.metric}}{your own semimetric will be used. Specify your
#'   own distance function in the argument \code{own.metric}}
#'  }
#' @param nderiv [\code{integer(1)}]\cr
#'   The order of derivation on which the metric shall be computed.
#'   The default is 0L.
#' @param derived [\code{logical(1)}]\cr
#' Is the data given in \code{fdata} already derived? Defaults to \code{FALSE},
#' which will lead to numerical derivation if \code{nderiv >= 1L} by applying
#' \code{\link[fda]{deriv.fd}} on a \code{\link[fda]{Data2fd}} representation of
#' \code{fdata}.
#' @param own.metric [\code{function(x, y, ...)}]\cr
#' returning a distance matrix with dimensions \code{nrow(x)} x \code{nrow(y)}.
#' See how to implement your distance function in \code{\link[proxy]{dist}}
#' @param ...
#' further arguments to and from other methods.
#' @return \code{classiKnn} returns an object of class \code{"classiKnn"}. \cr
#' An object of class \code{"classiKnn"} is a  list containing the following
#' components:
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
#' @importFrom fda Data2fd deriv.fd eval.fd
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
                     own.metric = NULL, ...) {
  # check inputs
  if(class(fdata) == "data.frame")
    fdata = as.matrix(fdata)
  assert_numeric(fdata)
  assertClass(fdata, "matrix")

  if(is.numeric(classes))
    classes = factor(classes)
  assertFactor(classes, any.missing = FALSE, len = nrow(fdata))
  assertNumeric(grid, any.missing = FALSE, len = ncol(fdata))
  assertChoice(metric, choices = "Euclidean") # TODO

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
                                       no.missing = no.missing, ...)
  proc.fdata = this.fdataTransform(fdata)

  ret = list(call = as.list(match.call()),
             classes = classes,
             fdata = fdata,
             proc.fdata = proc.fdata,
             grid = grid,
             knn = knn,
             metric = metric,
             nderiv = nderiv,
             this.fdataTransform = this.fdataTransform)
  class(ret) = "classiKnn"

  return(ret)
}

# param ... additional arguments to fda::Data2fd
# create a preprocessing function
# it uses arguments from the global environment
fdataTransform = function(fdata, grid, nderiv, derived, evenly.spaced, no.missing, ...) {
  if((derived | nderiv == 0L) & evenly.spaced & no.missing) {
    # original data can be used if no derivation or filling of missing values
    # or respacing is necessary
    return(function(fdata) fdata)
  } else {
    # create a preprocessing function
    function(fdata){
      # get basis representation, fill NAs and derive the data
      fda.fdata = fda::Data2fd(argvals = grid, t(fdata))
      if (!(derived | nderiv == 0L)) {
        fda.fdata = fda::deriv.fd(fda.fdata, nderiv = nderiv)
        # print("Derive")
      }
      fda.fdata = fda::eval.fd(evalarg = seq(grid[1], grid[length(grid)],
                                             length.out = length(grid)),
                               fdobj = fda.fdata)
      return(t(fda.fdata))
    }
  }
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


  dist.mat = computeDistMat(object$proc.fdata, newdata, method = object$metric, ...)

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

#' @title Compute a distance matrix
#'
#' @description This mainly internal function offers a unified framework to acces the
#' \code{\link[proxy]{dist}} function from the \code{proxy} package and additional
#' (semi-)metrics. For implemented methods see \code{\link{classiKnn}}.
#'
#' @param x matrix containing observations as rows
#' @param y see \code{x}. The default \code{NULL} uses \code{y = x}.
#' @param dmin,dmax,dmin1,dmax1,dmin2,dmax2 indizes used to define subspaces
#' @param method character string specifying the (semi-)metric to be used.
#' on the domain of \code{x}, used in different (semi-)metrics.
#' @param ... additional parameters to the (semi-)metrics.
#' @export
computeDistMat = function(x, y = NULL,
                          method = "Euclidean",
                          dmin = 1L, dmax = ncol(x),
                          dmin1 = 1L, dmax1 = ncol(x),
                          dmin2 = 1L, dmax2 = ncol(x),...) {
  requirePackages("proxy")
  proxy.set = unlist(summary(proxy::pr_DB)$names)

  assertChoice(method, choices = c(proxy.set))

  if(method %in% proxy.set) {
    return(as.matrix(proxy::dist(x, y, method = method, ...)))
  }

  # new semimetrics from fuchs etal 2015
  # they need two matrices as x and y
  if(is.null(y)) y = x
  if(method == "shortEuclidean") {
    stopifnot(dmin >= 1L)
    stopifnot(dmax <= ncol(x))
    computeDistMat(x[,dmin:dmax], y[,dmin:dmax], "Euclidean")
  }

  if(method == "mean") {
    return(outer(rowMeans(x), rowMeans(y), "-"))
  }

  if(method == "relAreas") {
    stopifnot(dmin1 >= 1L)
    stopifnot(dmax1 <= ncol(x))
    stopifnot(dmin2 >= 1L)
    stopifnot(dmax2 <= ncol(x))
  }

}



