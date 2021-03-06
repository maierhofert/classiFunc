% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/classiKernel.R
\name{predict.classiKernel}
\alias{predict.classiKernel}
\title{predict a classiKernel object}
\usage{
\method{predict}{classiKernel}(object, newdata = NULL,
  predict.type = "response", parallel = FALSE, ...)
}
\arguments{
\item{object}{[\code{classiKernel}]\cr
object of class classiKernel to get predictions from}

\item{newdata}{[\code{data.frame}]\cr
(optional) new data to predict from with observations as rows. Do not derive this data,
this will be done automatically if required by the model. If \code{NULL},
the training data is predicted, currently without using a leave-one-out prediction.}

\item{predict.type}{[\code{character(1)}]\cr
one of 'response' or 'prob', indicating the type of prediction. Choose
'response' to return a vector of length \code{nrow(newdata)} containing the
most predicted class.
Choose 'prob' to return a matrix with \code{nrow(newdata)} rows containing
the probabilities for the classes as columns.}

\item{parallel}{[\code{logical(1)}]\cr
Should the prediction be parallelized?
Uses \code{\link[parallelMap]{parallelMap}} for
parallelization. See \code{...} for further arguments.}

\item{...}{[\code{list}]\cr
additional arguments to \link{computeDistMat}.}
}
\description{
predict function for a classiKnn object.
}
\seealso{
classiKernel
}
