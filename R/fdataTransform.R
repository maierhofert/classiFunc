#' @title Create a preprocessing pipeline function
#'
#' @description internal function, documented due to the importance of its
#' concept. Creates a pipeline function to do all the
#' preprocessing needed in \code{\link{classiKnn}}. This is helpful to ensure,
#' that the data preprocessing is carried out in exactly the same way for the
#' train and the test set in \code{predict.classiKnn}.
#'
#' @param grid,nderiv,derived,evenly.spaced,no.missing,deriv.method see
#' \code{\link{classiKnn}}
#' @param ... additional arguments to fda::Data2fd
#' @return pipeline function taking one argument \code{fdata}. The returned
#' function carries out all the preprocessing needed for the calling model
#' of class \code{\link{classiKnn}}.
#'
#' @export
fdataTransform = function(grid, nderiv, derived, evenly.spaced,
                          no.missing, deriv.method, ...) {
  if((derived | nderiv == 0L) & evenly.spaced & no.missing) {
    # original data can be used if no derivation or filling of missing values
    # or respacing is necessary
    return(function(fdata) fdata)
  } else if (evenly.spaced & no.missing & deriv.method == "base.diff") {
    # fast derivation using base::diff
    return(function(fdata) {
      for(i in 1:nderiv) {
        fdata = t(apply(fdata, 1, diff))
      }
      fdata
    })
  } else {
    # create a preprocessing function
    return(function(fdata){
      # get basis representation, fill NAs and derive the data
      fda.fdata = fda::Data2fd(argvals = grid, t(fdata), ...)
      if (!(derived | nderiv == 0L)) {
        fda.fdata = fda::deriv.fd(fda.fdata, nderiv = nderiv)
        # print("Derive")
      }
      fda.fdata = fda::eval.fd(evalarg = seq(grid[1], grid[length(grid)],
                                             length.out = length(grid)),
                               fdobj = fda.fdata)
      t(fda.fdata)
    })
  }
}
