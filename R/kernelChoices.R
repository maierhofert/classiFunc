# global function naming all implemented kernel functions

#' List the names of all implemented kernel functions
#'
#' \code{kerChoices} is a function returning the names of  all
#' kernel functions that are currently implemented in the
#' \code{\link{classiFunc}}-package and can be used for the argument
#' \code{ker} in \code{\link{classiKernel}}.
#'
#' @export
kerChoices = function() {
  ker.names = c(
    # from fda.usc
    "Ker.norm", "Ker.cos", "Ker.epa", "Ker.tri", "Ker.quar",
    "Ker.unif",
    "AKer.norm", "AKer.cos", "AKer.epa", "AKer.tri", "AKer.quar",
    "AKer.unif",
    # newly implemented
    "custom.ker" # , "Ker.Triang"
  )
  fda.usc::Kernel
  return(ker.names)
}
