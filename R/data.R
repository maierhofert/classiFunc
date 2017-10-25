#' The shape of arrow heads.
#'
#' A dataset containing the outline of arrow heads as functional covariable.
#' This is a subset of the "ArrowHead" data set of the UCR TSC repository.
#'
#' The arrowhead data consists of outlines of the images of arrowheads.
#' The shapes of the projectile points are converted into a time series using
#' the angle-based method. The classification of projectile points is an
#' important topic in anthropology. The classes are based on shape distinctions
#' such as the presence and location of a notch in the arrow. The problem in the
#' repository is a length normalised version of that used in Ye09shapelets.
#' The three classes are called "Avonlea", "Clovis" and "Mix".
#'
#' @section Format A data frame with 100 rows (=observations) and 84 variables:
#' \describe{
#'   \item{col 1:83}{shape of a projectile as functional observation.}
#'   \item{target}{encoding the class of the projectile.}
#' }
#' @source \url{http://timeseriesclassification.com/description.php?Dataset=ArrowHead}
"ArrowHead"

#' Phonetic Time Series.
#'
#' A data set containing the audio files of english words.
#'
#' This data set is a subsample of the data used in Hamooni and Mueen (2014).
#' Each series is extracted
#' from the segmented audio collected from Google Translate, oxforddictionaries.com
#' and the Merrriam-Webster online dictionary. Each of these sources have
#' different features. Audio files collected from Google translate, Oxford, and
#' Merrriam-Webster dictionaries are recorded at 22050, 44100 and 11025 samples
#' per second respectively. All of them have male and female speakers in
#' different ratios. The Oxford dictionary includes British and American accent
#' pronunciation for each word. After data collection, they segment waveforms of
#' the words to generate phonemes using the Forced Aligner tool from the Penn
#' Phonetics Laboratory.
#'
#' @section Format A data frame with 100 rows (=observations) and 65 variables:
#' \describe{
#'   \item{col 1:64}{one functional observation.}
#'   \item{target}{encoding the word of the functional observation}
#' }
#' @references
#' Hamooni, Hossein, and Mueen, Abdullah.
#' "Dual-domain hierarchical classification of phonetic time series."
#' Data Mining (ICDM), 2014 IEEE International Conference on. IEEE, 2014.
#' @source \url{http://timeseriesclassification.com/description.php?Dataset=Phoneme}
"Phoneme"


##' Diffusion Tensor Imaging: tract profiles and outcomes
##'
##' Fractional anisotropy (FA) tract profiles for the corpus callosum (cca) and
##' the right corticospinal tract (rcst). Accompanying the tract profiles are
##' the subject ID numbers, visit number, total number of scans, multiple
##' sclerosis case status and Paced Auditory Serial Addition Test (pasat)
##' score.
##'
##' If you use this data as an example in written work, please include the
##' following acknowledgment: ``The MRI/DTI data were collected at Johns
##' Hopkins University and the Kennedy-Krieger Institute"
##'
##' Data and description was copied from the \code{\link[refund]{refund}} package.
##'
##' @name DTI
##' @docType data
##' @format A data frame made up of \describe{
##' \item{cca}{A 382 x 93
##' matrix of fractional anisotropy tract profiles from the corpus
##' callosum;}
##' \item{rcst}{A 382 x 55 matrix
##' of fractional anisotropy tract profiles from the right corticospinal
##' tract;}
##' \item{ID}{Numeric vector of subject ID numbers;}
##' \item{visit}{Numeric vector of the subject-specific visit
##' numbers;}
##' \item{visit.time}{Numeric vector of the subject-specific visit time, measured
##' in days since first visit;}
##' \item{Nscans}{Numeric vector indicating the total number of visits
##' for each subject;}
##' \item{case}{Numeric vector of multiple sclerosis case status: 0 - healthy control, 1 - MS case;}
##' \item{sex}{factor variable indicated subject's sex;}
##'
##' \item{pasat}{Numeric vector containing the PASAT score at
##' each visit.}
##' }
##' @references Goldsmith, J., Bobb, J., Crainiceanu, C., Caffo, B., and Reich,
##' D. (2011). Penalized Functional Regression. \emph{Journal of Computational
##' and Graphical Statistics}, 20, 830 - 851.
##'
##' Goldsmith, J., Crainiceanu, C., Caffo, B., and Reich, D. (2010).
##' Longitudinal Penalized Functional Regression for Cognitive Outcomes on
##' Neuronal Tract Measurements. \emph{Journal of the Royal Statistical
##' Society: Series C}, 61, 453 - 469.
NULL


##' Berkeley Growth Study Data
##'
##' A data frame containing the heights of 39 boys and 54 girls from age 1 to 18,
##' the ages at which they were collected.
##'
##'
##' Data and description was reformatted from the \code{\link[fda]{fda}} package.
##'
##' @name Growth_irregular
##' @docType data
##' @format A list made up of \describe{
##' \item{ID}{Factor of length 93 containing the subject IDs}
##' \item{sex}{Factor encoding the sex of children with values in \code{c(`male`, `female`)}}
##' \item{age}{Numeric vector of length 31 encoding the age at the measurements}
##' \item{height}{A 93 x 31 matrix giving the height in cm of 93 children at 31 ages}
##' }
##' @details The ages are not equally spaced, see \code{Growth$age}.
##' @references Ramsay, James O., and Silverman, Bernard W. (2006),
##' Functional Data Analysis, 2nd ed., Springer, New York.
##'
##' Ramsay, James O., and Silverman, Bernard W. (2002),
##' Applied Functional Data Analysis, Springer, New York, ch. 6.
##'
##' Tuddenham, R. D., and Snyder, M. M. (1954)
##' "Physical growth of California boys and girls from birth to age 18",
##'  University of California Publications in Child Development, 1, 183-364.
##' @seealso Growth
NULL

##' Berkeley Growth Study Data (regular grid)
##'
##' A data frame containing the heights of 39 boys and 54 girls from age 1 to 18,
##' the ages at which they were collected.
##'
##'
##' Data and description was reformatted from the \code{\link[fda]{fda}} package
##' to be observed on a regular grid in one year steps.
##'
##' @name Growth
##' @docType data
##' @format A list made up of \describe{
##' \item{ID}{Factor of length 93 containing the subject IDs}
##' \item{sex}{Factor encoding the sex of children with values in \code{c(`male`, `female`)}}
##' \item{height}{A 93 x 31 matrix giving the height in cm of 93 children at 31 ages}
##' }
##'  @inheritSection Growth_irregular references
##'  @seealso Growth_irregular
NULL
