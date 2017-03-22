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
#' The three classes are called "Avonlea", "Clovis" and "Mix"."
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
#' A dataset containing the audio files of english words.
#'
#' This data set is a subsample of the data used in the paper Dual-domain
#' Hierarchical Classification of Phonetic Time Series. Each series is extracted
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
#' @source \url{http://timeseriesclassification.com/description.php?Dataset=Phoneme}
"Phoneme"


#' Yoga poses of two actors
#'
#' A dataset containing the outlines of two actors transiting between yoga poses.
#' This large data set is used for profiling.
#'
#' The dataset was obtained by capturing two actors transiting between yoga poses
#' in front of a green screen. The problem is to discriminate between one actor
#' (male) and another (female). Each image was converted to a one dimensional
#' series by finding the outline and measuring the distance of the outline to
#' the centre. The dataset was obtained by capturing two actors transiting
#' between yoga poses in front of a green screen. The problem is to discriminate
#' between one actor (male) and another (female). Each image was converted to a
#' one dimensional series by finding the outline and measuring the distance of
#' the outline to the centre.
#'
#' @section Format A data frame with 100 rows (=observations) and 65 variables:
#' \describe{
#'   \item{col 1:425}{one functional observation.}
#'   \item{target}{encoding the actor}
#' }
#' @source \url{http://www.timeseriesclassification.com/description.php?Dataset=Yoga}
"Yoga"
