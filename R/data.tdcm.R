#' @name data.tdcm
#'
#' @aliases data.tdcm data.tdcm01 data.tdcm02 data.tdcm03 data.tdcm04 data.tdcm05
#'
#' @title Several datasets for the \pkg{TDCM} package.
#'
#' @description
#' Several datasets for the \pkg{TDCM} package
#'
#'
#' @format
#' Dataset \code{data.tdcm01}: This simulated dataset has two time points, four attributes, twenty items, one group of size 1000, and a single Q-matrix. The format is list of two:
#' \itemize{
#'    \item \code{$data} a data frame of binary item responses
#'    \item \code{$qmatrix} a data frame specifying the Q-matrix
#' }
#'
#' Dataset \code{data.tdcm02}: This simulated dataset has three time points, two attributes, ten items, one group of size 2500, and a single Q-matrix. The format is a list of two:
#' \itemize{
#'    \item \code{$data} a data frame of binary item responses
#'    \item \code{$qmatrix} a data frame specifying the Q-matrix
#' }
#'
#' Dataset \code{data.tdcm03}: This simulated dataset has three time points, two attributes, one group of size 1500, and three different ten-item Q-matrices for each time point. Anchor items are specified as items 1/11/21 and items 14/24. The format is a list of five:
#' \itemize{
#'    \item \code{$data} a data frame of binary item responses
#'    \item \code{$q1} a data frame specifying the Q-matrix for the first time point
#'    \item \code{$q2} a data frame specifying the Q-matrix for the second time point
#'    \item \code{$q3} a data frame specifying the Q-matrix for the third time point
#'    \item \code{$qmatrix123} a data frame specifying the combined Q-matrix for all time points
#' }
#'
#' Dataset \code{data.tdcm04}: This simulated dataset has two time points, four attributes, twenty items, two groups of size 800 and 900, respectively, and a single Q-matrix. The format is a list of three:
#' \itemize{
#'    \item \code{$data} a data frame of binary item responses
#'    \item \code{$qmatrix} a data frame specifying the Q-matrix
#'    \item \code{$groups} a vector specifying the examinee group memberships

#' }
#' Dataset \code{data.tdcm05}: This simulated dataset has one time point, four attributes, and twenty items. For use with the 1-PLCDM. The format is a list of two:
#' \itemize{
#'    \item \code{$data} a data frame of binary item responses
#'    \item \code{$qmatrix} a data frame specifying the Q-matrix

#' }
#'
#' @examples
#' ## Example 1: T = 2, A = 4
#' data(data.tdcm01, package = "TDCM")
#' dat1 <- data.tdcm01$data
#' qmat1 <- data.tdcm01$qmatrix
#'
#' # estimate TDCM with invariance assumed and full LCDM
#' m1 <- TDCM::tdcm(dat1, qmat1, time.points = 2, invariance = TRUE, dcmrule = "GDINA")
#'
#' # summarize results with tdcm.summary function
#' results <- TDCM::tdcm.summary(m1, time.points = 2)
#' results$item.parameters
#' results$growth
#' results$transition.probabilities
#'
#' # estimate TDCM with invariance assumed and only main effects
#' m2 <- TDCM::tdcm(dat1, qmat1, time.points = 2, invariance = TRUE, dcmrule = "GDINA1")
#'
#' # estimate TDCM with invariance not assumed
#' m3 <- TDCM::tdcm(dat1, qmat1, time.points = 2, invariance = FALSE, dcmrule = "GDINA")
#'
#' # compare models to assess measurement invariance.
#' TDCM::tdcm.compare(m1, m3)
#'
#' ## Example 2: T = 3, A = 2
#' data(data.tdcm02, package = "TDCM")
#' dat2 <- data.tdcm02$data
#' qmat2 <- data.tdcm02$qmatrix
#'
#' # estimate TDCM with invariance assumed and full LCDM
#' m1 <- TDCM::tdcm(dat2, qmat2, time.points = 3)
#'
#' # estimate TDCM with invariance not assumed
#' m2 <- TDCM::tdcm(dat2, qmat2, time.points = 3, invariance = FALSE)
#'
#' # compare models to assess measurement invariance
#' TDCM::tdcm.compare(m1, m2)

NULL
