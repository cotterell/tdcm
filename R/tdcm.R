#' @title Estimating the transition diagnostic classification model (TDCM)
#'
#' @description Function to calibrate the transition diagnostic classification model (TDCM; Madison & Bradshaw, 2018a),
#' which is a longitudinal extension of the log-linear cognitive diagnosis model (LCDM; Henson, Templin, & Willse, 2009).
#' Allows for specification of many specific DCMs via the \code{dcmrule} option. For the multigroup TDCM, see \code{\link{mg.tdcm}}.
#'
#' @param data a required \eqn{N \times T \times I} matrix. For each time point, binary item responses are in the columns.
#'
#' @param qmatrix a required \eqn{I \times A} matrix indicating which items measure which attributes. If there are multiple Q-matrices, then they must have the same number of attributes and must be stacked on top of each other for estimation (to specify multiple Q-matrices, see \code{number.q}, \code{num.items}, and \code{anchor}).
#'
#' @param time.points the number of time points (i.e., measurement/testing occasions), integer \eqn{\ge 2}.
#'
#' @param invariance boolean indicating whether item parameter invariance should be constrained to be equal at each time point. Default = T. If specified as false, item parameters are not assumed equal over time.
#'
#' @param dcmrule the specific DCM to be employed. Currently accepts “GDINA”, “ACDM”, “DINA”, "DINO", "RRUM", “GDINA1”, “GDINA2”, and so on. Default is “GDINA”, which is implemented with a logit link to estimate the LCDM. The “ACDM” rule will estimate the LCDM with only main effects. The “DINA” rule will estimate the DINA model (Haertel, 1989; Junker & Sijtsma, 2001). “GDINA1” will estimate the LCDM with only main effects, equivalent to “ACDM”. “GDINA2” will estimate the LCDM with up to two-way interaction effects. If \code{dcmrule} is entered as a single string, that DCM will be assumed for each item. If entered as a vector, a DCM can be specified for each item. Note 1: \code{dcmrule = "DINO"} does not display the LCDM main effects, only the negated interaction term. Note 2: \code{dcmrule = "RRUM"} does not display item parameters on the RRUM parametrization, only in the statistically equivalent ACDM form.
#'
#' @param number.q the number of Q-matrices. For many applications, the same assessment is administered at each time point and this number is 1 (default  = 1). If there are different Q-matrices for each time point, then this argument must be specified and should be equal to the number of time points. For example, if there are three time points, and the Q-matrix for each time point is different, then number.q = 3. If there are three time points, and the Q-matrix is different only for time point 3, then number.q is still specified as 3.

#' @param num.items when there are multiple Q-matrices, the number of items in each Q-matrix is specified as a T-length vector. For example, if there are three time points, and the Q-matrices for each time point have 8, 10, and 12 items, respectively, then \code{num.items = c(8, 10, 12)}. Default is an empty vector to indicate there is only one Q-matrix.

#' @param anchor when there are different tests at each time point, this optional anchor argument is a vector of pairs of item numbers indicating which items are the same across time points and should be held invariant. For example, if there are three Q-matrices with 10 items each, and Items 1, 11, and 21 are the same, and Items 14 and 24 are the same, then \code{anchor = c(1,11,1,21,14,24)}. Default is an empty vector to indicate there is only one Q-matrix.
#'
#' @param progress An optional logical indicating whether the function should print the progress of estimation.
#'
#' @details Estimation of the TDCM via the \pkg{CDM} package (George, et al., 2016), which is based on an
#' EM algorithm as described in de la Torre (2011). The estimation approach is further detailed in Madison et al. (2023).

#' @return An object of class \code{gdina} with entries as indicated in the CDM package. For the TDCM-specific results (e.g., growth, transitions), results are summarized using the \code{\link{tdcm.summary}} function.
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
#'
#' ## Example 3: T = 3, A = 2, Q = 3 (multiple Q-matrices)
#' data(data.tdcm03, package = "TDCM")
#' dat3 <- data.tdcm03$data
#' qmat3 <- data.tdcm03$qmatrix123
#'
#' # estimate TDCM with full LCDM and three Q-matrices with 10 items each
#' m1 <- TDCM::tdcm(dat3, qmat3, time.points = 3, number.q = 3, num.items = c(10, 10, 10))
#'
#' # estimate TDCM with full LCDM and three Q-matrices with 10 items each and anchor items
#' m2 <- TDCM::tdcm(dat3, qmat3, time.points = 3, number.q = 3, num.items = c(10, 10, 10),
#' anchor = c(1, 11, 1, 21, 14, 24))
#'
#' # compare models to assess measurement invariance of anchor items
#' TDCM::tdcm.compare(m1, m2)
#'
#' @references
#' de la Torre, J. (2011). The generalized DINA model framework. \emph{Psychometrika, 76}, 179-199.
#'
#' George, A. C., Robitzsch, A., Kiefer, T., Gross, J., & Ünlü , A. (2016). The R package CDM for cognitive diagnosis models. \emph{Journal of Statistical Software, 74}(2), 1-24.
#'
#' Henson, R., Templin, J., & Willse, J. (2009). Defining a family of cognitive diagnosis models using log linear models with latent variables. \emph{Psychometrika, 74}, 191-21.
#'
#' Madison, M. J., & Bradshaw, L. (2018a). Assessing growth in a diagnostic classification model framework. \emph{Psychometrika, 82}(4), 963-990.
#'
#' Madison, M. J., & Bradshaw, L. (2018b). Evaluating intervention effects in a diagnostic classification model framework. \emph{Journal of Educational Measurement, 55}(1), 32-51.
#'
#' Madison, M.J., Chung, S., Kim, J., & Bradshaw, L. (2023). Approaches to estimating longitudinal diagnostic classification models. \emph{Behaviormetrika}.
#'
#' Rupp, A. A., Templin, J., & Henson, R. (2010). \emph{Diagnostic measurement: Theory, methods, and applications}. New York: Guilford.
#'
#' @export
tdcm <- function(data, qmatrix, time.points, invariance = TRUE, dcmrule = "GDINA", number.q = 1, num.items = c(), anchor = c(), progress = FALSE) {

  if (number.q == 1) {

    if (progress) {
      tdcm_emit("Preparing data for tdcm()...")
    } # if

    # Initial Data Sorting
    n.items <- ncol(data) # Total Items
    items <- n.items / time.points # Items per time point
    N <- nrow(data) # Number of Examinees
    n.att <- ncol(qmatrix) # Number of Attributes

    # give names to items
    colnames(data) <- paste("Item", 1:n.items)
    rownames(qmatrix) <- paste("Item", 1:items)

    # build stacked Q-matrix
    qnew <- matrix(0, ncol = n.att * time.points, nrow = n.items)
    for (z in 1:time.points) {
      for (i in 1:items) {
        for (j in 1:n.att) {
          qnew[i + ((z - 1) * items), j + ((z - 1) * n.att)] <- qmatrix[i, j]
        } # for
      } # for
    } # for

    if (progress) {
      tdcm_emit("Estimating the TDCM in tdcm()...")
    } # if

    if (invariance == FALSE) {
      # If NOT invariant ~ no designmatrix
      tdcm <- CDM::gdina(
        data,
        qnew,
        linkfct = "logit",
        method = "ML",
        rule = dcmrule,
        progress = FALSE
      ) # tdcm
      tdcm$invariance <- FALSE
    } else {
      # if invariance = T, then constrain item params in design matrix
      tdcm.1 <- tdcm.base(data, qnew, dcmrule)
      c0 <- tdcm.1$coef
      c.0 <- nrow(c0)
      designmatrix <- diag(nrow = c.0 / time.points, ncol = c.0 / time.points)
      delta.designmatrix <- matrix(rep(t(designmatrix), time.points), ncol = ncol(designmatrix), byrow = TRUE)
      tdcm <- CDM::gdina(
        data,
        qnew,
        linkfct = "logit",
        method = "ML",
        progress = FALSE,
        delta.designmatrix = delta.designmatrix,
        rule = dcmrule
      ) # tdcm
    } # if
  } else { # multiple Q-matrices
    tdcm <- tdcm.mq(
      data = data,
      qmatrix = qmatrix,
      time.points = time.points,
      invariance = FALSE,
      dcmrule = dcmrule,
      number.q = number.q,
      num.items = num.items,
      anchor = anchor
    ) # tdcm
  } # if

  # set progress value in result object
  tdcm$progress <- progress

  if (progress) {
    tdcm_emit(
      sprintf(
        "%s %s",
        "Done estimating the TDCM in tdcm().",
        "Use tdcm.summary() to display results."
      ) # sprintf
    ) # tdcm_emit
  } # if

  return(tdcm)

} # tdcm
