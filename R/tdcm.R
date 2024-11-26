#' Estimating the Transition Diagnostic Classification Model (TDCM)
#'
#' `tdcm()` is used to estimate the transition diagnostic classification model
#' (TDCM; Madison & Bradshaw, 2018a), which is a longitudinal extension of the
#' log-linear cognitive diagnosis model (LCDM; Henson, Templin, & Willse,
#' 2009). For the multigroup TDCM, see [TDCM::mg.tdcm()].  It allows for the
#' specification of many specific DCMs via the `rule` option. The default DCM
#' rule and link function specifies the LCDM. The rule can be changed to
#' estimate the DINA model, DINO model, CRUM (i.e., ACDM, or main effects
#' model), or reduced interaction versions of the LCDM. The link function can be
#' changed to specify the GDINA model.
#'
#' @param data A required \eqn{N \times T \times I} data matrix containing
#'     binary item responses.  For each time point, the binary item responses
#'     are in the columns.
#'
#' @param q.matrix A required \eqn{I \times A} matrix indicating which items
#'     measure which attributes. If there are multiple Q-matrices, then they
#'     must have the same number of attributes and must be stacked on top of
#'     each other for estimation (to specify multiple Q-matrices, see
#'     `num.q.matrix`, `num.items`, and `anchor`).
#'
#' @param num.time.points A required integer \eqn{\ge 2} specifying the number
#'     of time points (i.e., measurement occasions).
#'
#' @param invariance logical. If `TRUE` (the default), then item parameters will
#'     be constrained to be equal at each time point. If `FALSE`, item
#'     parameters are not assumed to be equal over time.
#'
#' @param rule A string or a vector indicating the specific DCM to be employed.
#'     A vector of supported `rule` values is provided by [TDCM::tdcm.rules].
#'     If `rule` is supplied as a single string, then that DCM will be assumed
#'     for each item. If entered as a vector, a rule can be specified for each
#'     item.
#'
#' @param linkfct A string or a vector indicating the LCDM link
#'     function. Currently accepts "logit" (default) to estimate the LCDM. Can
#'     be specified "identity" to estimate the GDINA model. Also accepts a "log"
#'     link function.
#'
#' @param num.q.matrix An optional integer specifying the number of
#'     Q-matrices. For many applications, the same assessment is administered at
#'     each time point and this number is 1 (the default). If there are
#'     different Q-matrices for each time point, then this argument must be
#'     specified and should be equal to the number of time points. For example,
#'     if there are three time points, and the Q-matrix for each time point is
#'     different, then `num.q.matrix = 3`. If there are three time points, and
#'     the Q-matrix is different only for time point 3, then `num.q.matrix` is
#'     still specified as `3`.
#'
#' @param num.items An integer specifying the number of items. When there are
#'     multiple Q-matrices, the number of items in each Q-matrix is specified as
#'     a vector. For example, if there are three time points, and the Q-matrices
#'     for each time point have 8, 10, and 12 items, respectively, then
#'     `num.items = c(8, 10, 12)`.
#'
#' @param anchor When there are different tests at each time point, this
#'     optional argument is a vector of pairs of item numbers indicating which
#'     items are the same across time points and should be held invariant. For
#'     example, if there are three Q-matrices with 10 items each, and Items 1,
#'     11, and 21 are the same, and Items 14 and 24 are the same, then `anchor =
#'     c(1,11,1,21,14,24)`. Default is an empty vector to indicate absence of
#'     anchor items. Note: when anchor is specified, invariance is automatically
#'     set to false for non-anchor items.
#'
#' @param forget.att An optional vector allowing for constraining of individual
#'     attribute proficiency loss, or forgetting. The default allows forgetting
#'     for each measured attribute (e.g., \eqn{P(1 \rightarrow 0) \neq 0}). This
#'     vector is specified to indicate the attributes for which forgetting is
#'     not permitted.
#'
#' @param progress logical. If `FALSE`, the function will print the progress of
#'     estimation. If `TRUE` (default), no progress information is printed.
#'
#' @details Estimation of the TDCM via the \pkg{CDM} package (George, et al.,
#'     2016), which is based on an EM algorithm as described in de la Torre
#'     (2011). The estimation approach is further detailed in Madison et
#'     al. (2023).
#'
#' @return An object of class \code{gdina} with entries as described in
#'     [CDM::gdina()]. To see a TDCM-specific summary of the object (e.g.,
#'     growth, transitions), use [TDCM::tdcm.summary()].
#'
#' @inherit TDCM-package references
#'
#' @examples
#' \donttest{
#' ## Example 1: T = 2, A = 4
#' data(data.tdcm01, package = "TDCM")
#' data <- data.tdcm01$data
#' q.matrix <- data.tdcm01$q.matrix
#'
#' # Estimate full TDCM with invariance assumed.
#' model1 <- TDCM::tdcm(data, q.matrix, num.time.points = 2)
#'
#' # Summarize results with tdcm.summary().
#' results <- TDCM::tdcm.summary(model1)
#' results$item.parameters
#' results$growth
#' results$transition.probabilities
#' }
#' @export
tdcm <- function(
    data,
    q.matrix,
    num.time.points,
    invariance = TRUE,
    rule = "LCDM",
    linkfct = "logit",
    num.q.matrix = 1,
    num.items = c(),
    anchor = c(),
    forget.att = c(),
    progress = TRUE
) {

  # translate rule argument
  rule <- tdcm.rule.as.cdm.rule(rule)

  if (num.q.matrix == 1) {

    if (progress) {
      print("Preparing data for tdcm()...", quote = FALSE)
    } # if

    # if (progress) {
    #   tdcm_emit("Preparing data for tdcm()...")
    # } # if

    # Initial Data Sorting
    n.items <- ncol(data) # Total Items
    items <- n.items / num.time.points # Items per time point
    N <- nrow(data) # Number of Examinees
    n.att <- ncol(q.matrix) # Number of Attributes

    # give names to items
    colnames(data) <- paste("Item", 1:n.items)
    rownames(q.matrix) <- paste("Item", 1:items)

    # build stacked Q-matrix
    qnew <- matrix(0, ncol = n.att * num.time.points, nrow = n.items)
    for (z in 1:num.time.points) {
      for (i in 1:items) {
        for (j in 1:n.att) {
          qnew[i + ((z - 1) * items), j + ((z - 1) * n.att)] <- q.matrix[i, j]
        } # for
      } # for
    } # for

    # if (progress) {
    #   tdcm_emit("Estimating the TDCM in tdcm(). Depending on model complexity, estimation time may vary.")
    # } # if

    if (progress) {
      print("Estimating the multigroup TDCM in mg.tdcm()...", quote = FALSE)
      print("Depending on model complexity, estimation time may vary...", quote = FALSE)
    } # if

    #if user constraints forgetting
    if(length(forget.att != 0)){

      #reduce the skill space
      m0 <- tdcm.base(data, qnew, rule)
      full.space = m0$attribute.patt.splitted

      forget = c()
      for(i in forget.att){

        rows = which(full.space[,i] > full.space[,i+n.att])
        forget = append(forget, rows)

      }

      forget = unique(forget)
      red.space = full.space[-forget,]

    } else{#full skill space

      m0 <- tdcm.base(data, qnew, rule)
      red.space = m0$attribute.patt.splitted

    }


    if (invariance == FALSE) {
      # If NOT invariant ~ no designmatrix
      tdcm <- suppressWarnings(CDM::gdina(
        data,
        qnew,
        linkfct = linkfct,
        method = "ML",
        rule = rule,
        skillclasses = red.space,
        reduced.skillspace=FALSE,
        progress = FALSE
      )) # tdcm
      tdcm$invariance <- FALSE
    } else {
      # if invariance = T, then constrain item params in design matrix
      tdcm.1 <- tdcm.base(data, qnew, rule)
      c0 <- tdcm.1$coef
      c.0 <- nrow(c0)
      designmatrix <- diag(nrow = c.0 / num.time.points, ncol = c.0 / num.time.points)
      delta.designmatrix <- matrix(rep(t(designmatrix), num.time.points), ncol = ncol(designmatrix), byrow = TRUE)
      tdcm <- suppressWarnings(CDM::gdina(
        data,
        qnew,
        linkfct = linkfct,
        method = "ML",
        progress = FALSE,
        delta.designmatrix = delta.designmatrix,
        skillclasses = red.space,
        rule = rule,
        reduced.skillspace=FALSE
      )) # tdcm
    } # if
  } else { # multiple Q-matrices
    tdcm <- tdcm.mq(
      data = data,
      q.matrix = q.matrix,
      num.time.points = num.time.points,
      invariance = FALSE,
      rule = rule,
      linkfct = linkfct,
      num.q.matrix = num.q.matrix,
      num.items = num.items,
      forget.att = c(),
      anchor = anchor,
      progress = progress
    ) # tdcm
  } # if

  # set progress value in result object
  tdcm$progress <- progress

  # save number of time points
  tdcm$numtimepoints = num.time.points

  if (progress) {
    print("TDCM estimation complete.", quote = FALSE)
    print("Use tdcm.summary() to display results.", quote = FALSE)
  } # if

  # if (progress) {
  #   tdcm_emit(
  #     sprintf(
  #       "%s %s",
  #       "TDCM estimation complete.",
  #       "Use tdcm.summary() to display results."
  #     ) # sprintf
  #   ) # tdcm_emit
  # } # if

  return(tdcm)

} # tdcm
