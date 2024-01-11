#' Estimating the Transition Diagnostic Classification Model (TDCM)
#'
#' @description Function to calibrate the transition diagnostic classification
#'   model (TDCM; Madison & Bradshaw, 2018a), which is a longitudinal extension
#'   of the log-linear cognitive diagnosis model (LCDM; Henson, Templin, &
#'   Willse, 2009). Allows for specification of many specific DCMs via the
#'   `dcmrule` option. For the multigroup TDCM, see `mg.tdcm`.
#'
#' @param data a required \eqn{N \times T \times I} matrix. For each time point,
#'   binary item responses are in the columns.
#'
#' @param qmatrix a required \eqn{I \times A} matrix indicating which items
#'   measure which attributes. If there are multiple Q-matrices, then they must
#'   have the same number of attributes and must be stacked on top of each other
#'   for estimation (to specify multiple Q-matrices, see `number.q`,
#'   `num.items`, and `anchor`).
#'
#' @param time.points the number of time points (i.e., measurement / testing
#'   occasions), integer \eqn{\ge 2}.
#'
#' @param invariance boolean indicating whether item parameter invariance
#'   should be constrained to be equal at each time point. Default = TRUE. If
#'   specified as false, item parameters are not assumed equal over time.
#'
#' @param dcmrule the specific DCM to be employed. Currently accepts
#'   “GDINA”, “ACDM”, “DINA”, “GDINA1”, “GDINA2”, and so on. Default is “GDINA”,
#'   which is implemented with a logit link to estimate the LCDM. The “ACDM”
#'   rule will estimate the LCDM with only main effects. The “DINA” rule will
#'   estimate the DINA model (Haertel, 1989; Junker & Sijtsma, 2001). “GDINA1”
#'   will estimate the LCDM with only main effects, equivalent to “ACDM”.
#'   “GDINA2” will estimate the LCDM with up to two-way interaction effects. If
#'   `dcmrule` is entered as a single string, that DCM will be assumed for each
#'   item. If entered as a vector, a DCM can be specified for each item.
#'
#' @param number.q the number of Q-matrices. For many applications, the
#'   same assessment is administered at each time point and this number is 1
#'   (default  = 1). If there are different Q-matrices for each time point, then
#'   this argument must be specified and should be equal to the number of time
#'   points. For example, if there are three time points, and the Q-matrix for
#'   each time point is different, then number.q = 3. If there are three time
#'   points, and the Q-matrix is different only for time point 3, then number.q
#'   is still specified as 3.

#' @param num.items when there are multiple Q-matrices, the number of items in
#'   each Q-matrix is specified as a T-length vector. For example, if there are
#'   three time points, and the Q-matrices for each time point have 8, 10, and
#'   12 items, respectively, then `num.items = c(8, 10, 12)`. Default is an
#'   empty vector to indicate there is only one Q-matrix.

#' @param anchor when there are different tests at each time point, this
#'   optional anchor argument is a vector of pairs of item numbers indicating
#'   which items are the same across time points and should be held invariant.
#'   For example, if there are three Q-matrices with 10 items each, and Items 1,
#'   11, and 21 are the same, and Items 14 and 24 are the same, then `anchor =
#'   c(1,11,1,21,14,24)`. Default is an empty vector to indicate there is only
#'   one Q-matrix.
#'
#' @details Estimation of the TDCM via the `CDM` package (George, et al.,
#'   2016), which is based on an EM algorithm as described in de la Torre
#'   (2011). The estimation approach is further detailed in Madison et al.
#'   (2023).

#' @return An object of class \code{gdina} with entries as indicated in the CDM
#'   package. For the TDCM-specific results (e.g., growth, transitions), results
#'   are summarized using the `tdcm.summary` function.
#'
#' @references
#' * de la Torre, J. (2011). The generalized DINA model framework.
#' _Psychometrika_, 76, 179-199.
#'
#' * George, A. C., Robitzsch, A., Kiefer, T., Gross, J., & Ünlü , A. (2016).
#' The R package CDM for cognitive diagnosis models.
#' _Journal of Statistical Software_, 74(2), 1-24.
#'
#' * Henson, R., Templin, J., & Willse, J. (2009). Defining a family of
#' cognitive diagnosis models using log linear models with latent variables.
#' _Psychometrika_, 74, 191-21.
#'
#' * Madison, M. J., & Bradshaw, L. (2018a). Assessing growth in a diagnostic
#'   classification model framework. _Psychometrika_, 82(4), 963-990.
#'
#' * Madison, M. J., & Bradshaw, L. (2018b). Evaluating intervention effects in
#'   a diagnostic classification model framework.
#'   _Journal of Educational Measurement_, 55(1), 32-51.
#'
#' * Madison, M.J., Chung, S., Kim, J., & Bradshaw, L. (2023). Approaches to
#'   estimating longitudinal diagnostic classification models.
#'   _Behaviormetrika_.
#'
#' * Rupp, A. A., Templin, J., & Henson, R. (2010).
#'   _Diagnostic measurement: Theory, methods, and applications_.
#'   New York: Guilford.
#'
#' @export
tdcm <- function(data,
                qmatrix,
                time.points,
                invariance = T,
                dcmrule = "GDINA",
                number.q = 1,
                num.items = c(),
                anchor = c()) {
  if (number.q == 1) {

    tdcm_emit("Preparing data...")

    n.items = ncol(data) #Total Items
    items = n.items / time.points #Items per time point
    N = nrow(data) #Number of Examinees
    n.att = ncol(qmatrix) #Number of Attributes

    #give names to items
    colnames(data) = paste("Item", 1:n.items)
    rownames(qmatrix) = paste("Item", 1:items)
    qnew = matrix(0, ncol = n.att * time.points, nrow = n.items) #build stacked Q-matrix
    for (z in 1:time.points) {
      for (i in 1:items) {
        for (j in 1:n.att) {
          qnew[i + ((z - 1) * items), j + ((z - 1) * n.att)] = qmatrix[i, j]
        }
      }
    } #end 3 for loops in building qnew

    print("Estimating TDCM...", quote = F)
    tdcm.1 = CDM::gdina(
      data,
      qnew,
      linkfct = "logit",
      method = "ML",
      mono.constr = T,
      progress = F,
      maxit = 1,
      rule = dcmrule
    ) #Both variants use this as a base

    tdcm_emit("Estimating TDCM...")

    if (invariance == F) {
      #If NOT invariant ~ no designmatrix
      tdcm = CDM::gdina(
        data,
        qnew,
        linkfct = "logit",
        method = "ML",
        rule = dcmrule,
        progress = F
      )
      tdcm$invariance = F
    } else{
      #if invariance = T, then constrain item parms in design matrix
      c0 = tdcm.1$coef
      c.0 = nrow(c0)
      designmatrix = diag(nrow = c.0 / time.points,
                          ncol = c.0 / time.points)
      delta.designmatrix = matrix(rep(t(designmatrix), time.points),
                                  ncol = ncol(designmatrix),
                                  byrow = TRUE)
      tdcm = CDM::gdina(
        data,
        qnew,
        linkfct = "logit",
        method = "ML",
        progress = F,
        delta.designmatrix = delta.designmatrix,
        rule = dcmrule
      )
    }#end invariance = T case
  }#end if number.q = 1 if statement
  else{
    #multiple Q-matrices
    tdcm = tdcm.mq(
      data = data,
      qmatrix = qmatrix,
      time.points = time.points,
      invariance = F,
      dcmrule = dcmrule,
      number.q = number.q,
      num.items = num.items,
      anchor = anchor
    )
  }
  print("Routine finished. Use the tdcm.summary function to display results.",
        quote = F)
  return(tdcm)
}
