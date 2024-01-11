#' Fitting Transition Diagnostic Classification Models (TDCMs)
#'
#' @description The `tdcm` Function calibrates and fits the transition
#' diagnostic classification model (TDCM; Madison & Bradshaw, 2018a), a
#' longitudinal extension of the log-linear cognitive diagnosis model (LCDM;
#' Henson, Templin, & Willse, 2009). To calibrate a TDCM involving multiple
#' groups, refer to the `mgtdcm` function.
#'
#' @details
#' Estimation is performed using the `CDM` package according to the approach
#' described in Madison et al. (2023).
#'
#' @param ir_data The item response data, a required \eqn{T \times N \times I}
#' `array` where \eqn{T} is the number of time points, \eqn{N} is the number of
#' responses, and \eqn{I} is the number of items. The `ir_from()` function can
#' be used to help prepare this argument using values from a two-dimensional
#' `array`, `data.frame`, or `matrix`.
#'
#' @param q_data The Q-matrix data, a required \eqn{T \times I \times A} `array`
#' where \eqn{T} is the number of time points, \eqn{I} is the number of items,
#' and \eqn{A} is the number of attributes. The `qmatrix_from()` function can be
#' used to help prepare this argument using values from a two-dimensional
#' `array`, `data.frame`, or `matrix`.
#'
#' @param invariance A `logical` (i.e., boolean) value that indicates whether
#' item parameter invariance is constrained to be equal at each time point. The
#' default value is `TRUE`. If set to `FALSE`, item parameters are not assumed
#' to be equal over time.
#'
#' @param dcm_rule A string or vector of condensation rules. Refer to the `rule`
#' parameter of `CDM::gdina` for more information. The default value is
#' `"GDINA"`.  If `dcm_rule` is a string, the condensation rule applies to all
#' items. If `dcm_rule` is a vector, then the condesnation rules are applied
#' item-wise.
#'
#' @return An object of class `tdcm` with entries as indicated in the CDM
#' package. For the TDCM-specific results (e.g., growth, transitions), results
#' are summarized using the tdcm.summary() function.
#'
#' @references
#' * de la Torre, J. (2011). The generalized DINA model framework.
#'   *Psychometrika, 76*, 179-199.
#' * George, A. C., Robitzsch, A., Kiefer, T., Gross, J., & Ünlü , A. (2016).
#'   The R package CDM for cognitive diagnosis models. *Journal of Statistical
#'   Software, 74*(2), 1-24.
#' * Henson, R., Templin, J., & Willse, J. (2009). Defining a family of cognitive
#'   diagnosis models using log linear models with latent variables.
#'   *Psychometrika, 74*, 191-21.
#' * Madison, M. J., & Bradshaw, L. (2018a). Assessing growth in a diagnostic
#'   classification model framework. *Psychometrika, 82*(4), 963-990.
#' * Madison, M. J., & Bradshaw, L. (2018b). Evaluating intervention effects in
#'   a diagnostic classification model framework. *Journal of Educational
#'   Measurement, 55*(1), 32-51.
#' * Madison, M.J., Chung, S., Kim, J., & Bradshaw, L. (2023). Approaches to
#'   estimating longitudinal diagnostic classification models.
#'   *Behaviormetrika*.
#' * Rupp, A. A., Templin, J., & Henson, R. (2010).
#'   *Diagnostic measurement: Theory, methods, and applications*.
#'   New York: Guilford.
#'
#' @export
tdcm <- function(
  ir_data,
  q_data,
  invariance = TRUE,
  dcm_rule = "GDINA"
) {

  tdcm_emit("Checking TDCM arguments...")
  tdcm_check_dims(ir_data, q_data)

  num_time_points = dim(ir_data)[1]
  num_responses = dim(ir_data)[2]
  num_items = dim(ir_data)[3]
  num_attributes = dim(q_data)[3]

  tdcm_emit(paste("- using num_time_points =", num_time_points))
  tdcm_emit(paste("- using num_responses =", num_responses))
  tdcm_emit(paste("- using num_items =", num_items))
  tdcm_emit(paste("- using num_attributes =", num_responses))

  tdcm_emit("Estimating TDCM...")
  if (FALSE) {
    CDM::gdd()
  }

  # if (number.q == 1) {
  #
  #  #Initial Data Sorting
  #  n.items = ncol(data) # Total Items
  #  items = n.items / time.points # Items per time point
  #  N = nrow(data) # Number of Examinees
  #  n.att = ncol(qmatrix) # Number of Attributes
  #
  #  #give names to items
  #  colnames(data) = paste("Item", 1:n.items)
  #  rownames(qmatrix) = paste("Item", 1:items)
  #  qnew = matrix(0, ncol = n.att * time.points, nrow = n.items) #build stacked Q-matrix
  #  for (z in 1:time.points) {
  #   for (i in 1:items) {
  #    for (j in 1:n.att) {
  #     qnew[i + ((z - 1) * items), j + ((z - 1) * n.att)] = qmatrix[i, j]
  #    }
  #   }
  #  } #end 3 for loops in building qnew
  #
  #  print("Estimating TDCM...", quote = F)
  #  tdcm.1 = CDM::gdina(
  #   data,
  #   qnew,
  #   linkfct = "logit",
  #   method = "ML",
  #   mono.constr = T,
  #   progress = F,
  #   maxit = 1,
  #   rule = dcmrule
  #  ) #Both variants use this as a base
  #  print(paste("Estimating TDCM, progress = ", round(stats::runif(1, 40, 60), 0), "%...", sep =
  #         ""),
  #     quote = F)
  #
  #  if (invariance == F) {
  #   #If NOT invariant ~ no designmatrix
  #   tdcm = CDM::gdina(
  #    data,
  #    qnew,
  #    linkfct = "logit",
  #    method = "ML",
  #    rule = dcmrule,
  #    progress = F
  #   )
  #   tdcm$invariance = F
  #  } else{
  #   #if invariance = T, then constrain item parms in design matrix
  #   c0 = tdcm.1$coef
  #   c.0 = nrow(c0)
  #   designmatrix = diag(nrow = c.0 / time.points,
  #             ncol = c.0 / time.points)
  #   delta.designmatrix = matrix(rep(t(designmatrix), time.points),
  #                 ncol = ncol(designmatrix),
  #                 byrow = TRUE)
  #   tdcm = CDM::gdina(
  #    data,
  #    qnew,
  #    linkfct = "logit",
  #    method = "ML",
  #    progress = F,
  #    delta.designmatrix = delta.designmatrix,
  #    rule = dcmrule
  #   )
  #  } #end invariance = T case
  # } else{
  #  # # multiple Q-matrices
  #  # tdcm = tdcm.mq(
  #  #  data = data,
  #  #  qmatrix = qmatrix,
  #  #  time.points = time.points,
  #  #  invariance = F,
  #  #  dcmrule = dcmrule,
  #  #  number.q = number.q,
  #  #  num.items = num.items,
  #  #  anchor = anchor
  #  # )
  #  tdcm_warn("tdcm_mq not yet supported")
  # }
  # print("Routine finished. Use the tdcm.summary function to display results.",
  #    quote = F)
  # return(tdcm)

  return(NULL)
 } # tdcm
