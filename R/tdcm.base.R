#' @title Utility function for base estimation in TDCMs.
#'
#' @param data item response data
#' @param qnew stacked Q-matrix
#' @param dcmrule specific DCM to estimate
#' @keywords internal
tdcm.base <- function(data, qnew, dcmrule) {
  tdcm.1 <- CDM::gdina(
    data,
    qnew,
    linkfct = "logit",
    method = "ML",
    mono.constr = TRUE,
    progress = FALSE,
    maxit = 1,
    rule = dcmrule
  ) # tdcm.1
  return(tdcm.1)
} # tdcm.base
