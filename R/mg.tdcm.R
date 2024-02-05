#' Estimating the multigroup transition diagnostic classification model (TDCM)
#'
#' This function estimates the multigroup TDCM (Madison & Bradshaw, 2018).
#'
#' @param data A required \eqn{N \times T \times I} matrix. For each time point, binary item
#' responses are in the columns.
#'
#' @param q.matrix a required \eqn{I \times A} matrix indicating which items measure which
#' attributes.
#'
#' @param num.time.points The number of time points (i.e., measurement/testing occasions),
#' integer \eqn{\ge 2}.
#'
#' @param rule A string or a vector of the specific DCM to be employed. Currently accepts the
#' same values as `rule` in [CDM::gdina()]. The default is `"GDINA"`, which is implemented with a
#' logit link to estimate the LCDM. If `rule` is supplied as a single string, then that DCM will
#' be assumed for each item. If entered as a vector, a DCM can be specified for each item.
#'
#' @param groups A required vector of group identifiers for multiple group estimation.
#'
#' @param group.invariance logical. If `TRUE` (the default), item parameter invariance is assumed
#' to be equal for all groups. If `FALSE`, item parameter invariance is not assumed to be equal for
#' all groups.
#'
#' @param item.invariance logical. If `TRUE` (the default), item parameter invariance is assumed
#' to be equal for all time points. If `FALSE`, item parameter invariance is not assumed to be
#' equal for all time points.
#'
#' @param progress logical. If `FALSE` (the default), the function will print the progress of
#' estimation. If `TRUE`, no progress information is printed.
#'
#' @return An object of class \code{gdina} with entries as indicated in the \pkg{CDM} package.
#' For the TDCM-specific results (e.g., growth, transitions), use `TDCM::mg.tdcm.summary()`.
#'
#' @note
#' Currently, the `TDCM::mg.tdcm()` function only accepts a single Q-matrix.
#'
#' @inherit TDCM-package references
#'
#' @examples
#' \donttest{
#' ## Example 4: G = 2, T = 2, A = 4
#' data(data.tdcm04, package = "TDCM")
#' data <- data.tdcm04$data
#' q.matrix <- data.tdcm04$q.matrix
#' groups <- data.tdcm04$groups
#'
#' # Estimate full multigroup TDCM with invariance assumed.
#' mg.model <- TDCM::mg.tdcm(data, q.matrix, num.time.points = 2, groups = groups)
#'
#' # summarize results
#' results <- TDCM::mg.tdcm.summary(mg.model, num.time.points = 2)
#'
#' # plot results
#' TDCM::tdcm.plot(results)
#' }
#'
#' @export
mg.tdcm <- function(
    data,
    q.matrix,
    num.time.points,
    rule = "GDINA",
    groups,
    group.invariance = TRUE,
    item.invariance = TRUE,
    progress = FALSE
) {

  if (progress) {
    tdcm_emit("Preparing data for mg.tdcm()...")
  } # if

  # Initial Data Sorting
  n.items <- ncol(data) # Total Items
  items <- n.items / num.time.points # Items per time point
  N <- nrow(data) # Number of Examinees
  n.att <- ncol(q.matrix) # Number of Attributes
  group.invariance <- group.invariance
  item.invariance <- item.invariance
  num.groups <- length(unique(groups))
  colnames(data) <- paste("Item", 1:n.items)

  qnew <- matrix(0, ncol = n.att * num.time.points, nrow = n.items)
  for (z in 1:num.time.points) {
    for (i in 1:items) {
      for (j in 1:n.att) {
        qnew[i + ((z - 1) * items), j + ((z - 1) * n.att)] <- q.matrix[i, j]
      }
    }
  }

  if (progress) {
    tdcm_emit("Estimating the TDCM in mg.tdcm()... This may take a few minutes...")
  } # if

  complexity <- n.att * num.time.points * num.groups
  if (complexity < 20) {
    esttime <- round(stats::runif(1, 30, 50), 0)
  } else {
    esttime <- round(stats::runif(1, 10, 25), 0)
  }

  # Case 1: all invariance
  if (group.invariance == TRUE & item.invariance == TRUE) {
    # base model, 1 iteration for design matrix
    tdcm.1 <- CDM::gdina(data, qnew,
                         linkfct = "logit", method = "ML", mono.constr = TRUE,
                         group = groups, progress = FALSE, maxit = 1, rule = rule)

    # build design matrix
    c0 <- tdcm.1$coef
    c.0 <- nrow(c0)
    designmatrix <- diag(nrow = c.0 / num.time.points, ncol = c.0 / num.time.points)
    delta.designmatrix <- matrix(rep(t(designmatrix), num.time.points), ncol = ncol(designmatrix), byrow = TRUE)

    # estimate mg tdcm
    tdcm <- CDM::gdina(data, qnew,
                       group = groups, linkfct = "logit", method = "ML",
                       delta.designmatrix = delta.designmatrix, rule = rule, progress = FALSE)
  }

  # Case 2: group invariance, no time invariance
  else if (group.invariance == TRUE & item.invariance == FALSE) {
    # estimate mg tdcm
    tdcm <- CDM::gdina(data, qnew,
                       group = groups, linkfct = "logit", method = "ML", progress = FALSE,
                       rule = rule)
  }

  # Case 3: time invariance, no group invariance
  else if (group.invariance == FALSE & item.invariance == TRUE) {
    # base model, 1 iteration for design matrix
    tdcm.1 <- CDM::gdina(data, qnew,
                         linkfct = "logit", method = "ML", mono.constr = TRUE,
                         group = groups, progress = FALSE, maxit = 1, rule = rule, invariance = FALSE)

    # build design matrix
    c0 <- tdcm.1$coef
    c.0 <- nrow(c0)
    designmatrix <- diag(nrow = c.0 / num.time.points, ncol = c.0 / num.time.points)
    delta.designmatrix <- matrix(rep(t(designmatrix), num.time.points), ncol = ncol(designmatrix), byrow = TRUE)

    # estimate mg tdcm
    tdcm <- CDM::gdina(data, qnew,
                       group = groups, linkfct = "logit", method = "ML", progress = FALSE,
                       delta.designmatrix = delta.designmatrix, rule = rule, invariance = FALSE)
  }

  # Case 4: no group or time invariance
  else {
    # estimate mg tdcm
    tdcm <- CDM::gdina(data, qnew,
                       group = groups, linkfct = "logit", method = "ML", progress = FALSE,
                       rule = rule, invariance = FALSE)
  }

  tdcm$group.invariance <- group.invariance
  tdcm$item.invariance <- item.invariance

  # set progress value in result object
  tdcm$progress <- progress

  if (progress) {
    tdcm_emit(
      sprintf(
        "%s %s",
        "Done estimating the TDCM in mg.tdcm().",
        "Use mg.tdcm.summary() to display results."
      ) # sprintf
    ) # tdcm_emit
  } # if

  return(tdcm)
}
