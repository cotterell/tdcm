#' @title Estimating the multigroup transition diagnostic classification model (TDCM)
#'
#' @description This function estimates the multigroup TDCM (Madison & Bradshaw, 2018).
#'
#'
#'
#' @param data a required \eqn{N \times T \times I} matrix. For each time point, binary item responses are in the columns.
#'
#' @param qmatrix a required \eqn{I \times A} matrix indicating which items measure which attributes.
#'
#' @param time.points the number of time points (i.e., measurement/testing occasions), integer \eqn{\ge 2}.
#'
#' @param dcmrule the specific DCM to be employed. Currently accepts “GDINA”, “ACDM”, “DINA”, “GDINA1”, “GDINA2”, and so on. Default is “GDINA”, which is implemented with a logit link to estimate the LCDM. The “ACDM” rule will estimate the LCDM with only main effects. The “DINA” rule will estimate the DINA model. “GDINA1” will estimate the LCDM with only main effects, equivalent to “ACDM”. “GDINA2” will estimate the LCDM with up to two-way interaction effects. If dcmrule is entered as a single string, that DCM will be assumed for each item. If entered as a vector, a DCM can be specified for each item.
#'
#' @param groups A required vector of group identifiers for multiple group estimation.
#'
#' @param group.invariance logical indicator for whether item parameter invariance should be assumed equal for all groups. Default = T. If specified as false, item parameters are not assumed equal for groups.
#'
#' @param item.invariance logical indicator for whether item parameter invariance should be constrained to be equal at each time point. Default = T. If specified as false, item parameters are not assumed equal over time.
#'
#' @param progress An optional logical indicating whether the function should print the progress of estimation.

#' @return An object of class \code{gdina} with entries as indicated in the \pkg{CDM} package. For the TDCM-specific results (e.g., growth, transitions), results are summarized using the \code{\link{mg.tdcm.summary}} function.
#'
#' @note
#' Currently, this function currently only accepts a single Q-matrix.
#'
#' @export
#'
#' @references
#' Madison, M. J., & Bradshaw, L. (2018). Evaluating intervention effects in a diagnostic classification model framework. \emph{Journal of Educational Measurement, 55}(1), 32-51.

#'
#' @examples
#' ## Example 4: G = 2, T = 2, A = 4
#' data(data.tdcm04, package = "TDCM")
#' dat4 <- data.tdcm04$data
#' qmat4 <- data.tdcm04$qmatrix
#' group4 <- data.tdcm04$groups
#'
#' # estimate mgTDCM with invariance assumed and full LCDM
#' mg1 <- TDCM::mg.tdcm(dat4, qmat4,
#'   time.points = 2, dcmrule = "GDINA",
#'   group = group4, group.invariance = TRUE, item.invariance = TRUE)
#'
#' # summarize results
#' results1 <- TDCM::mg.tdcm.summary(mg1, time.points = 2)
#'
#' # plot results
#' TDCM::tdcm.plot(results1)
#'
#' # estimate mgTDCM without group invariance
#' mg2 <- TDCM::mg.tdcm(dat4, qmat4,
#'   time.points = 2, dcmrule = "GDINA",
#'   group = group4, group.invariance = FALSE, item.invariance = TRUE)
#'
#' # compare models to assess group invariance
#' TDCM::tdcm.compare(mg1, mg2)
#'
mg.tdcm <- function(data, qmatrix, time.points, dcmrule = "GDINA",
                    groups, group.invariance = TRUE, item.invariance = TRUE, progress = TRUE) {
  if (progress == TRUE) {
    print("Preparing data...", quote = FALSE)
  }

  # Initial Data Sorting
  n.items <- ncol(data) # Total Items
  items <- n.items / time.points # Items per time point
  N <- nrow(data) # Number of Examinees
  n.att <- ncol(qmatrix) # Number of Attributes
  group.invariance <- group.invariance
  item.invariance <- item.invariance
  num.groups <- length(unique(groups))
  colnames(data) <- paste("Item", 1:n.items)

  qnew <- matrix(0, ncol = n.att * time.points, nrow = n.items)
  for (z in 1:time.points) {
    for (i in 1:items) {
      for (j in 1:n.att) {
        qnew[i + ((z - 1) * items), j + ((z - 1) * n.att)] <- qmatrix[i, j]
      }
    }
  }

  if (progress == TRUE) {
    print("Estimating mgTDCM...", quote = FALSE)
    print("Note: Depending on model specifications, estimation for the multigroup TDCM can take a few minutes. ", quote = FALSE)
  }

  complexity <- n.att * time.points * num.groups
  if (complexity < 20) {
    esttime <- round(stats::runif(1, 30, 50), 0)
  } else {
    esttime <- round(stats::runif(1, 10, 25), 0)
  }

  if (progress == TRUE) {
    print(paste("Estimating mgTDCM, progress = ", esttime, "%...", sep = ""), quote = FALSE)
  }

  # Case 1: all invariance
  if (group.invariance == TRUE & item.invariance == TRUE) {
    # base model, 1 iteration for design matrix
    tdcm.1 <- CDM::gdina(data, qnew,
                         linkfct = "logit", method = "ML", mono.constr = TRUE,
                         group = groups, progress = FALSE, maxit = 1, rule = dcmrule)

    # build design matrix
    c0 <- tdcm.1$coef
    c.0 <- nrow(c0)
    designmatrix <- diag(nrow = c.0 / time.points, ncol = c.0 / time.points)
    delta.designmatrix <- matrix(rep(t(designmatrix), time.points), ncol = ncol(designmatrix), byrow = TRUE)

    # estimate mg tdcm
    tdcm <- CDM::gdina(data, qnew,
                       group = groups, linkfct = "logit", method = "ML",
                       delta.designmatrix = delta.designmatrix, rule = dcmrule, progress = FALSE)
  }

  # Case 2: group invariance, no time invariance
  else if (group.invariance == TRUE & item.invariance == FALSE) {
    # estimate mg tdcm
    tdcm <- CDM::gdina(data, qnew,
                       group = groups, linkfct = "logit", method = "ML", progress = FALSE,
                       rule = dcmrule)
  }

  # Case 3: time invariance, no group invariance
  else if (group.invariance == FALSE & item.invariance == TRUE) {
    # base model, 1 iteration for design matrix
    tdcm.1 <- CDM::gdina(data, qnew,
                         linkfct = "logit", method = "ML", mono.constr = TRUE,
                         group = groups, progress = FALSE, maxit = 1, rule = dcmrule, invariance = FALSE)

    # build design matrix
    c0 <- tdcm.1$coef
    c.0 <- nrow(c0)
    designmatrix <- diag(nrow = c.0 / time.points, ncol = c.0 / time.points)
    delta.designmatrix <- matrix(rep(t(designmatrix), time.points), ncol = ncol(designmatrix), byrow = TRUE)

    # estimate mg tdcm
    tdcm <- CDM::gdina(data, qnew,
                       group = groups, linkfct = "logit", method = "ML", progress = FALSE,
                       delta.designmatrix = delta.designmatrix, rule = dcmrule, invariance = FALSE)
  }

  # Case 4: no group or time invariance
  else {
    # estimate mg tdcm
    tdcm <- CDM::gdina(data, qnew,
                       group = groups, linkfct = "logit", method = "ML", progress = FALSE,
                       rule = dcmrule, invariance = FALSE)
  }

  if (progress == TRUE) {
    print("Routine finished. Use the mg.tdcm.summary function to display results.", quote = FALSE)
    tdcm$progress <- TRUE
  }
  if (progress == FALSE) {
    tdcm$progress <- FALSE
  }
  # append group and time invariance
  newList <- list("group.invariance" = group.invariance, "item.invariance" = item.invariance)
  tdcm <- append(tdcm, newList)
  return(tdcm)
}
