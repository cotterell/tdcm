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
#' same values as `rule` in [TDCM::tdcm()]: "LCDM", "DINA", "DINO", "CRUM", "RRUM", "LCDM1" for
#' the LCDM with only main effects, "LCDM2" for the LCDM with two-way interactions, "LCDM3",
#' and so on. If `rule` is supplied as a single string, then that DCM will be assumed for each item.
#' If entered as a vector, a rule can be specified for each item.
#'
#' @param linkfct A string indicating the LCDM link function. Currently accepts
#' "logit" (default) to estimate the LCDM. Can be specified "identity" to estimate the
#' GDINA model. Also accepts a "log" link function.
#'
#' @param groups A required vector of integer group identifiers (e.g., 1, 2, 3) for multiple group estimation.
#'
#' @param forget.att An optional vector allowing for constraining of individual attribute proficiency
#' loss, or forgetting. The default allows forgetting for each measured attribute
#' (e.g., \eqn{P(1 \rightarrow 0) \neq 0}). This vector is specified to indicate the attributes for which
#' forgetting is not permitted.
#'
#' @param group.invariance logical. If `TRUE` (default), item parameters are assumed
#' to be equal for all groups. If `FALSE`, item parameters are not assumed to be equal for
#' all groups.
#'
#' @param time.invariance logical. If `TRUE` (default), item parameters are assumed
#' to be equal for all time points. If `FALSE`, item parameters are not assumed to be
#' equal for all time points.
#'
#' @param progress logical. If `FALSE`, the function will print the progress of
#' estimation. If `TRUE` (default), no progress information is printed.
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
#' results <- TDCM::mg.tdcm.summary(mg.model)
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
    rule = "LCDM",
    linkfct = "logit",
    groups,
    forget.att = c(),
    group.invariance = TRUE,
    time.invariance = TRUE,
    progress = TRUE
) {

  #translate rule argument
  if(rule == "LCDM"){rule = "GDINA"}
  else if(rule == "CRUM"){rule = "ACDM"}
  else if(rule == "DINA"){rule = "DINA"}
  else if(rule == "DINO"){rule = "DINO"}
  else if(rule == "RRUM"){rule = "RRUM"}
  else if(rule == "LCDM1"){rule = "GDINA1"}
  else if(rule == "LCDM2"){rule = "GDINA2"}
  else if(rule == "LCDM3"){rule = "GDINA3"}
  else if(rule == "LCDM4"){rule = "GDINA4"}
  else if(rule == "LCDM5"){rule = "GDINA5"}
  else if(rule == "LCDM6"){rule = "GDINA6"}
  else if(rule == "LCDM7"){rule = "GDINA7"}
  else if(rule == "LCDM8"){rule = "GDINA8"}
  else if(rule == "LCDM9"){rule = "GDINA9"}
  else if(rule == "LCDM10"){rule = "GDINA10"}


  if (progress) {
    print("Preparing data for mg.tdcm()...", quote = FALSE)
  } # if

  # Initial Data Sorting
  n.items <- ncol(data) # Total Items
  items <- n.items / num.time.points # Items per time point
  N <- nrow(data) # Number of Examinees
  n.att <- ncol(q.matrix) # Number of Attributes
  group.invariance <- group.invariance
  time.invariance <- time.invariance
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


  # Case 1: all invariance
  if (group.invariance == TRUE & time.invariance == TRUE) {
    # base model, 1 iteration for design matrix
    tdcm.1 <- CDM::gdina(data, qnew,
                         linkfct = linkfct, method = "ML", mono.constr = TRUE,
                         group = groups, progress = FALSE, maxit = 1, rule = rule)

    # build design matrix
    c0 <- tdcm.1$coef
    c.0 <- nrow(c0)
    designmatrix <- diag(nrow = c.0 / num.time.points, ncol = c.0 / num.time.points)
    delta.designmatrix <- matrix(rep(t(designmatrix), num.time.points), ncol = ncol(designmatrix), byrow = TRUE)

    # estimate mg tdcm
    tdcm <- CDM::gdina(data, qnew,
                       group = groups, linkfct = linkfct, method = "ML", skillclasses = red.space,
                       delta.designmatrix = delta.designmatrix, rule = rule, reduced.skillspace = FALSE,
                       progress = FALSE)
  }

  # Case 2: group invariance, no time invariance
  else if (group.invariance == TRUE & time.invariance == FALSE) {
    # estimate mg tdcm
    tdcm <- CDM::gdina(data, qnew,
                       group = groups, linkfct = linkfct, method = "ML", progress = FALSE,
                       rule = rule, skillclasses = red.space, reduced.skillspace = FALSE)
  }

  # Case 3: time invariance, no group invariance
  else if (group.invariance == FALSE & time.invariance == TRUE) {
    # base model, 1 iteration for design matrix
    tdcm.1 <- CDM::gdina(data, qnew,
                         linkfct = linkfct, method = "ML", mono.constr = TRUE,skillclasses = red.space,
                         group = groups, progress = FALSE, maxit = 1, rule = rule, reduced.skillspace = FALSE,
                         invariance = FALSE)

    # build design matrix
    c0 <- tdcm.1$coef
    c.0 <- nrow(c0)
    designmatrix <- diag(nrow = c.0 / num.time.points, ncol = c.0 / num.time.points)
    delta.designmatrix <- matrix(rep(t(designmatrix), num.time.points), ncol = ncol(designmatrix), byrow = TRUE)

    # estimate mg tdcm
    tdcm <- CDM::gdina(data, qnew,
                       group = groups, linkfct = linkfct, method = "ML", progress = FALSE,skillclasses = red.space,
                       delta.designmatrix = delta.designmatrix, rule = rule,
                       reduced.skillspace=FALSE, invariance = FALSE)
  }

  # Case 4: no group or time invariance
  else {
    # estimate mg tdcm
    tdcm <- CDM::gdina(data, qnew,
                       group = groups, linkfct = linkfct, method = "ML", progress = FALSE,
                       skillclasses = red.space, rule = rule, reduced.skillspace = FALSE,
                       invariance = FALSE)
  }

  tdcm$group.invariance <- group.invariance
  tdcm$time.invariance <- time.invariance

  # set progress value in result object
  tdcm$progress <- progress

  #save number of time points
  tdcm$numtimepoints = num.time.points

  if (progress) {
    print("Multigroup TDCM estimation complete.", quote = FALSE)
    print("Use mg.tdcm.summary() to display results.", quote = FALSE)

  } # if

  return(tdcm)
}
