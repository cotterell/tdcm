#' The Transition Diagnostic Classification Model Framework
#'
#' The _R_ package \pkg{TDCM} enables longitudinal diagnostic classification model (DCM) analyses
#' within the transition diagnostic classification model (TDCM) framework.
#'
#' @details Diagnostic classification models (DCMs) are psychometric models designed to classify
#' examinees according to their proficiency on a set of categorical latent traits, known as
#' attributes. Longitudinal DCMs have been developed as psychometric options for modeling changes
#' in attribute proficiency over time.
#'
#' The \pkg{TDCM} implements estimation of the transition DCM (TDCM; Madison & Bradshaw, 2018a),
#' a longitudinal extension of the log-linear cognitive diagnosis model (LCDM; Henson, Templin, &
#' Willse, 2009). As the LCDM subsumes many other DCMs, many other DCMs can be estimated
#' longitudinally via the TDCM. The package includes functions to estimate the single-group and
#' multigroup TDCM, summarize results of interest including item parameters, growth proportions,
#' transition probabilities, transitional reliability, attribute correlations, model fit, and
#' growth plots.
#'
#' For more details and examples, see \code{vignette("TDCM", package = "TDCM")}.
#'
#' @author
#' The \pkg{TDCM} package was created by:
#' * Matthew J. Madison, University of Georgia, mjmadison@uga.edu
#' * Sergio Haab, University of Iowa, sergio-haab@uiowa.edu
#' * Minjeong Jeon, University of California - Los Angeles, mjjeon@ucla.edu
#' * Michael E. Cotterell, University of Georgia, mepcott@uga.edu
#'
#' More information about the \pkg{TDCM} package authors, copyright holders, funders, etc. is
#' available in the package's `DESCRIPTION` file. You can see this information by executing
#' `utils::packageDescription("TDCM")`.
#'
#' @references
#' de la Torre, J. (2011). The Generalized DINA Model Framework. _Psychometrika_ **76**, 179–199.
#' <doi:10.1007/s11336-011-9207-7>
#'
#' George, A. C., Robitzsch, A., Kiefer, T., Gross, J., & Ünlü , A. (2016). The _R_ package **CDM**
#' for Cognitive Diagnosis Models. _Journal of Statistical Software_, **74**(2), 1-24.
#' <doi:10.18637/jss.v074.i02>
#'
#' Henson, R., Templin, J., & Willse, J. (2009). Defining a Family of Cognitive Diagnosis Models
#' Using Log-Linear Models with Latent Variables. _Psychometrika_, **74**, 191-21.
#' <doi:10.1007/s11336-008-9089-5>
#'
#' Johnson, M. S., & Sinharay, S. (2020). The Reliability of the Posterior Probability of Skill
#' Attainment in Diagnostic Classification Models. _Journal of Educational Measurement_, **47**(1),
#' 5–31. <doi:10.3102/1076998619864550>
#'
#' Kaya, Y., & Leite, W. (2017). Assessing Change in Latent Skills Across Time With Longitudinal
#' Cognitive Diagnosis Modeling: An Evaluation of Model Performance.
#' _Educational and Psychological Measurement_, **77**(3), 369–388.
#' <doi:10.1177/0013164416659314>
#'
#' Li, F., Cohen, A., Bottge, B., & Templin, J. (2015). A Latent Transition Analysis Model for
#' Assessing Change in Cognitive Skills. _Educational and Psychological Measurement_, **76**(2),
#' 181–204.
#' <doi:10.1177/0013164415588946>
#'
#' Madison, M. J. (2019). Reliably Assessing Growth with Longitudinal Diagnostic Classification
#' Models. _Educational Measurement: Issues and Practice_, **38**(2), 68-78.
#' <doi:10.1111/emip.12243>
#'
#' Madison, M. J., & Bradshaw, L. (2018a). Assessing Growth in a Diagnostic Classification Model
#' Framework. _Psychometrika_, **83**(4), 963-990.
#' <doi:10.1007/s11336-018-9638-5>
#'
#' Madison, M. J., & Bradshaw, L. (2018b). Evaluating Intervention Effects in a Diagnostic
#' Classification Model Framework. _Journal of Educational Measurement_, **55**(1), 32-51.
#' <doi:10.1111/jedm.12162>
#'
#' Madison, M.J., Chung, S., Kim, J., & Bradshaw, L.P. (2024) Approaches to estimating longitudinal
#' diagnostic classification models. _Behaviormetrika_, **51**(7), 7-19.
#' <doi:10.1007/s41237-023-00202-5>
#'
#' Rupp, A. A., Templin, J., & Henson, R. (2010).
#' _Diagnostic Measurement: Theory, Methods, and Applications_. New York: Guilford.
#' ISBN: 9781606235430.
#'
#' Schellman, M., & Madison, M. J. (2024). Estimating the reliability of skill transition in longitudinal DCMs.
#'  _Journal of Educational and Behavioral Statistics_.
#'
#' Templin, J., & Bradshaw, L. (2013). Measuring the Reliability of Diagnostic Classification Model
#' Examinee Estimates. _Journal of Classification_, **30**, 251-275.
#' <doi:10.1007/s00357-013-9129-4>
#'
#' Wang. S., Yang. Y., Culpepper, S. A., & Douglas, J. (2018). Tracking Skill Acquisition With
#' Cognitive Diagnosis Models: A Higher-Order, Hidden Markov Model With Covariates.
#' _Journal of Educational and Behavioral Statistics_, **43**(1), 57-87.
#' <doi:10.3102/1076998617719727>
#'
#' @keywords internal
#' @aliases TDCM-package NULL
#' @docType package
#' @rdname TDCM-package
"_PACKAGE"

## usethis namespace: start
## usethis namespace: end
NULL
