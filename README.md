
<!-- README.md is generated from README.Rmd. Please edit that file -->

# tdcm

<!-- badges: start -->

[![R-CMD-check](https://github.com/cotterell/tdcm/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/cotterell/tdcm/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/cotterell/tdcm/branch/main/graph/badge.svg)](https://app.codecov.io/gh/cotterell/tdcm?branch=main)
<!-- badges: end -->

The R package **tdcm** lets users estimate the transition diagnostic
classification model (TDCM) described in [Madison & Bradshaw
(2018)](https://doi.org/10.1007/s11336-018-9638-5), a longitudinal
extension of the log-linear cognitive diagnosis model (LDCM) in [Henson,
Templin & Willse (2009)](https://doi.org/10.1007/s11336-008-9089-5). As
the LCDM subsumes many other diagnostic classification models (DCMs),
many other DCMs can be estimated longitudinally via the TDCM. The tdcm
package includes functions to estimate the single-group and multigroup
TDCM, summarize results of interest including item parameters, growth
proportions, transition probabilities, transitional reliability,
attribute correlations, model fit, and growth plots.

## Installation

You can install the development version of ‘tdcm’ like so:

``` r
# Install devtools from CRAN
install.packages("tdcm")
```

``` r
# Or the development version from GitHub:
# install.packages("devtools")
devtools::install_github("cotterell/tdcm")
```

## Getting Started

See `vignette("tdcm", package = "tdcm")` or for an overview of the
‘tdcm’ package.
