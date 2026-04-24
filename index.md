# TDCM

The ‘**TDCM**’ R package lets users estimate the transition diagnostic
classification model (TDCM) described in [Madison & Bradshaw
(2018)](https://doi.org/10.1007/s11336-018-9638-5), a longitudinal
extension of the log-linear cognitive diagnosis model (LCDM) in [Henson,
Templin & Willse (2009)](https://doi.org/10.1007/s11336-008-9089-5). As
the LCDM subsumes many other diagnostic classification models (DCMs),
many other DCMs can be estimated longitudinally via the TDCM framework.
The ‘TDCM’ package includes functions to estimate the single-group and
multigroup TDCM, summarize results of interest including item
parameters, growth proportions, transition probabilities, transitional
reliability, attribute correlations, model fit, and growth plots.

## Installation

You can install the ‘TDCM’ package like so:

``` r
# Install TDCM from CRAN
install.packages("TDCM")
```

``` r
# Install the latest development version of TDCM from GitHub:
if (!require("devtools")) install.packages("devtools")
devtools::install_github("cotterell/tdcm")
```

## Getting Started

See
[`vignette("TDCM", package = "TDCM")`](https://cotterell.github.io/tdcm/articles/TDCM.md)
or for an overview of the ‘TDCM’ package.
