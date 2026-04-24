# Comparing the fit of two TDCMs

Provides a comparison of two TDCMs. Can be used to compare different
measurement models or assess measurement invariance over time or over
groups in the multigroup TDCM case. Only accepts two models.

## Usage

``` r
tdcm.compare(model1, model2)
```

## Arguments

- model1:

  a `gdina` object returned from the
  [`tdcm`](https://cotterell.github.io/tdcm/dev/reference/tdcm.md) or
  [`mg.tdcm`](https://cotterell.github.io/tdcm/dev/reference/mg.tdcm.md)
  function.

- model2:

  a second `gdina` object returned from the
  [`tdcm`](https://cotterell.github.io/tdcm/dev/reference/tdcm.md) or
  [`mg.tdcm`](https://cotterell.github.io/tdcm/dev/reference/mg.tdcm.md)
  function

## Value

This function returns a data frame with model fit statistics (AIC/BIC)
and results from a likelihood ratio or deviance test.

## Note

- Currently, this function currently accepts two models for comparison.

- Both models must be fit to the same item responses and Q-matrix.

- The function will provide results for two non-nested models. Please
  ensure that models are nested before interpreting the likelihood ratio
  test for nested models.

- The likelihood ratio test is not valid for some model comparisons
  (e.g., LCDM vs DINA) because of model constraints.

## Examples

``` r
# \donttest{
## Example 1: T = 2, A = 4
data(data.tdcm01, package = "TDCM")
dat1 <- data.tdcm01$data
qmat1 <- data.tdcm01$q.matrix

# estimate TDCM with invariance assumed and full LCDM
m1 <- TDCM::tdcm(dat1, qmat1, num.time.points = 2, invariance = TRUE, rule = "LCDM")
#> [1] Preparing data for tdcm()...
#> [1] Estimating the TDCM in tdcm()...
#> [1] Depending on model complexity, estimation time may vary...
#> [1] TDCM estimation complete.
#> [1] Use tdcm.summary() to display results.

# estimate TDCM with invariance not assumed
m2 <- TDCM::tdcm(dat1, qmat1, num.time.points = 2, invariance = FALSE, rule = "LCDM")
#> [1] Preparing data for tdcm()...
#> [1] Estimating the TDCM in tdcm()...
#> [1] Depending on model complexity, estimation time may vary...
#> [1] TDCM estimation complete.
#> [1] Use tdcm.summary() to display results.

# compare models to assess measurement invariance.
TDCM::tdcm.compare(m1, m2)
#>   Model   loglike Deviance Npars      AIC      BIC Chisq df      p
#> 1    m1 -21369.72 42739.44   311 43361.44 44887.75 64.68 56 0.1995
#> 2    m2 -21337.38 42674.75   367 43408.75  45209.9    NA NA     NA
# }
```
