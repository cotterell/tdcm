# Plotting TDCM Results

`tdcm.plot()` visualizes the results from TDCM analyses.

## Usage

``` r
tdcm.plot(results, attribute.names = c(), group.names = c(), type = "both")
```

## Arguments

- results:

  results from
  [`tdcm.summary`](https://cotterell.github.io/tdcm/dev/reference/tdcm.summary.md)
  or
  [`mg.tdcm.summary`](https://cotterell.github.io/tdcm/dev/reference/mg.tdcm.summary.md)

- attribute.names:

  an optional vector of attribute names to include in plots.

- group.names:

  an optional vector of group names to include in plots.

- type:

  an option to specify the type of plot in single group cases; "both" is
  default and will produce a line plot and a bar chart; "line" will
  produce a line plot; and "bar" will produce a bar chart.

## Value

No return value, called for side effects.

## Examples

``` r
# \donttest{
## Example 1: T = 2, A = 4
data(data.tdcm01, package = "TDCM")
dat1 = data.tdcm01$data
qmat1 = data.tdcm01$q.matrix

#estimate TDCM with invariance assumed and full LCDM
m1 = TDCM::tdcm(dat1, qmat1, num.time.points = 2, invariance = TRUE, rule = "LCDM")
#> [1] Preparing data for tdcm()...
#> [1] Estimating the TDCM in tdcm()...
#> [1] Depending on model complexity, estimation time may vary...
#> [1] TDCM estimation complete.
#> [1] Use tdcm.summary() to display results.

#summarize results with tdcm.summary function
results1 = TDCM::tdcm.summary(m1)
#> [1] Summarizing results...
#> [1] Routine finished. Check results.

#plot results
TDCM::tdcm.plot(results1, attribute.names = c("Addition", "Subtraction",
"Multiplication", "Division"))


#> [1] **Check the plots window for line and bar plots of growth proportions.
# }
```
