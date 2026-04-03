# TDCM results compiler and summarizer.

Function to summarize results obtained with the [`tdcm`](tdcm.md)
function. It includes information regarding the item parameters,
attribute posterior probabilities, transition posterior probabilities,
attribute mastery classifications, growth, growth effects,transition
probabilities, attribute correlations, model fit statistics, and several
transition reliability metrics developed by Schellman and Madison
(2024).

## Usage

``` r
tdcm.summary(
  model,
  transition.option = 1,
  classthreshold = 0.5,
  attribute.names = c()
)
```

## Arguments

- model:

  A `tdcm` object returned from the [`tdcm`](tdcm.md) function.

- transition.option:

  An optional argument to specify how growth and transition
  probabilities should be reported for each attribute across time
  points.

  - `transition.option = 1` (default): Summarizes the transition
    probabilities by comparing the first and last time point.

  - `transition.option = 2`: Summarizes the transition by comparing the
    first time point to every subsequent time point.

  - `transition.option = 3`: Summarizes the transition probabilities by
    comparing each consecutive time point sequentially.

- classthreshold:

  A numeric value between 0 and 1 specifying the probability threshold
  for determining examinees' proficiency based on the posterior
  probabilities.

  - The default value is `.50`, which optimizes overall classification
    accuracy.

  - Lower values reduce the probability of false negatives, such that
    fewer proficient examinees are misclassified as non-proficient.

  - Higher values reduce the probability of false positives, such that
    fewer non-proficient examinees are misclassified as proficient.

- attribute.names:

  An optional character `vector` specifying the attribute names to be
  included in the results outputs. By default, `attribute.names=NULL`,
  which uses the generic attribute labels from the Q-matrix.

## Value

A list with the following items:

- `$item.parameters`: Item parameter estimates from the specified DCM.

- `$growth`: Proficiency proportions for each time point and each
  attribute.

- `$growth.effects`: It includes three growth effect size metrics for
  each attribute and specified transitions:

  1.  **Growth**: Difference in proficiency proportions between the
      later and earlier time point.

  2.  **Odds Ratio**: Ratio between the proficiency odds at the later
      time point and the proficiency odds at the earlier time point.

  3.  **Cohen's h** (Cohen, 1988): Arcsine-transformed difference in
      proficiency proportions.

  Note that the `growth.effect` output directly depend on the option
  specified in `transition.option`.

  *Example*:

  Suppose a test measures two attributes at three time points. Because
  there are more than two time points, the growth effect output is
  calculated based on the option specified in `transition.option`.

  - If `transition.option=1`, the growth effect for Attribute 1 and 2 is
    computed between Time Point 1 (first) and Time Point 3 (last).

  - If `transition.option=2`, the growth effect for Attribute 1 and 2 is
    computed between:

    - Time Point 1 (first) and Time Point 2 (latter).

    - Time Point 1 (first) and Time Point 3 (latter).

  - If `transition.option=3`, the growth effect for Attribute 1 and 2 is
    obtained between:

    - Time Point 1 (earlier) and Time Point 2 (next).

    - Time Point 2 (earlier), and Time Point 3 (next).

- `$transition.probabilities`: Conditional attribute proficiency
  transition probability matrices.

- `$posterior.probabilities`: Examinee marginal attribute posterior
  probabilities of proficiency.

- `$transition.posteriors`: Examinee marginal attribute transition
  posterior probabilities.

- `$most.likely.transitions`: Examinee most likely transitions for each
  attribute and transition.

- `$classifications`: Examinee classifications determined by the
  specified threshold applied to the posterior probabilities.

- `$reliability`: Estimated transition reliability metrics for each
  attribute for the specified transitions option specified (Madison,
  2019; Schellman & Madison, 2024). It includes seven metrics:

  - **pt bis**: Longitudinal point biserial metric, which reflects the
    ratio between the estimated attribute proficiency base rates with
    the attribute proficiency posterior probabilities.

  - **info gain**: Longitudinal information gain metric. It quantifies
    how much additional information is gained regarding an attribute's
    transition over time.

  - **polychor**: Longitudinal tetrachoric metric. It quantifies how
    consistently an examinee transitions between mastery states between
    two time points.

  - **ave max tr**: Average maximum transition posterior metric. It
    quantities how likely an examinee is classified into a specific
    transition state over time.

  - **P(t \> k)**: Proportion of examinees whose marginal attribute
    transition posteriors exceed a threshold *k*. The thresholds used
    are *k* = 0.6, 0.7, 0.8, and 0.9, representing the proportion of
    examinees with attribute transition posterior probabilities greater
    than these values. For example, if P(t\>.6) = 0.90, 90% of examinees
    have a posterior probability greater than 0.6.

  - **wt pt bis**: Weighted longitudinal point biserial. A variation of
    the longitudinal point biserial metric that computes the correlation
    between true attribute transition classification and observed
    marginal transition probabilities. It assigns greater weight to more
    prevalent attribute transitions based on each attributes' transition
    base rate, ensuring that transitions occurring more frequently in
    the data contribute more significantly to the computed reliability
    value.

  - **wt info gain**: Weighted longitudinal information gain. A
    variation of the longitudinal information gain that quantifies the
    additional information provided by the attribute transition
    posterior probabilities in predicting examinees' true transition
    status. It assigns greater weight to more prevalent attribute
    transitions, ensuring that transitions occurring more frequently in
    the data contribute more significantly to the computed reliability
    value.

- `$att.corr`: Estimated attribute correlation matrix.

- `$model.fit`: Several model fit indices and tests are output
  including:

  - Item root mean square error of approximation (RMSEA; von Davier,
    2005).

  - Mean item RMSEA.

  - Bivariate item fit statistics (Chen et al., 2013).

  - Absolute fit statistics such as mean absolute deviation for
    observed.

  - Expected item correlations (MADcor; DiBello, Roussos, & Stout,
    2007).

  - Standardized root mean square root of squared residuals (SRMSR;
    Maydeu-Olivares, 2013).

## References

Chen, J., de la Torre, J., & Zhang, Z. (2013). Relative and absolute fit
evaluation in cognitive diagnosis modeling. *Journal of Educational
Measurement, 50*, 123-140.

Cohen, J. (1988). *Statistical Power Analysis for the Behavioral
Sciences* (2nd ed.). Hillsdale, NJ: Lawrence Erlbaum Associates,
Publishers.

DiBello, L. V., Roussos, L. A., & Stout, W. F. (2007). *Review of
cognitively diagnostic assessment and a summary of psychometric models*.
In C. R. Rao and S. Sinharay (Eds.), Handbook of Statistics, Vol. 26
(pp.979–1030). Amsterdam: Elsevier.

Johnson, M. S., & Sinharay, S. (2020). The reliability of the posterior
probability of skill attainment in diagnostic classification models.
*Journal of Educational Measurement, 47*(1), 5 – 31.

Madison, M. J. (2019). Reliably assessing growth with longitudinal
diagnostic classification models. *Educational Measurement: Issues and
Practice, 38*(2), 68-78.

Maydeu-Olivares, A. (2013). Goodness-of-fit assessment of item response
theory models (with discussion). *Measurement: Interdisciplinary
Research and Perspectives, 11*, 71-137.

Schellman, M., & Madison, M. J. (2024). Estimating the reliability of
skill transition in longitudinal DCMs. *Journal of Educational and
Behavioral Statistics*.

Templin, J., & Bradshaw, L. (2013). Measuring the reliability of
diagnostic classification model examinee estimates. *Journal of
Classification, 30*, 251-275.

von Davier M. (2008). A general diagnostic model applied to language
testing data. *The British journal of mathematical and statistical
psychology, 61*(2), 287–307.

## Examples

``` r
# \donttest{
############################################################################
# Example 1: TDCM with full measurement invariance and equal Q-matrix
############################################################################

# Load data: T = 2, A = 4
data(data.tdcm01, package = "TDCM")
dat1 <- data.tdcm01$data
qmat1 <- data.tdcm01$q.matrix

# Estimate model

model1 <- TDCM::tdcm(dat1, qmat1, num.time.points = 2, invariance = TRUE, rule = "LCDM")
#> [1] Preparing data for tdcm()...
#> [1] Estimating the TDCM in tdcm()...
#> [1] Depending on model complexity, estimation time may vary...
#> [1] TDCM estimation complete.
#> [1] Use tdcm.summary() to display results.

# summarize results with tdcm.summary function
results1 <- TDCM::tdcm.summary(model1, transition.option = 1)
#> [1] Summarizing results...
#> [1] Routine finished. Check results.
results1$item.parameters
#>         λ0     λ1,1  λ1,2  λ1,3  λ1,4  λ2,12 λ2,13 λ2,14 λ2,23 λ2,24  λ2,34
#> Item 1  -1.905 2.599   --    --    --    --    --    --    --    --     -- 
#> Item 2  -2.072 2.536   --    --    --    --    --    --    --    --     -- 
#> Item 3  -1.934 2.517   --    --    --    --    --    --    --    --     -- 
#> Item 4  -1.892 1.091 1.499   --    --  1.057   --    --    --    --     -- 
#> Item 5  -2.17  1.456   --  1.794   --    --  1.018   --    --    --     -- 
#> Item 6  -1.843   --  2.199   --    --    --    --    --    --    --     -- 
#> Item 7  -1.825   --  2.259   --    --    --    --    --    --    --     -- 
#> Item 8  -1.967   --  2.497   --    --    --    --    --    --    --     -- 
#> Item 9  -2.009   --  1.079 1.511   --    --    --    --  1.818   --     -- 
#> Item 10 -2       --  1.849   --  1.324   --    --    --    --  1.065    -- 
#> Item 11 -1.845   --    --  2.329   --    --    --    --    --    --     -- 
#> Item 12 -2.033   --    --  2.539   --    --    --    --    --    --     -- 
#> Item 13 -2.071   --    --  2.55    --    --    --    --    --    --     -- 
#> Item 14 -2.093   --    --  1.739 2.031   --    --    --    --    --   0.496
#> Item 15 -1.785 0.307   --  1.295   --    --  2.374   --    --    --     -- 
#> Item 16 -2.218   --    --    --  2.837   --    --    --    --    --     -- 
#> Item 17 -2.084   --    --    --  2.69    --    --    --    --    --     -- 
#> Item 18 -2.101   --    --    --  2.521   --    --    --    --    --     -- 
#> Item 19 -2.1   2.653   --    --  1.432   --    --  0.098   --    --     -- 
#> Item 20 -2.061   --  2.545   --  1.53    --    --    --    --  -0.005   -- 
results1$growth
#>             T1[1] T2[1]
#> Attribute 1 0.190 0.370
#> Attribute 2 0.317 0.491
#> Attribute 3 0.392 0.579
#> Attribute 4 0.242 0.693
results1$growth.effects
#>             T1[1] T2[1] Growth Odds Ratio Cohen`s h
#> Attribute 1 0.190 0.370  0.180       2.50      0.41
#> Attribute 2 0.317 0.491  0.174       2.08      0.36
#> Attribute 3 0.392 0.579  0.187       2.13      0.38
#> Attribute 4 0.242 0.693  0.451       7.07      0.94
results1$transition.probabilities
#> , , Attribute 1: Time 1 to Time 2
#> 
#>        T2 [0] T2 [1]
#> T1 [0]  0.680  0.320
#> T1 [1]  0.417  0.583
#> 
#> , , Attribute 2: Time 1 to Time 2
#> 
#>        T2 [0] T2 [1]
#> T1 [0]  0.581  0.419
#> T1 [1]  0.353  0.647
#> 
#> , , Attribute 3: Time 1 to Time 2
#> 
#>        T2 [0] T2 [1]
#> T1 [0]  0.549  0.451
#> T1 [1]  0.221  0.779
#> 
#> , , Attribute 4: Time 1 to Time 2
#> 
#>        T2 [0] T2 [1]
#> T1 [0]  0.371  0.629
#> T1 [1]  0.104  0.896
#> 
results1$reliability
#>             pt bis info gain polychor ave max tr P(t>.6) P(t>.7) P(t>.8)
#> Attribute 1  0.821     0.516    0.936      0.931   0.966   0.927   0.861
#> Attribute 2  0.792     0.552    0.916      0.908   0.939   0.893   0.839
#> Attribute 3  0.770     0.540    0.922      0.895   0.943   0.870   0.796
#> Attribute 4  0.771     0.494    0.914      0.913   0.952   0.894   0.829
#>             P(t>.9) wt pt bis wt info gain
#> Attribute 1   0.790     0.834        0.601
#> Attribute 2   0.731     0.809        0.591
#> Attribute 3   0.674     0.786        0.584
#> Attribute 4   0.748     0.798        0.602
head(results1$most.likely.transitions)
#>   Attribute 1: T1 to T2 Attribute 2: T1 to T2 Attribute 3: T1 to T2
#> 1     01                    10                    01               
#> 2     10                    00                    10               
#> 3     00                    11                    01               
#> 4     00                    01                    01               
#> 5     00                    01                    01               
#> 6     00                    10                    01               
#>   Attribute 4: T1 to T2
#> 1     11               
#> 2     11               
#> 3     10               
#> 4     01               
#> 5     11               
#> 6     01               
results1$model.fit$Item.RMSEA
#>     Item 1     Item 2     Item 3     Item 4     Item 5     Item 6     Item 7 
#> 0.09391612 0.12079524 0.10670311 0.10952611 0.11962801 0.13655715 0.13845978 
#>     Item 8     Item 9    Item 10    Item 11    Item 12    Item 13    Item 14 
#> 0.10811876 0.11353405 0.11115225 0.12981641 0.11323978 0.10265758 0.11435661 
#>    Item 15    Item 16    Item 17    Item 18    Item 19    Item 20    Item 21 
#> 0.12122112 0.12147005 0.10578848 0.11120378 0.09767873 0.13304620 0.10788168 
#>    Item 22    Item 23    Item 24    Item 25    Item 26    Item 27    Item 28 
#> 0.10949474 0.11713454 0.12149082 0.11334556 0.12767058 0.12317678 0.10590232 
#>    Item 29    Item 30    Item 31    Item 32    Item 33    Item 34    Item 35 
#> 0.11158355 0.11326936 0.11504822 0.11948474 0.11920146 0.09564141 0.12998822 
#>    Item 36    Item 37    Item 38    Item 39    Item 40 
#> 0.11363849 0.12522381 0.11581421 0.10939110 0.11244670 

############################################################################
# Example 2: TDCM with full measurement invariance and different Q-matrices
############################################################################

# Load dataset: T=3, A=2
data(data.tdcm03, package = "TDCM")
data <- data.tdcm03$data
q1 <- data.tdcm03$q.matrix.1
q2 <- data.tdcm03$q.matrix.2
q3 <- data.tdcm03$q.matrix.3
q <- data.tdcm03$q.matrix.stacked

# Estimate model
model2 <- TDCM::tdcm(data, q, num.time.points = 3,
                    rule = "LCDM",
                    num.q.matrix = 3,
                    num.items = c(10,10,10))
#> [1] Preparing data for tdcm()...
#> [1] Estimating the TDCM in tdcm()...
#> [1] Depending on model complexity, estimation time may vary...
#> [1] TDCM estimation complete.
#> [1] Use tdcm.summary() to display results.

#----------------------------------------------------------------------------
# With different transition options
#----------------------------------------------------------------------------

## a) If transition.option = 1

results2_option1 <- TDCM::tdcm.summary(model2)
#> [1] Summarizing results...
#> [1] Routine finished. Check results.
results2_option1$transition.probabilities
#> , , Attribute 1: Time 1 to Time 3
#> 
#>        T3 [0] T3 [1]
#> T1 [0]  0.202  0.798
#> T1 [1]  0.146  0.854
#> 
#> , , Attribute 2: Time 1 to Time 3
#> 
#>        T3 [0] T3 [1]
#> T1 [0]  0.325  0.675
#> T1 [1]  0.257  0.743
#> 
#, , Attribute 1: Time 1 to Time 3
#
#       T3 [0] T3 [1]
#T1 [0]  0.202  0.798
#T1 [1]  0.146  0.854
#
#, , Attribute 2: Time 1 to Time 3
#
#       T3 [0] T3 [1]
#T1 [0]  0.325  0.675
#T1 [1]  0.257  0.743

results2_option1$reliability
#>             pt bis info gain polychor ave max tr P(t>.6) P(t>.7) P(t>.8)
#> Attribute 1  0.550     0.387    0.737      0.830   0.888   0.780   0.643
#> Attribute 2  0.665     0.474    0.808      0.851   0.899   0.801   0.694
#>             P(t>.9) wt pt bis wt info gain
#> Attribute 1   0.452     0.594        0.474
#> Attribute 2   0.536     0.681        0.525
#            pt bis info gain polychor ave max tr P(t>.6) P(t>.7) P(t>.8)
#Attribute 1  0.550     0.387    0.737      0.830   0.888   0.780   0.643
#Attribute 2  0.665     0.474    0.808      0.851   0.899   0.801   0.694

# b) If transition.option = 2

# Summary with transition.option = 2
results2_option2 <- TDCM::tdcm.summary(model2, transition.option = 2)
#> [1] Summarizing results...
#> [1] Routine finished. Check results.
results2_option2$transition.probabilities
#> , , Attribute 1: Time 1 to Time 2
#> 
#>       [0]   [1]
#> [0] 0.510 0.490
#> [1] 0.424 0.576
#> 
#> , , Attribute 2: Time 1 to Time 2
#> 
#>       [0]   [1]
#> [0] 0.456 0.544
#> [1] 0.334 0.666
#> 
#> , , Attribute 1: Time 1 to Time 3
#> 
#>       [0]   [1]
#> [0] 0.202 0.798
#> [1] 0.146 0.854
#> 
#> , , Attribute 2: Time 1 to Time 3
#> 
#>       [0]   [1]
#> [0] 0.325 0.675
#> [1] 0.257 0.743
#> 
#, , Attribute 1: Time 1 to Time 2
#
#.    [0]   [1]
#[0] 0.510 0.490
#[1] 0.424 0.576
#
#, , Attribute 2: Time 1 to Time 2
#
#     [0]   [1]
#[0] 0.456 0.544
#[1] 0.334 0.666
#
#, , Attribute 1: Time 1 to Time 3
#
#     [0]   [1]
#[0] 0.202 0.798
#[1] 0.146 0.854
#
#, , Attribute 2: Time 1 to Time 3
#
#     [0]   [1]
#[0] 0.325 0.675
#[1] 0.257 0.743

results2_option2$reliability
#> , , T1 to T2
#> 
#>             pt bis info gain polychor ave max tr P(t>.6) P(t>.7) P(t>.8)
#> Attribute 1  0.586     0.444    0.770      0.796   0.828   0.710   0.581
#> Attribute 2  0.692     0.503    0.838      0.853   0.885   0.799   0.713
#>             P(t>.9) wt pt bis wt info gain
#> Attribute 1   0.372     0.594        0.472
#> Attribute 2   0.570     0.707        0.540
#> 
#> , , T1 to T3
#> 
#>             pt bis info gain polychor ave max tr P(t>.6) P(t>.7) P(t>.8)
#> Attribute 1  0.550     0.387    0.737      0.830   0.888   0.780   0.643
#> Attribute 2  0.665     0.474    0.808      0.851   0.899   0.801   0.694
#>             P(t>.9) wt pt bis wt info gain
#> Attribute 1   0.452     0.594        0.474
#> Attribute 2   0.536     0.681        0.525
#> 
#, , T1 to T2
#
#.           pt bis info gain polychor ave max tr P(t>.6) P(t>.7) P(t>.8)
#Attribute 1  0.586     0.444    0.770      0.796   0.828   0.710   0.581
#Attribute 2  0.692     0.503    0.838      0.853   0.885   0.799   0.713
#
#, , T1 to T3
#
#.           pt bis info gain polychor ave max tr P(t>.6) P(t>.7) P(t>.8)
#Attribute 1  0.550     0.387    0.737      0.830   0.888   0.780   0.643
#Attribute 2  0.665     0.474    0.808      0.851   0.899   0.801   0.694

## c) If transition.option = 3

results2_option3 <- TDCM::tdcm.summary(model2, transition.option = 3)
#> [1] Summarizing results...
#> [1] Routine finished. Check results.
results2_option3$transition.probabilities
#> , , Attribute 1: Time 1 to Time 2
#> 
#>       [0]   [1]
#> [0] 0.510 0.490
#> [1] 0.424 0.576
#> 
#> , , Attribute 2: Time 1 to Time 2
#> 
#>       [0]   [1]
#> [0] 0.456 0.544
#> [1] 0.334 0.666
#> 
#> , , Attribute 1: Time 2 to Time 3
#> 
#>       [0]   [1]
#> [0] 0.183 0.817
#> [1] 0.188 0.812
#> 
#> , , Attribute 2: Time 2 to Time 3
#> 
#>       [0]   [1]
#> [0] 0.361 0.639
#> [1] 0.262 0.738
#> 
#, , Attribute 1: Time 1 to Time 2
#
#    [0]   [1]
#[0] 0.510 0.490
#[1] 0.424 0.576
#
#, , Attribute 2: Time 1 to Time 2
#
#     [0]   [1]
#[0] 0.456 0.544
#[1] 0.334 0.666
#
#, , Attribute 1: Time 2 to Time 3
#
#     [0]   [1]
#[0] 0.183 0.817
#[1] 0.188 0.812
#
#, , Attribute 2: Time 2 to Time 3
#
#     [0]   [1]
#[0] 0.361 0.639
#[1] 0.262 0.738

results2_option3$reliability
#> , , T1 to T2
#> 
#>             pt bis info gain polychor ave max tr P(t>.6) P(t>.7) P(t>.8)
#> Attribute 1  0.586     0.444    0.770      0.796   0.828   0.710   0.581
#> Attribute 2  0.692     0.503    0.838      0.853   0.885   0.799   0.713
#>             P(t>.9) wt pt bis wt info gain
#> Attribute 1   0.372     0.594        0.472
#> Attribute 2   0.570     0.707        0.540
#> 
#> , , T2 to T3
#> 
#>             pt bis info gain polychor ave max tr P(t>.6) P(t>.7) P(t>.8)
#> Attribute 1  0.537     0.396    0.724      0.801   0.841   0.724   0.578
#> Attribute 2  0.691     0.502    0.861      0.853   0.880   0.799   0.714
#>             P(t>.9) wt pt bis wt info gain
#> Attribute 1   0.387     0.575        0.470
#> Attribute 2   0.568     0.705        0.535
#> 
#, , T1 to T2
#
#.           pt bis info gain polychor ave max tr P(t>.6) P(t>.7) P(t>.8)
#Attribute 1  0.586     0.444    0.770      0.796   0.828   0.710   0.581
#Attribute 2  0.692     0.503    0.838      0.853   0.885   0.799   0.713
#
#, , T2 to T3
#
#.            pt bis info gain polychor ave max tr P(t>.6) P(t>.7) P(t>.8)
#Attribute 1  0.537     0.396    0.724      0.801   0.841   0.724   0.578
#Attribute 2  0.691     0.502    0.861      0.853   0.880   0.799   0.714

#----------------------------------------------------------------------------
# With different thresholds
#----------------------------------------------------------------------------

## a) If classthreshold = 0.5 (default)

results2_1 <- TDCM::tdcm.summary(model2)
#> [1] Summarizing results...
#> [1] Routine finished. Check results.
head(results2_1$posterior.probabilities)
#>    T1A1  T1A2  T2A1  T2A2  T3A1  T3A2
#> 1 0.068 0.882 0.961 0.967 1.000 1.000
#> 2 0.001 0.010 0.845 0.749 0.070 0.402
#> 3 0.005 0.683 0.816 0.395 0.987 0.133
#> 4 0.007 0.988 0.996 0.998 0.993 0.997
#> 5 0.001 0.000 0.205 0.019 0.999 0.814
#> 6 0.011 0.001 0.630 0.004 0.900 0.077
#    T1A1  T1A2  T2A1  T2A2  T3A1  T3A2
# 1 0.068 0.882 0.961 0.967 1.000 1.000
# 2 0.001 0.010 0.845 0.749 0.070 0.402
# 3 0.005 0.683 0.816 0.395 0.987 0.133
# 4 0.007 0.988 0.996 0.998 0.993 0.997
# 5 0.001 0.000 0.205 0.019 0.999 0.814
# 6 0.011 0.001 0.630 0.004 0.900 0.077

head(results2_1$classifications)
#>   T1A1 T1A2 T2A1 T2A2 T3A1 T3A2
#> 1    0    1    1    1    1    1
#> 2    0    0    1    1    0    0
#> 3    0    1    1    0    1    0
#> 4    0    1    1    1    1    1
#> 5    0    0    0    0    1    1
#> 6    0    0    1    0    1    0
#   T1A1 T1A2 T2A1 T2A2 T3A1 T3A2
# 1    0    1    1    1    1    1
# 2    0    0    1    1    0    0
# 3    0    1    1    0    1    0
# 4    0    1    1    1    1    1
# 5    0    0    0    0    1    1
# 6    0    0    1    0    1    0

## b) If classthreshold = 0.7

results2_2 <- TDCM::tdcm.summary(model2, classthreshold = 0.7)
#> [1] Summarizing results...
#> [1] Routine finished. Check results.
head(results2_2$posterior.probabilities)
#>    T1A1  T1A2  T2A1  T2A2  T3A1  T3A2
#> 1 0.068 0.882 0.961 0.967 1.000 1.000
#> 2 0.001 0.010 0.845 0.749 0.070 0.402
#> 3 0.005 0.683 0.816 0.395 0.987 0.133
#> 4 0.007 0.988 0.996 0.998 0.993 0.997
#> 5 0.001 0.000 0.205 0.019 0.999 0.814
#> 6 0.011 0.001 0.630 0.004 0.900 0.077
#    T1A1  T1A2  T2A1  T2A2  T3A1  T3A2
# 1 0.068 0.882 0.961 0.967 1.000 1.000
# 2 0.001 0.010 0.845 0.749 0.070 0.402
# 3 0.005 0.683 0.816 0.395 0.987 0.133
# 4 0.007 0.988 0.996 0.998 0.993 0.997
# 5 0.001 0.000 0.205 0.019 0.999 0.814
# 6 0.011 0.001 0.630 0.004 0.900 0.077

head(results2_2$classifications)
#>   T1A1 T1A2 T2A1 T2A2 T3A1 T3A2
#> 1    0    1    1    1    1    1
#> 2    0    0    1    1    0    0
#> 3    0    0    1    0    1    0
#> 4    0    1    1    1    1    1
#> 5    0    0    0    0    1    1
#> 6    0    0    0    0    1    0
#   T1A1 T1A2 T2A1 T2A2 T3A1 T3A2
# 1    0    1    1    1    1    1
# 2    0    0    1    1    0    0
# 3    0    0    1    0    1    0
# 4    0    1    1    1    1    1
# 5    0    0    0    0    1    1
# 6    0    0    0    0    1    0

## c) If classthreshold = 0.3

results2_3 <- TDCM::tdcm.summary(model2, classthreshold = 0.3)
#> [1] Summarizing results...
#> [1] Routine finished. Check results.
head(results2_3$posterior.probabilities)
#>    T1A1  T1A2  T2A1  T2A2  T3A1  T3A2
#> 1 0.068 0.882 0.961 0.967 1.000 1.000
#> 2 0.001 0.010 0.845 0.749 0.070 0.402
#> 3 0.005 0.683 0.816 0.395 0.987 0.133
#> 4 0.007 0.988 0.996 0.998 0.993 0.997
#> 5 0.001 0.000 0.205 0.019 0.999 0.814
#> 6 0.011 0.001 0.630 0.004 0.900 0.077
#    T1A1  T1A2  T2A1  T2A2  T3A1  T3A2
# 1 0.068 0.882 0.961 0.967 1.000 1.000
# 2 0.001 0.010 0.845 0.749 0.070 0.402
# 3 0.005 0.683 0.816 0.395 0.987 0.133
# 4 0.007 0.988 0.996 0.998 0.993 0.997
# 5 0.001 0.000 0.205 0.019 0.999 0.814
# 6 0.011 0.001 0.630 0.004 0.900 0.077

head(results2_3$classifications)
#>   T1A1 T1A2 T2A1 T2A2 T3A1 T3A2
#> 1    0    1    1    1    1    1
#> 2    0    0    1    1    0    1
#> 3    0    1    1    1    1    0
#> 4    0    1    1    1    1    1
#> 5    0    0    0    0    1    1
#> 6    0    0    1    0    1    0
#   T1A1 T1A2 T2A1 T2A2 T3A1 T3A2
# 1    0    1    1    1    1    1
# 2    0    0    1    1    0    1
# 3    0    1    1    1    1    0
# 4    0    1    1    1    1    1
# 5    0    0    0    0    1    1
# 6    0    0    1    0    1    0
# }
```
