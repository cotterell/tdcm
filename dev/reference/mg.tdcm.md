# Estimating the multigroup transition diagnostic classification model (TDCM)

`mg.tdcm()` estimates the Transition Diagnostic Classification Model for
scenarios involving multiple groups (e.g., control and treatment group;
Madison & Bradshaw, 2018b). Similar to
[`tdcm()`](https://cotterell.github.io/tdcm/dev/reference/tdcm.md), this
function supports the estimation of various DCMs by allowing different
rule specifications via the `rule` option and link functions via the
`linkfct` option,with LCDM as the default rule and link function. The
rule can be modified to estimate the DINA model, DINO model, CRUM (i.e.,
ACDM, or main effects model), or reduced interaction versions of the
LCDM. Additionally, the link function can be adjusted to specify the
GDINA model.

## Usage

``` r
mg.tdcm(
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
)
```

## Arguments

- data:

  A required \\N \times T \times I\\ `matrix` or `data.frame` where rows
  correspond to `N` examinees and columns represent the binary item
  responses across `T` time points and `I` items.

- q.matrix:

  A required \\I \times A\\ `matrix` indicating which items measure
  which attributes. Currently, the function only accepts a single
  Q-matrix.

- num.time.points:

  A required integer \\\ge 2\\ specifying the number of time points
  (i.e., measurement occasions).

- rule:

  A `string` or a `vector` indicating the specific DCM to be employed. A
  vector of supported `rule` values is provided by
  [tdcm.rules](https://cotterell.github.io/tdcm/dev/reference/tdcm.rules.md).
  Currently accepted values are: "LCDM", "DINA", "DINO", "CRUM", "RRUM",
  "LCDM1" for the LCDM with only main effects, "LCDM2" for the LCDM with
  two-way interactions, "LCDM3", and so on. If `rule` is supplied as a
  single string, then that DCM will be assumed for each item. If entered
  as a vector, a rule can be specified for each item. The vector must
  have length equal to the total number of items across all time points.

- linkfct:

  A `string` or a `vector` indicating the LCDM link function. Currently
  accepts "logit" (default) to estimate the LCDM, "identity" to estimate
  the GDINA model, and "log" link function to estimate the reduced
  reparameterized unified model (RRUM). The vector must have length
  equal to the total number of items across all time points.

- groups:

  A required `vector` of integer group identifiers for multiple group
  estimation.

- forget.att:

  An optional `vector` allowing for constraining of individual attribute
  proficiency loss, or forgetting. The default allows forgetting for
  each measured attribute (e.g., \\P(1 \rightarrow 0) \neq 0\\). See
  [tdcm](https://cotterell.github.io/tdcm/dev/reference/tdcm.md) for
  more detailed information.

- group.invariance:

  logical argument. If `TRUE` (default), item parameters are assumed to
  be equal for all groups. If `FALSE`, item parameters are not assumed
  to be equal for all groups.

- time.invariance:

  logical argument. If `TRUE` (default), item parameters are assumed to
  be equal for all time points. If `FALSE`, item parameters are not
  assumed to be equal for all time points.

- progress:

  logical argument. If `FALSE`, the function will print the progress of
  estimation. If `TRUE` (default), no progress information is printed.

## Value

An object of class `gdina` with entries as indicated in the CDM package.
For the TDCM-specific results (e.g., growth, transitions), use
[`TDCM::mg.tdcm.summary()`](https://cotterell.github.io/tdcm/dev/reference/mg.tdcm.summary.md).

## Details

**Multigroup Transition Diagnostic Classification Model (Multigroup
TDCM)**

Multigroup TDCM is a confirmatory latent transition model that measures
examinees' growth or decline in attribute mastery over time among groups
(Madison & Bradshaw, 2018b). In this model, the probability of the item
response vector \\X_e\\ is conditioned on observed groups membership
\\G\\:

\$\$ P(X_e = x_e\|G=g) = \sum\_{c_1=1}^{C} \sum\_{c_2=1}^{C} \cdots
\sum\_{c_T=1}^{C} v\_{c_1\|g} \tau\_{c_2 \| c\_{1},g} \tau\_{c_3 \|
c\_{2},g} \cdots \tau\_{c_T \| c\_{T-1},g} \prod\_{t=1}^{T}
\prod\_{i=1}^{I} \pi\_{i c\_{t,g}}^{x\_{eit}} (1 - \pi\_{i
c\_{t,g}})^{1 - x\_{eit}}, \$\$

where:

- \\v\_{c_1\|g}\\ represents the probability of belonging to attribute
  profile \\c\\ at time 1 given the observed group \\g\\.

- \\\tau\_{c_t \| c\_{T-1},g}\\ represents the probability of
  transitioning attribute profiles from time point \\t-1\\ to time point
  \\t\\.

- \\\pi\_{ic\_{t,g}}\\ is the item response function, which models the
  probability of answering item \\i\\ correctly at time \\t\\ given
  attribute profile \\c\\ and observed group \\g\\.

Therefore, if the study purpose is to assess growth between a treatment
and control group in a pre- and post-test design, the probability of the
item response vector reduces to:

\$\$ P(X_e = x_e\|G=g) = \sum\_{c_1=1}^{C} \sum\_{c_2=1}^{C} v\_{c_1\|g}
\tau\_{c_2 \| c\_{1,g}} \prod\_{t=1}^{2} \prod\_{i=1}^{I} \pi\_{i
c\_{t,g}}^{x\_{eit}} (1 - \pi\_{i c\_{t,g}})^{1 - x\_{eit}}. \$\$

**Accounting for Measurement Invariance**

Measurement invariance indicates whether the **item response function**
remains constant over time points (**time invariance**) or across groups
(**group invariance**). Note that regardless of the assumed invariance,
attribute mastery transitions can still be compared across time and
groups.

Depending on the assumed constrained, one of the four measurement
invariance conditions can be applied:

Consider an experiment design with a treatment and control group.

### **a) No time Invariance across time or group invariance assumed**

If neither time nor group invariance is assumed, each item has a
different response function over time and across groups. Thus, the
probability of the item response function remains unchanged.

\$\$ P(X_e = x_e\|G=g) = \sum\_{c_1=1}^{C} \sum\_{c_2=1}^{C} v\_{c_1\|g}
\tau\_{c_2 \| c\_{1}, g} \prod\_{t=1}^{2} \prod\_{i=1}^{I} \pi\_{i
c\_{t,g}}^{x\_{eit}} (1 - \pi\_{i c\_{t,g}})^{1 - x\_{eit}}. \$\$

### **b) No time Invariance across time assumed but group invariance assumed**

If time invariance is not assumed but group invariance is, each item has
a different response function over time. Thus, the probability of the
item response function only depends on \\t\\.

\$\$ P(X_e = x_e\|G=g) = \sum\_{c_1=1}^{C} \sum\_{c_2=1}^{C} v\_{c_1\|g}
\tau\_{c_2 \| c\_{1}, g} \prod\_{t=1}^{2} \prod\_{i=1}^{I} \pi\_{i
c\_{t}}^{x\_{eit}} (1 - \pi\_{i c\_{t}})^{1 - x\_{eit}}. \$\$

### **c) Time Invariance across time assumed but not group invariance**

If time invariance is assumed but group invariance is not, each item has
a different response function across groups. Thus, the probability of
the item response function only depends on \\g\\.

\$\$ P(X_e = x_e\|G=g) = \sum\_{c_1=1}^{C} \sum\_{c_2=1}^{C} v\_{c_1\|g}
\tau\_{c_2 \| c\_{1}, g} \prod\_{t=1}^{2} \prod\_{i=1}^{I} \pi\_{i
c\_{g}}^{x\_{eit}} (1 - \pi\_{i c\_{g}})^{1 - x\_{eit}}. \$\$

### **d) Time Invariance across time and group invariance assumed**

Finally, when time and group invariance are assumed, each item has the
same item response function over time and groups, reducing the
measurement model to an LCDM.

\$\$ P(X_e = x_e\|G=g) = \sum\_{c_1=1}^{C} \sum\_{c_2=1}^{C} v\_{c_1\|g}
\tau\_{c_2 \| c\_{1,g}} \prod\_{t=1}^{2} \prod\_{i=1}^{I} \pi\_{i
c}^{x\_{eit}} (1 - \pi\_{i c})^{1 - x\_{eit}}. \$\$

## References

de la Torre, J. (2011). The Generalized DINA model framework.
*Psychometrika, 76*, 179–199. <doi:10.1007/s11336-011-9207-7>.

George, A. C., Robitzsch, A., Kiefer, T., Gross, J., & Ünlü , A. (2016).
The *R* package **CDM** for cognitive diagnosis models. *Journal of
Statistical Software,74*(2), 1-24. <doi:10.18637/jss.v074.i02>

Henson, R., Templin, J., & Willse, J. (2009). Defining a family of
cognitive diagnosis models using log-linear models with latent
variables. *Psychometrika, 74*, 191-21. <doi:10.1007/s11336-008-9089-5>.

Johnson, M. S., & Sinharay, S. (2020). The reliability of the posterior
probability of skill attainment in diagnostic classification models.
*Journal of Educational Measurement, 47*(1), 5–31.
<doi:10.3102/1076998619864550>.

Kaya, Y., & Leite, W. (2017). Assessing change in latent skills across
time With longitudinal cognitive diagnosis modeling: An evaluation of
model performance. *Educational and Psychological Measurement, 77*(3),
369–388. <doi:10.1177/0013164416659314>.

Li, F., Cohen, A., Bottge, B., & Templin, J. (2015). A latent transition
analysis model for assessing change in cognitive skills. *Educational
and Psychological Measurement, 76*(2), 181–204.
<doi:10.1177/0013164415588946>.

Madison, M. J. (2019). Reliably assessing growth with longitudinal
diagnostic classification models. *Educational Measurement: Issues and
Practice, 38*(2), 68-78. <doi:10.1111/emip.12243>.

Madison, M. J., & Bradshaw, L. (2018a). Assessing growth in a diagnostic
classification model framework. *Psychometrika, 83*(4), 963-990.
<doi:10.1007/s11336-018-9638-5>.

Madison, M. J., & Bradshaw, L. (2018b). Evaluating intervention effects
in a diagnostic classification model framework. *Journal of Educational
Measurement, 55*(1), 32-51. <doi:10.1111/jedm.12162>.

Madison, M.J., Chung, S., Kim, J., & Bradshaw, L.P. (2024) Approaches to
estimating longitudinal diagnostic classification models.
*Behaviormetrika, 51*(7), 7-19. <doi:10.1007/s41237-023-00202-5>.

Madison, M. J., Jeon, M., Cotterell, M., Haab, S., & Zor, S. (2025).
TDCM: An R package for estimating longitudinal diagnostic classification
models. *Multivariate Behavioral Research*, 1–10.
<doi:10.1080/00273171.2025.2453454>.

Ravand, H., Effatpanah, F., Kunina-Habenicht, O., & Madison, M. J.
(2025). A didactic illustration of writing skill growth through a
longitudinal diagnostic classification model. *Frontiers in Psychology*.
<doi:10.3389/fpsyg.2024.1521808>.

Rupp, A. A., Templin, J., & Henson, R. (2010). *Diagnostic Measurement:
Theory, Methods, and Applications*. New York: Guilford. ISBN:
9781606235430.

Schellman, M., & Madison, M. J. (2024). Estimating the reliability of
skill transition in longitudinal DCMs. *Journal of Educational and
Behavioral Statistics*.<doi:10.3102/10769986241256032>.

Templin, J., & Bradshaw, L. (2013). Measuring the reliability of
diagnostic classification model examinee estimates. *Journal of
Classification, 30*, 251-275. <doi:10.1007/s00357-013-9129-4>.

Wang. S., Yang. Y., Culpepper, S. A., & Douglas, J. (2018). Tracking
skill acquisition with cognitive diagnosis models: A higher-order,
hidden Markov model With covariates. *Journal of Educational and
Behavioral Statistics, 43*(1), 57-87. <doi:10.3102/1076998617719727>.

## Examples

``` r
# \donttest{
############################################################################
# Example 1: Multigroup TDCM without assuming time or group invariance
############################################################################

# Load data: G = 2, T = 2, A = 4, I = 20
data(data.tdcm04, package = "TDCM")
data <- data.tdcm04$data
q.matrix <- data.tdcm04$q.matrix
groups <- data.tdcm04$groups

# Estimate model
mg.model1 <- TDCM::mg.tdcm(data, q.matrix, num.time.points = 2,
                           groups = groups, time.invariance = FALSE,
                           group.invariance = FALSE)
#> [1] Preparing data for mg.tdcm()...
#> [1] Estimating the multigroup TDCM in mg.tdcm()...
#> [1] Depending on model complexity, estimation time may vary...
#> [1] Multigroup TDCM estimation complete.
#> [1] Use mg.tdcm.summary() to display results.

# Summarize results
results1 <- TDCM::mg.tdcm.summary(mg.model1)
#> [1] Summarizing results...
#> Warning: NaNs produced
#> Warning: NaNs produced
#> [1] Routine finished. Check results.
results1$item.parameters
#> , , Group 1
#> 
#>         λ0     λ1,1  λ1,2  λ1,3  λ1,4  λ2,12  λ2,13 λ2,14 λ2,23 λ2,24 λ2,34
#> Item 1  -2.05  2.594   --    --    --    --     --    --    --    --    -- 
#> Item 2  -2.041 2.47    --    --    --    --     --    --    --    --    -- 
#> Item 3  -1.849 2.282   --    --    --    --     --    --    --    --    -- 
#> Item 4  -2.168 2.008 1.783   --    --  -0.068   --    --    --    --    -- 
#> Item 5  -2.021 1.827   --  1.061   --    --   0.699   --    --    --    -- 
#> Item 6  -2.118   --  2.515   --    --    --     --    --    --    --    -- 
#> Item 7  -1.835   --  2.535   --    --    --     --    --    --    --    -- 
#> Item 8  -1.987   --  2.512   --    --    --     --    --    --    --    -- 
#> Item 9  -2.219   --  2.032 1.916   --    --     --    --  0.039   --    -- 
#> Item 10 -2.119   --  1.263   --  1.717   --     --    --    --  1.355   -- 
#> Item 11 -1.984   --    --  2.422   --    --     --    --    --    --    -- 
#> Item 12 -2.511   --    --  2.858   --    --     --    --    --    --    -- 
#> Item 13 -2.108   --    --  2.245   --    --     --    --    --    --    -- 
#> Item 14 -1.914   --    --  0.346 0.977   --     --    --    --    --  2.097
#> Item 15 -2.148 1.678   --  2.224   --    --   0.583   --    --    --    -- 
#> Item 16 -2.039   --    --    --  2.416   --     --    --    --    --    -- 
#> Item 17 -2.439   --    --    --  3.186   --     --    --    --    --    -- 
#> Item 18 -2.056   --    --    --  2.643   --     --    --    --    --    -- 
#> Item 19 -1.926 1.293   --    --  1.068   --     --  1.461   --    --    -- 
#> Item 20 -2.227   --  1.882   --  1.749   --     --    --    --  0.208   -- 
#> Item 21 -1.797 2.202   --    --    --    --     --    --    --    --    -- 
#> Item 22 -1.959 2.405   --    --    --    --     --    --    --    --    -- 
#> Item 23 -2.454 2.804   --    --    --    --     --    --    --    --    -- 
#> Item 24 -2.353 1.785 1.909   --    --  0.789    --    --    --    --    -- 
#> Item 25 -2.313 1.237   --  2.041   --    --   1.354   --    --    --    -- 
#> Item 26 -1.836   --  2.349   --    --    --     --    --    --    --    -- 
#> Item 27 -1.951   --  2.555   --    --    --     --    --    --    --    -- 
#> Item 28 -1.949   --  2.487   --    --    --     --    --    --    --    -- 
#> Item 29 -1.96    --  1.632 1.775   --    --     --    --  0.71    --    -- 
#> Item 30 -2.286   --  1.949   --  1.973   --     --    --    --  0.361   -- 
#> Item 31 -1.794   --    --  2.466   --    --     --    --    --    --    -- 
#> Item 32 -1.886   --    --  2.574   --    --     --    --    --    --    -- 
#> Item 33 -1.516   --    --  1.969   --    --     --    --    --    --    -- 
#> Item 34 -2.066   --    --  1.307 1.667   --     --    --    --    --  1.191
#> Item 35 -2.329 2.013   --  1.643   --    --   0.891   --    --    --    -- 
#> Item 36 -2.577   --    --    --  3.058   --     --    --    --    --    -- 
#> Item 37 -2.028   --    --    --  2.627   --     --    --    --    --    -- 
#> Item 38 -1.889   --    --    --  2.206   --     --    --    --    --    -- 
#> Item 39 -1.982 1.678   --    --  1.374   --     --  0.825   --    --    -- 
#> Item 40 -2.19    --  2.483   --  1.572   --     --    --    --  0.602   -- 
#> 
#> , , Group 2
#> 
#>         λ0     λ1,1  λ1,2  λ1,3  λ1,4  λ2,12 λ2,13 λ2,14 λ2,23 λ2,24 λ2,34 
#> Item 1  -1.932 2.385   --    --    --    --    --    --    --    --    --  
#> Item 2  -1.921 2.347   --    --    --    --    --    --    --    --    --  
#> Item 3  -2.055 2.388   --    --    --    --    --    --    --    --    --  
#> Item 4  -2.112 2.102 1.31    --    --  0.889   --    --    --    --    --  
#> Item 5  -2.027 1.556   --  0.995   --    --  1.578   --    --    --    --  
#> Item 6  -1.893   --  2.523   --    --    --    --    --    --    --    --  
#> Item 7  -1.871   --  2.496   --    --    --    --    --    --    --    --  
#> Item 8  -2.053   --  2.392   --    --    --    --    --    --    --    --  
#> Item 9  -2.062   --  1.69  1.373   --    --    --    --  1.149   --    --  
#> Item 10 -1.997   --  1.478   --  1.222   --    --    --    --  1.271   --  
#> Item 11 -2.053   --    --  2.467   --    --    --    --    --    --    --  
#> Item 12 -2.006   --    --  2.547   --    --    --    --    --    --    --  
#> Item 13 -2.003   --    --  2.658   --    --    --    --    --    --    --  
#> Item 14 -2.056   --    --  3.463 1.336   --    --    --    --    --  -1.045
#> Item 15 -2.051 1.423   --  1.399   --    --  1.317   --    --    --    --  
#> Item 16 -2.182   --    --    --  2.729   --    --    --    --    --    --  
#> Item 17 -2.289   --    --    --  2.932   --    --    --    --    --    --  
#> Item 18 -2.266   --    --    --  2.768   --    --    --    --    --    --  
#> Item 19 -2.227 1.408   --    --  1.542   --    --  1.128   --    --    --  
#> Item 20 -1.898   --  1.182   --  1.453   --    --    --    --  1.319   --  
#> Item 21 -1.757 2.365   --    --    --    --    --    --    --    --    --  
#> Item 22 -2.455 3.151   --    --    --    --    --    --    --    --    --  
#> Item 23 -2.393 2.99    --    --    --    --    --    --    --    --    --  
#> Item 24 -1.626 1.061 1.029   --    --  1.447   --    --    --    --    --  
#> Item 25 -1.96  1.027   --  1.274   --    --  1.765   --    --    --    --  
#> Item 26 -1.723   --  2.266   --    --    --    --    --    --    --    --  
#> Item 27 -1.766   --  2.177   --    --    --    --    --    --    --    --  
#> Item 28 -1.908   --  2.338   --    --    --    --    --    --    --    --  
#> Item 29 -2.22    --  1.947 1.341   --    --    --    --  1.014   --    --  
#> Item 30 -1.967   --  1.124   --  1.527   --    --    --    --  1.036   --  
#> Item 31 -2.011   --    --  2.503   --    --    --    --    --    --    --  
#> Item 32 -2.6     --    --  3.072   --    --    --    --    --    --    --  
#> Item 33 -2.027   --    --  2.652   --    --    --    --    --    --    --  
#> Item 34 -1.663   --    --  0.832 0.805   --    --    --    --    --  1.816 
#> Item 35 -2.037 1.656   --  1.554   --    --  0.601   --    --    --    --  
#> Item 36 -2.022   --    --    --  2.431   --    --    --    --    --    --  
#> Item 37 -2.866   --    --    --  3.443   --    --    --    --    --    --  
#> Item 38 -1.935   --    --    --  2.235   --    --    --    --    --    --  
#> Item 39 -1.947 1.525   --    --  1.439   --    --  0.635   --    --    --  
#> Item 40 -2.809   --  2.121   --  2.788   --    --    --    --  -0.19   --  
#> 

## In this case, neither time nor group invariance is assumed,
## meaning that item parameters are estimated separately for
## each group and time point. This allows item functioning to vary both
## across groups and over time.

# , , Group 1
#
#            l0     l1,1  l1,2  l1,3  l1,4  l2,12  l2,13 l2,14 l2,23 l2,24 l2,34
#   Item 1  -2.05  2.594   --    --    --    --     --    --    --    --    --
#   Item 2  -2.041 2.47    --    --    --    --     --    --    --    --    --
#   Item 3  -1.849 2.282   --    --    --    --     --    --    --    --    --
#   Item 4  -2.168 2.008 1.783   --    --  -0.068   --    --    --    --    --
#   Item 5  -2.021 1.827   --  1.061   --    --   0.699   --    --    --    --
#   Item 6  -2.118   --  2.515   --    --    --     --    --    --    --    --
#   Item 7  -1.835   --  2.535   --    --    --     --    --    --    --    --
#   Item 8  -1.987   --  2.512   --    --    --     --    --    --    --    --
#   Item 9  -2.219   --  2.032 1.916   --    --     --    --  0.039   --    --
#   Item 10 -2.119   --  1.263   --  1.717   --     --    --    --  1.355   --
#   Item 11 -1.984   --    --  2.422   --    --     --    --    --    --    --
#   Item 12 -2.511   --    --  2.858   --    --     --    --    --    --    --
#   Item 13 -2.108   --    --  2.245   --    --     --    --    --    --    --
#   Item 14 -1.914   --    --  0.346 0.977   --     --    --    --    --  2.097
#   Item 15 -2.148 1.678   --  2.224   --    --   0.583   --    --    --    --
#   Item 16 -2.039   --    --    --  2.416   --     --    --    --    --    --
#   Item 17 -2.439   --    --    --  3.186   --     --    --    --    --    --
#   Item 18 -2.056   --    --    --  2.643   --     --    --    --    --    --
#   Item 19 -1.926 1.293   --    --  1.068   --     --  1.461   --    --    --
#   Item 20 -2.227   --  1.882   --  1.749   --     --    --    --  0.208   --
#   Item 21 -1.797 2.202   --    --    --    --     --    --    --    --    --
#   Item 22 -1.959 2.405   --    --    --    --     --    --    --    --    --
#   Item 23 -2.454 2.804   --    --    --    --     --    --    --    --    --
#   Item 24 -2.353 1.785 1.909   --    --  0.789    --    --    --    --    --
#   Item 25 -2.313 1.237   --  2.041   --    --   1.354   --    --    --    --
#   Item 26 -1.836   --  2.349   --    --    --     --    --    --    --    --
#   Item 27 -1.951   --  2.555   --    --    --     --    --    --    --    --
#   Item 28 -1.949   --  2.487   --    --    --     --    --    --    --    --
#   Item 29 -1.96    --  1.632 1.775   --    --     --    --  0.71    --    --
#   Item 30 -2.286   --  1.949   --  1.973   --     --    --    --  0.361   --
#   Item 31 -1.794   --    --  2.466   --    --     --    --    --    --    --
#   Item 32 -1.886   --    --  2.574   --    --     --    --    --    --    --
#   Item 33 -1.516   --    --  1.969   --    --     --    --    --    --    --
#   Item 34 -2.066   --    --  1.307 1.667   --     --    --    --    --  1.191
#   Item 35 -2.329 2.013   --  1.643   --    --   0.891   --    --    --    --
#   Item 36 -2.577   --    --    --  3.058   --     --    --    --    --    --
#   Item 37 -2.028   --    --    --  2.627   --     --    --    --    --    --
#   Item 38 -1.889   --    --    --  2.206   --     --    --    --    --    --
#   Item 39 -1.982 1.678   --    --  1.374   --     --  0.825   --    --    --
#   Item 40 -2.19    --  2.483   --  1.572   --     --    --    --  0.602   --
#
#   , , Group 2
#
#            l0    l1,1  l1,2  l1,3  l1,4  l2,12 l2,13 l2,14 l2,23 l2,24 l2,34
#   Item 1  -1.932 2.385   --    --    --    --    --    --    --    --    --
#   Item 2  -1.921 2.347   --    --    --    --    --    --    --    --    --
#   Item 3  -2.055 2.388   --    --    --    --    --    --    --    --    --
#   Item 4  -2.112 2.102 1.31    --    --  0.889   --    --    --    --    --
#   Item 5  -2.027 1.556   --  0.995   --    --  1.578   --    --    --    --
#   Item 6  -1.893   --  2.523   --    --    --    --    --    --    --    --
#   Item 7  -1.871   --  2.496   --    --    --    --    --    --    --    --
#   Item 8  -2.053   --  2.392   --    --    --    --    --    --    --    --
#   Item 9  -2.062   --  1.69  1.373   --    --    --    --  1.149   --    --
#   Item 10 -1.997   --  1.478   --  1.222   --    --    --    --  1.271   --
#   Item 11 -2.053   --    --  2.467   --    --    --    --    --    --    --
#   Item 12 -2.006   --    --  2.547   --    --    --    --    --    --    --
#   Item 13 -2.003   --    --  2.658   --    --    --    --    --    --    --
#   Item 14 -2.056   --    --  3.463 1.336   --    --    --    --    --  -1.045
#   Item 15 -2.051 1.423   --  1.399   --    --  1.317   --    --    --    --
#   Item 16 -2.182   --    --    --  2.729   --    --    --    --    --    --
#   Item 17 -2.289   --    --    --  2.932   --    --    --    --    --    --
#   Item 18 -2.266   --    --    --  2.768   --    --    --    --    --    --
#   Item 19 -2.227 1.408   --    --  1.542   --    --  1.128   --    --    --
#   Item 20 -1.898   --  1.182   --  1.453   --    --    --    --  1.319   --
#   Item 21 -1.757 2.365   --    --    --    --    --    --    --    --    --
#   Item 22 -2.455 3.151   --    --    --    --    --    --    --    --    --
#   Item 23 -2.393 2.99    --    --    --    --    --    --    --    --    --
#   Item 24 -1.626 1.061 1.029   --    --  1.447   --    --    --    --    --
#   Item 25 -1.96  1.027   --  1.274   --    --  1.765   --    --    --    --
#   Item 26 -1.723   --  2.266   --    --    --    --    --    --    --    --
#   Item 27 -1.766   --  2.177   --    --    --    --    --    --    --    --
#   Item 28 -1.908   --  2.338   --    --    --    --    --    --    --    --
#   Item 29 -2.22    --  1.947 1.341   --    --    --    --  1.014   --    --
#   Item 30 -1.967   --  1.124   --  1.527   --    --    --    --  1.036   --
#   Item 31 -2.011   --    --  2.503   --    --    --    --    --    --    --
#   Item 32 -2.6     --    --  3.072   --    --    --    --    --    --    --
#   Item 33 -2.027   --    --  2.652   --    --    --    --    --    --    --
#   Item 34 -1.663   --    --  0.832 0.805   --    --    --    --    --  1.816
#   Item 35 -2.037 1.656   --  1.554   --    --  0.601   --    --    --    --
#   Item 36 -2.022   --    --    --  2.431   --    --    --    --    --    --
#   Item 37 -2.866   --    --    --  3.443   --    --    --    --    --    --
#   Item 38 -1.935   --    --    --  2.235   --    --    --    --    --    --
#   Item 39 -1.947 1.525   --    --  1.439   --    --  0.635   --    --    --
#   Item 40 -2.809   --  2.121   --  2.788   --    --    --    --  -0.19   --

results1$growth
#> , , Group 1
#> 
#>             T1[1] T2[1]
#> Attribute 1 0.301 0.438
#> Attribute 2 0.400 0.492
#> Attribute 3 0.245 0.594
#> Attribute 4 0.426 0.732
#> 
#> , , Group 2
#> 
#>             T1[1] T2[1]
#> Attribute 1 0.305 0.623
#> Attribute 2 0.380 0.734
#> Attribute 3 0.204 0.705
#> Attribute 4 0.446 0.725
#> 
results1$growth.effects
#> , , Group 1
#> 
#>             T1[1] T2[1] Growth Odds Ratio Cohen`s h
#> Attribute 1 0.301 0.438  0.137       1.81      0.29
#> Attribute 2 0.400 0.492  0.092       1.45      0.19
#> Attribute 3 0.245 0.594  0.349       4.51      0.72
#> Attribute 4 0.426 0.732  0.306       3.68      0.63
#> 
#> , , Group 2
#> 
#>             T1[1] T2[1] Growth Odds Ratio Cohen`s h
#> Attribute 1 0.305 0.623  0.318       3.77      0.65
#> Attribute 2 0.380 0.734  0.354       4.50      0.73
#> Attribute 3 0.204 0.705  0.501       9.33      1.06
#> Attribute 4 0.446 0.725  0.279       3.27      0.57
#> 
results1$transition.probabilities
#> , , Attribute 1: Time 1 to Time 2, Group 1
#> 
#>        T2 [0] T2 [1]
#> T1 [0]  0.634  0.366
#> T1 [1]  0.396  0.604
#> 
#> , , Attribute 2: Time 1 to Time 2, Group 1
#> 
#>        T2 [0] T2 [1]
#> T1 [0]   0.58   0.42
#> T1 [1]   0.40   0.60
#> 
#> , , Attribute 3: Time 1 to Time 2, Group 1
#> 
#>        T2 [0] T2 [1]
#> T1 [0]  0.464  0.536
#> T1 [1]  0.226  0.774
#> 
#> , , Attribute 4: Time 1 to Time 2, Group 1
#> 
#>        T2 [0] T2 [1]
#> T1 [0]  0.342  0.658
#> T1 [1]  0.169  0.831
#> 
#> , , Attribute 1: Time 1 to Time 2, Group 2
#> 
#>        T2 [0] T2 [1]
#> T1 [0]  0.441  0.559
#> T1 [1]  0.231  0.769
#> 
#> , , Attribute 2: Time 1 to Time 2, Group 2
#> 
#>        T2 [0] T2 [1]
#> T1 [0]  0.363  0.637
#> T1 [1]  0.108  0.892
#> 
#> , , Attribute 3: Time 1 to Time 2, Group 2
#> 
#>        T2 [0] T2 [1]
#> T1 [0]  0.354  0.646
#> T1 [1]  0.068  0.932
#> 
#> , , Attribute 4: Time 1 to Time 2, Group 2
#> 
#>        T2 [0] T2 [1]
#> T1 [0]  0.342  0.658
#> T1 [1]  0.192  0.808
#> 

# plot results
TDCM::tdcm.plot(results1)








#> [1] **Check the plots window for line and bar plots for group growth proportions.

############################################################################
# Example 2: Multigroup TDCM assuming group invariance
############################################################################

# Estimate model
mg.model2 <- TDCM::mg.tdcm(data, q.matrix, num.time.points = 2,
                           groups = groups, time.invariance = FALSE,
                           group.invariance = TRUE)
#> [1] Preparing data for mg.tdcm()...
#> [1] Estimating the multigroup TDCM in mg.tdcm()...
#> [1] Depending on model complexity, estimation time may vary...
#> [1] Multigroup TDCM estimation complete.
#> [1] Use mg.tdcm.summary() to display results.

# summarize results
results2 <- TDCM::mg.tdcm.summary(mg.model2)
#> [1] Summarizing results...
#> [1] Routine finished. Check results.
results2$item.parameters
#>         λ0     λ1,1  λ1,2  λ1,3  λ1,4  λ2,12 λ2,13 λ2,14 λ2,23 λ2,24 λ2,34
#> Item 1  -1.983 2.496   --    --    --    --    --    --    --    --    -- 
#> Item 2  -1.965 2.401   --    --    --    --    --    --    --    --    -- 
#> Item 3  -1.943 2.33    --    --    --    --    --    --    --    --    -- 
#> Item 4  -2.133 2.073 1.586   --    --  0.319   --    --    --    --    -- 
#> Item 5  -1.999 1.676   --  0.948   --    --  1.222   --    --    --    -- 
#> Item 6  -1.976   --  2.495   --    --    --    --    --    --    --    -- 
#> Item 7  -1.855   --  2.532   --    --    --    --    --    --    --    -- 
#> Item 8  -2.011   --  2.445   --    --    --    --    --    --    --    -- 
#> Item 9  -2.096   --  1.822 1.751   --    --    --    --  0.498   --    -- 
#> Item 10 -2.044   --  1.357   --  1.456   --    --    --    --  1.327   -- 
#> Item 11 -1.989   --    --  2.433   --    --    --    --    --    --    -- 
#> Item 12 -2.172   --    --  2.628   --    --    --    --    --    --    -- 
#> Item 13 -2.035   --    --  2.461   --    --    --    --    --    --    -- 
#> Item 14 -2.008   --    --  1.764 1.221   --    --    --    --    --  0.665
#> Item 15 -2.071 1.551   --  1.942   --    --  0.765   --    --    --    -- 
#> Item 16 -2.108   --    --    --  2.587   --    --    --    --    --    -- 
#> Item 17 -2.327   --    --    --  3.016   --    --    --    --    --    -- 
#> Item 18 -2.17    --    --    --  2.727   --    --    --    --    --    -- 
#> Item 19 -2.073 1.469   --    --  1.316   --    --  1.139   --    --    -- 
#> Item 20 -2.04    --  1.537   --  1.608   --    --    --    --  0.709   -- 
#> Item 21 -1.787 2.316   --    --    --    --    --    --    --    --    -- 
#> Item 22 -2.14  2.731   --    --    --    --    --    --    --    --    -- 
#> Item 23 -2.435 2.934   --    --    --    --    --    --    --    --    -- 
#> Item 24 -2.104 1.485 1.586   --    --  1.004   --    --    --    --    -- 
#> Item 25 -2.197 1.263   --  1.734   --    --  1.394   --    --    --    -- 
#> Item 26 -1.847   --  2.374   --    --    --    --    --    --    --    -- 
#> Item 27 -1.902   --  2.369   --    --    --    --    --    --    --    -- 
#> Item 28 -1.961   --  2.418   --    --    --    --    --    --    --    -- 
#> Item 29 -2.066   --  1.705 1.603   --    --    --    --  0.865   --    -- 
#> Item 30 -2.137   --  1.437   --  1.76    --    --    --    --  0.753   -- 
#> Item 31 -1.933   --    --  2.502   --    --    --    --    --    --    -- 
#> Item 32 -2.222   --    --  2.786   --    --    --    --    --    --    -- 
#> Item 33 -1.725   --    --  2.264   --    --    --    --    --    --    -- 
#> Item 34 -1.844   --    --  1.088 1.253   --    --    --    --    --  1.404
#> Item 35 -2.236 1.919   --  1.623   --    --  0.629   --    --    --    -- 
#> Item 36 -2.211   --    --    --  2.652   --    --    --    --    --    -- 
#> Item 37 -2.429   --    --    --  3.025   --    --    --    --    --    -- 
#> Item 38 -1.906   --    --    --  2.216   --    --    --    --    --    -- 
#> Item 39 -2.026 1.658   --    --  1.48    --    --  0.636   --    --    -- 
#> Item 40 -2.405   --  1.843   --  1.921   --    --    --    --  0.741   -- 

## In this case, since group invariance is assumed,
## the item parameters are the same across groups.
## However, items parameters can still vary across time points.

#            l0   l1,1  l1,2  l1,3  l1,4  l2,12 l2,13 l2,14 l2,23 l2,24 l2,34
#  Item 1  -1.983 2.496   --    --    --    --    --    --    --    --    --
#  Item 2  -1.965 2.401   --    --    --    --    --    --    --    --    --
#  Item 3  -1.943 2.33    --    --    --    --    --    --    --    --    --
#  Item 4  -2.133 2.073 1.586   --    --  0.319   --    --    --    --    --
#  Item 5  -1.999 1.676   --  0.948   --    --  1.222   --    --    --    --
#  Item 6  -1.976   --  2.495   --    --    --    --    --    --    --    --
#  Item 7  -1.855   --  2.532   --    --    --    --    --    --    --    --
#  Item 8  -2.011   --  2.445   --    --    --    --    --    --    --    --
#  Item 9  -2.096   --  1.822 1.751   --    --    --    --  0.498   --    --
#  Item 10 -2.044   --  1.357   --  1.456   --    --    --    --  1.327   --
#  Item 11 -1.989   --    --  2.433   --    --    --    --    --    --    --
#  Item 12 -2.172   --    --  2.628   --    --    --    --    --    --    --
#  Item 13 -2.035   --    --  2.461   --    --    --    --    --    --    --
#  Item 14 -2.008   --    --  1.764 1.221   --    --    --    --    --  0.665
#  Item 15 -2.071 1.551   --  1.942   --    --  0.765   --    --    --    --
#  Item 16 -2.108   --    --    --  2.587   --    --    --    --    --    --
#  Item 17 -2.327   --    --    --  3.016   --    --    --    --    --    --
#  Item 18 -2.17    --    --    --  2.727   --    --    --    --    --    --
#  Item 19 -2.073 1.469   --    --  1.316   --    --  1.139   --    --    --
#  Item 20 -2.04    --  1.537   --  1.608   --    --    --    --  0.709   --
#  Item 21 -1.787 2.316   --    --    --    --    --    --    --    --    --
#  Item 22 -2.14  2.731   --    --    --    --    --    --    --    --    --
#  Item 23 -2.435 2.934   --    --    --    --    --    --    --    --    --
#  Item 24 -2.104 1.485 1.586   --    --  1.004   --    --    --    --    --
#  Item 25 -2.197 1.263   --  1.734   --    --  1.394   --    --    --    --
#  Item 26 -1.847   --  2.374   --    --    --    --    --    --    --    --
#  Item 27 -1.902   --  2.369   --    --    --    --    --    --    --    --
#  Item 28 -1.961   --  2.418   --    --    --    --    --    --    --    --
#  Item 29 -2.066   --  1.705 1.603   --    --    --    --  0.865   --    --
#  Item 30 -2.137   --  1.437   --  1.76    --    --    --    --  0.753   --
#  Item 31 -1.933   --    --  2.502   --    --    --    --    --    --    --
#  Item 32 -2.222   --    --  2.786   --    --    --    --    --    --    --
#  Item 33 -1.725   --    --  2.264   --    --    --    --    --    --    --
#  Item 34 -1.844   --    --  1.088 1.253   --    --    --    --    --  1.404
#  Item 35 -2.236 1.919   --  1.623   --    --  0.629   --    --    --    --
#  Item 36 -2.211   --    --    --  2.652   --    --    --    --    --    --
#  Item 37 -2.429   --    --    --  3.025   --    --    --    --    --    --
#  Item 38 -1.906   --    --    --  2.216   --    --    --    --    --    --
#  Item 39 -2.026 1.658   --    --  1.48    --    --  0.636   --    --    --
#  Item 40 -2.405   --  1.843   --  1.921   --    --    --    --  0.741   --

results2$growth
#> , , Group 1
#> 
#>             T1[1] T2[1]
#> Attribute 1 0.303 0.437
#> Attribute 2 0.397 0.501
#> Attribute 3 0.225 0.613
#> Attribute 4 0.424 0.738
#> 
#> , , Group 2
#> 
#>             T1[1] T2[1]
#> Attribute 1 0.297 0.625
#> Attribute 2 0.378 0.738
#> Attribute 3 0.208 0.694
#> Attribute 4 0.444 0.717
#> 
results2$growth.effects
#> , , Group 1
#> 
#>             T1[1] T2[1] Growth Odds Ratio Cohen`s h
#> Attribute 1 0.303 0.437  0.134       1.79      0.28
#> Attribute 2 0.397 0.501  0.104       1.52      0.21
#> Attribute 3 0.225 0.613  0.388       5.46      0.81
#> Attribute 4 0.424 0.738  0.314       3.83      0.65
#> 
#> , , Group 2
#> 
#>             T1[1] T2[1] Growth Odds Ratio Cohen`s h
#> Attribute 1 0.297 0.625  0.328       3.95      0.67
#> Attribute 2 0.378 0.738  0.360       4.64      0.74
#> Attribute 3 0.208 0.694  0.486       8.64      1.02
#> Attribute 4 0.444 0.717  0.273       3.17      0.56
#> 
results2$transition.probabilities
#> , , Attribute 1: Time 1 to Time 2, Group 1
#> 
#>        T2 [0] T2 [1]
#> T1 [0]  0.636  0.364
#> T1 [1]  0.396  0.604
#> 
#> , , Attribute 2: Time 1 to Time 2, Group 1
#> 
#>        T2 [0] T2 [1]
#> T1 [0]  0.569  0.431
#> T1 [1]  0.393  0.607
#> 
#> , , Attribute 3: Time 1 to Time 2, Group 1
#> 
#>        T2 [0] T2 [1]
#> T1 [0]  0.444  0.556
#> T1 [1]  0.190  0.810
#> 
#> , , Attribute 4: Time 1 to Time 2, Group 1
#> 
#>        T2 [0] T2 [1]
#> T1 [0]  0.332  0.668
#> T1 [1]  0.166  0.834
#> 
#> , , Attribute 1: Time 1 to Time 2, Group 2
#> 
#>        T2 [0] T2 [1]
#> T1 [0]  0.435  0.565
#> T1 [1]  0.232  0.768
#> 
#> , , Attribute 2: Time 1 to Time 2, Group 2
#> 
#>        T2 [0] T2 [1]
#> T1 [0]  0.359  0.641
#> T1 [1]  0.104  0.896
#> 
#> , , Attribute 3: Time 1 to Time 2, Group 2
#> 
#>        T2 [0] T2 [1]
#> T1 [0]  0.367  0.633
#> T1 [1]  0.074  0.926
#> 
#> , , Attribute 4: Time 1 to Time 2, Group 2
#> 
#>        T2 [0] T2 [1]
#> T1 [0]  0.349  0.651
#> T1 [1]  0.201  0.799
#> 

# plot results
TDCM::tdcm.plot(results2)








#> [1] **Check the plots window for line and bar plots for group growth proportions.

############################################################################
# Example 3: Multigroup TDCM assuming time invariance
############################################################################

# Estimate model
mg.model3 <- TDCM::mg.tdcm(data, q.matrix, num.time.points = 2,
                           groups = groups, time.invariance = TRUE,
                           group.invariance = FALSE)
#> [1] Preparing data for mg.tdcm()...
#> [1] Estimating the multigroup TDCM in mg.tdcm()...
#> [1] Depending on model complexity, estimation time may vary...
#> [1] Multigroup TDCM estimation complete.
#> [1] Use mg.tdcm.summary() to display results.

# summarize results
results3 <- TDCM::mg.tdcm.summary(mg.model3)
#> [1] Summarizing results...
#> Warning: NaNs produced
#> Warning: NaNs produced
#> [1] Routine finished. Check results.
results3$item.parameters
#> , , Group 1
#> 
#>         λ0     λ1,1  λ1,2  λ1,3  λ1,4  λ2,12 λ2,13 λ2,14 λ2,23 λ2,24 λ2,34
#> Item 1  -1.945 2.38    --    --    --    --    --    --    --    --    -- 
#> Item 2  -2.029 2.44    --    --    --    --    --    --    --    --    -- 
#> Item 3  -2.174 2.551   --    --    --    --    --    --    --    --    -- 
#> Item 4  -2.281 1.887 1.859   --    --  0.364   --    --    --    --    -- 
#> Item 5  -2.155 1.415   --  1.519   --    --  1.16    --    --    --    -- 
#> Item 6  -1.96    --  2.414   --    --    --    --    --    --    --    -- 
#> Item 7  -1.884   --  2.542   --    --    --    --    --    --    --    -- 
#> Item 8  -1.964   --  2.501   --    --    --    --    --    --    --    -- 
#> Item 9  -2.11    --  1.771 1.931   --    --    --    --  0.404   --    -- 
#> Item 10 -2.198   --  1.535   --  1.847   --    --    --    --  0.945   -- 
#> Item 11 -1.966   --    --  2.543   --    --    --    --    --    --    -- 
#> Item 12 -2.247   --    --  2.749   --    --    --    --    --    --    -- 
#> Item 13 -1.834   --    --  2.118   --    --    --    --    --    --    -- 
#> Item 14 -1.952   --    --  0.731 1.23    --    --    --    --    --  1.817
#> Item 15 -2.291 1.877   --  1.923   --    --  0.688   --    --    --    -- 
#> Item 16 -2.262   --    --    --  2.706   --    --    --    --    --    -- 
#> Item 17 -2.231   --    --    --  2.899   --    --    --    --    --    -- 
#> Item 18 -1.985   --    --    --  2.439   --    --    --    --    --    -- 
#> Item 19 -1.961 1.425   --    --  1.195   --    --  1.245   --    --    -- 
#> Item 20 -2.244   --  2.284   --  1.701   --    --    --    --  0.268   -- 
#> 
#> , , Group 2
#> 
#>         λ0     λ1,1  λ1,2  λ1,3  λ1,4  λ2,12 λ2,13 λ2,14 λ2,23 λ2,24 λ2,34
#> Item 1  -1.833 2.364   --    --    --    --    --    --    --    --    -- 
#> Item 2  -2.146 2.718   --    --    --    --    --    --    --    --    -- 
#> Item 3  -2.201 2.68    --    --    --    --    --    --    --    --    -- 
#> Item 4  -1.865 1.597 1.149   --    --  1.167   --    --    --    --    -- 
#> Item 5  -1.969 1.309   --  1.118   --    --  1.665   --    --    --    -- 
#> Item 6  -1.824   --  2.413   --    --    --    --    --    --    --    -- 
#> Item 7  -1.832   --  2.349   --    --    --    --    --    --    --    -- 
#> Item 8  -1.973   --  2.361   --    --    --    --    --    --    --    -- 
#> Item 9  -2.088   --  1.758 1.25    --    --    --    --  1.201   --    -- 
#> Item 10 -1.985   --  1.354   --  1.401   --    --    --    --  1.106   -- 
#> Item 11 -2.006   --    --  2.452   --    --    --    --    --    --    -- 
#> Item 12 -2.192   --    --  2.69    --    --    --    --    --    --    -- 
#> Item 13 -1.984   --    --  2.624   --    --    --    --    --    --    -- 
#> Item 14 -1.946   --    --  2.269 1.185   --    --    --    --    --  0.27 
#> Item 15 -2.025 1.539   --  1.453   --    --  0.957   --    --    --    -- 
#> Item 16 -2.057   --    --    --  2.549   --    --    --    --    --    -- 
#> Item 17 -2.435   --    --    --  3.068   --    --    --    --    --    -- 
#> Item 18 -2.094   --    --    --  2.514   --    --    --    --    --    -- 
#> Item 19 -2.076 1.589   --    --  1.492   --    --  0.738   --    --    -- 
#> Item 20 -2.288   --  1.674   --  2.12    --    --    --    --  0.495   -- 
#> 

## Since time invariance is assumed, the item parameters are the same across time points.
## However, items parameters can still vary across groups.

# , , Group 1
#
#             l0   l1,1  l1,2  l1,3  l1,4  l2,12 l2,13 l2,14 l2,23 l2,24 l2,34
#   Item 1  -1.945 2.38    --    --    --    --    --    --    --    --    --
#   Item 2  -2.029 2.44    --    --    --    --    --    --    --    --    --
#   Item 3  -2.174 2.551   --    --    --    --    --    --    --    --    --
#   Item 4  -2.281 1.887 1.859   --    --  0.364   --    --    --    --    --
#   Item 5  -2.155 1.415   --  1.519   --    --  1.16    --    --    --    --
#   Item 6  -1.96    --  2.414   --    --    --    --    --    --    --    --
#   Item 7  -1.884   --  2.542   --    --    --    --    --    --    --    --
#   Item 8  -1.964   --  2.501   --    --    --    --    --    --    --    --
#   Item 9  -2.11    --  1.771 1.931   --    --    --    --  0.404   --    --
#   Item 10 -2.198   --  1.535   --  1.847   --    --    --    --  0.945   --
#   Item 11 -1.966   --    --  2.543   --    --    --    --    --    --    --
#   Item 12 -2.247   --    --  2.749   --    --    --    --    --    --    --
#   Item 13 -1.834   --    --  2.118   --    --    --    --    --    --    --
#   Item 14 -1.952   --    --  0.731 1.23    --    --    --    --    --  1.817
#   Item 15 -2.291 1.877   --  1.923   --    --  0.688   --    --    --    --
#   Item 16 -2.262   --    --    --  2.706   --    --    --    --    --    --
#   Item 17 -2.231   --    --    --  2.899   --    --    --    --    --    --
#   Item 18 -1.985   --    --    --  2.439   --    --    --    --    --    --
#   Item 19 -1.961 1.425   --    --  1.195   --    --  1.245   --    --    --
#   Item 20 -2.244   --  2.284   --  1.701   --    --    --    --  0.268   --
#
#   , , Group 2
#
#             l0   l1,1  l1,2  l1,3  l1,4  l2,12 l2,13 l2,14 l2,23 l2,24 l2,34
#   Item 1  -1.833 2.364   --    --    --    --    --    --    --    --    --
#   Item 2  -2.146 2.718   --    --    --    --    --    --    --    --    --
#   Item 3  -2.201 2.68    --    --    --    --    --    --    --    --    --
#   Item 4  -1.865 1.597 1.149   --    --  1.167   --    --    --    --    --
#   Item 5  -1.969 1.309   --  1.118   --    --  1.665   --    --    --    --
#   Item 6  -1.824   --  2.413   --    --    --    --    --    --    --    --
#   Item 7  -1.832   --  2.349   --    --    --    --    --    --    --    --
#   Item 8  -1.973   --  2.361   --    --    --    --    --    --    --    --
#   Item 9  -2.088   --  1.758 1.25    --    --    --    --  1.201   --    --
#   Item 10 -1.985   --  1.354   --  1.401   --    --    --    --  1.106   --
#   Item 11 -2.006   --    --  2.452   --    --    --    --    --    --    --
#   Item 12 -2.192   --    --  2.69    --    --    --    --    --    --    --
#   Item 13 -1.984   --    --  2.624   --    --    --    --    --    --    --
#   Item 14 -1.946   --    --  2.269 1.185   --    --    --    --    --  0.27
#   Item 15 -2.025 1.539   --  1.453   --    --  0.957   --    --    --    --
#   Item 16 -2.057   --    --    --  2.549   --    --    --    --    --    --
#   Item 17 -2.435   --    --    --  3.068   --    --    --    --    --    --
#   Item 18 -2.094   --    --    --  2.514   --    --    --    --    --    --
#   Item 19 -2.076 1.589   --    --  1.492   --    --  0.738   --    --    --
#   Item 20 -2.288   --  1.674   --  2.12    --    --    --    --  0.495   --

results3$growth
#> , , Group 1
#> 
#>             T1[1] T2[1]
#> Attribute 1 0.314 0.440
#> Attribute 2 0.394 0.495
#> Attribute 3 0.231 0.622
#> Attribute 4 0.421 0.735
#> 
#> , , Group 2
#> 
#>             T1[1] T2[1]
#> Attribute 1 0.298 0.628
#> Attribute 2 0.376 0.735
#> Attribute 3 0.206 0.697
#> Attribute 4 0.446 0.705
#> 
results3$growth.effects
#> , , Group 1
#> 
#>             T1[1] T2[1] Growth Odds Ratio Cohen`s h
#> Attribute 1 0.314 0.440  0.126       1.72      0.26
#> Attribute 2 0.394 0.495  0.101       1.51      0.20
#> Attribute 3 0.231 0.622  0.391       5.48      0.81
#> Attribute 4 0.421 0.735  0.314       3.81      0.65
#> 
#> , , Group 2
#> 
#>             T1[1] T2[1] Growth Odds Ratio Cohen`s h
#> Attribute 1 0.298 0.628  0.330       3.98      0.67
#> Attribute 2 0.376 0.735  0.359       4.60      0.74
#> Attribute 3 0.206 0.697  0.491       8.87      1.03
#> Attribute 4 0.446 0.705  0.259       2.97      0.53
#> 
results3$transition.probabilities
#> , , Attribute 1: Time 1 to Time 2, Group 1
#> 
#>        T2 [0] T2 [1]
#> T1 [0]  0.633  0.367
#> T1 [1]  0.400  0.600
#> 
#> , , Attribute 2: Time 1 to Time 2, Group 1
#> 
#>        T2 [0] T2 [1]
#> T1 [0]  0.575  0.425
#> T1 [1]  0.397  0.603
#> 
#> , , Attribute 3: Time 1 to Time 2, Group 1
#> 
#>        T2 [0] T2 [1]
#> T1 [0]  0.433  0.567
#> T1 [1]  0.194  0.806
#> 
#> , , Attribute 4: Time 1 to Time 2, Group 1
#> 
#>        T2 [0] T2 [1]
#> T1 [0]  0.336  0.664
#> T1 [1]  0.166  0.834
#> 
#> , , Attribute 1: Time 1 to Time 2, Group 2
#> 
#>        T2 [0] T2 [1]
#> T1 [0]  0.434  0.566
#> T1 [1]  0.228  0.772
#> 
#> , , Attribute 2: Time 1 to Time 2, Group 2
#> 
#>        T2 [0] T2 [1]
#> T1 [0]  0.364  0.636
#> T1 [1]  0.100  0.900
#> 
#> , , Attribute 3: Time 1 to Time 2, Group 2
#> 
#>        T2 [0] T2 [1]
#> T1 [0]  0.362  0.638
#> T1 [1]  0.074  0.926
#> 
#> , , Attribute 4: Time 1 to Time 2, Group 2
#> 
#>        T2 [0] T2 [1]
#> T1 [0]  0.357  0.643
#> T1 [1]  0.217  0.783
#> 

# plot results
TDCM::tdcm.plot(results3)








#> [1] **Check the plots window for line and bar plots for group growth proportions.

############################################################################
# Example 4: Multigroup TDCM assuming time and group invariance
############################################################################

# Estimate model
mg.model4 <- TDCM::mg.tdcm(data, q.matrix, num.time.points = 2,
                           groups = groups)
#> [1] Preparing data for mg.tdcm()...
#> [1] Estimating the multigroup TDCM in mg.tdcm()...
#> [1] Depending on model complexity, estimation time may vary...
#> [1] Multigroup TDCM estimation complete.
#> [1] Use mg.tdcm.summary() to display results.

# summarize results
results4 <- TDCM::mg.tdcm.summary(mg.model4)
#> [1] Summarizing results...
#> [1] Routine finished. Check results.
results4$item.parameters
#>         λ0     λ1,1  λ1,2  λ1,3  λ1,4  λ2,12 λ2,13 λ2,14 λ2,23 λ2,24 λ2,34
#> Item 1  -1.888 2.393   --    --    --    --    --    --    --    --    -- 
#> Item 2  -2.06  2.572   --    --    --    --    --    --    --    --    -- 
#> Item 3  -2.185 2.633   --    --    --    --    --    --    --    --    -- 
#> Item 4  -2.126 1.778 1.575   --    --  0.688   --    --    --    --    -- 
#> Item 5  -2.072 1.431   --  1.353   --    --  1.328   --    --    --    -- 
#> Item 6  -1.918   --  2.432   --    --    --    --    --    --    --    -- 
#> Item 7  -1.888   --  2.451   --    --    --    --    --    --    --    -- 
#> Item 8  -2.001   --  2.443   --    --    --    --    --    --    --    -- 
#> Item 9  -2.096   --  1.76  1.694   --    --    --    --  0.699   --    -- 
#> Item 10 -2.093   --  1.38    --  1.614   --    --    --    --  1.04    -- 
#> Item 11 -1.973   --    --  2.485   --    --    --    --    --    --    -- 
#> Item 12 -2.191   --    --  2.697   --    --    --    --    --    --    -- 
#> Item 13 -1.893   --    --  2.375   --    --    --    --    --    --    -- 
#> Item 14 -1.941   --    --  1.437 1.236   --    --    --    --    --  1.06 
#> Item 15 -2.172 1.743   --  1.784   --    --  0.683   --    --    --    -- 
#> Item 16 -2.153   --    --    --  2.613   --    --    --    --    --    -- 
#> Item 17 -2.366   --    --    --  3.007   --    --    --    --    --    -- 
#> Item 18 -2.046   --    --    --  2.47    --    --    --    --    --    -- 
#> Item 19 -2.036 1.537   --    --  1.367   --    --  0.922   --    --    -- 
#> Item 20 -2.225   --  1.721   --  1.774   --    --    --    --  0.694   -- 

## Since both time and group invariance are assumed, the item parameters remain the same
## across time and groups.

#             l0  l1,1  l1,2  l1,3  l1,4  l2,12 l2,13 l2,14 l2,23 l2,24 l2,34
#  Item 1  -1.888 2.393   --    --    --    --    --    --    --    --    --
#  Item 2  -2.06  2.572   --    --    --    --    --    --    --    --    --
#  Item 3  -2.185 2.633   --    --    --    --    --    --    --    --    --
#  Item 4  -2.126 1.778 1.575   --    --  0.688   --    --    --    --    --
#  Item 5  -2.072 1.431   --  1.353   --    --  1.328   --    --    --    --
#  Item 6  -1.918   --  2.432   --    --    --    --    --    --    --    --
#  Item 7  -1.888   --  2.451   --    --    --    --    --    --    --    --
#  Item 8  -2.001   --  2.443   --    --    --    --    --    --    --    --
#  Item 9  -2.096   --  1.76  1.694   --    --    --    --  0.699   --    --
#  Item 10 -2.093   --  1.38    --  1.614   --    --    --    --  1.04    --
#  Item 11 -1.973   --    --  2.485   --    --    --    --    --    --    --
#  Item 12 -2.191   --    --  2.697   --    --    --    --    --    --    --
#  Item 13 -1.893   --    --  2.375   --    --    --    --    --    --    --
#  Item 14 -1.941   --    --  1.437 1.236   --    --    --    --    --  1.06
#  Item 15 -2.172 1.743   --  1.784   --    --  0.683   --    --    --    --
#  Item 16 -2.153   --    --    --  2.613   --    --    --    --    --    --
#  Item 17 -2.366   --    --    --  3.007   --    --    --    --    --    --
#  Item 18 -2.046   --    --    --  2.47    --    --    --    --    --    --
#  Item 19 -2.036 1.537   --    --  1.367   --    --  0.922   --    --    --
#  Item 20 -2.225   --  1.721   --  1.774   --    --    --    --  0.694   --
results4$growth
#> , , Group 1
#> 
#>             T1[1] T2[1]
#> Attribute 1 0.307 0.438
#> Attribute 2 0.402 0.500
#> Attribute 3 0.224 0.619
#> Attribute 4 0.424 0.737
#> 
#> , , Group 2
#> 
#>             T1[1] T2[1]
#> Attribute 1 0.301 0.626
#> Attribute 2 0.382 0.737
#> Attribute 3 0.204 0.698
#> Attribute 4 0.448 0.712
#> 
results4$growth.effects
#> , , Group 1
#> 
#>             T1[1] T2[1] Growth Odds Ratio Cohen`s h
#> Attribute 1 0.307 0.438  0.131       1.76      0.27
#> Attribute 2 0.402 0.500  0.098       1.49      0.20
#> Attribute 3 0.224 0.619  0.395       5.63      0.83
#> Attribute 4 0.424 0.737  0.313       3.81      0.65
#> 
#> , , Group 2
#> 
#>             T1[1] T2[1] Growth Odds Ratio Cohen`s h
#> Attribute 1 0.301 0.626  0.325       3.89      0.66
#> Attribute 2 0.382 0.737  0.355       4.53      0.73
#> Attribute 3 0.204 0.698  0.494       9.02      1.04
#> Attribute 4 0.448 0.712  0.264       3.05      0.54
#> 
results4$transition.probabilities
#> , , Attribute 1: Time 1 to Time 2, Group 1
#> 
#>        T2 [0] T2 [1]
#> T1 [0]  0.634  0.366
#> T1 [1]  0.399  0.601
#> 
#> , , Attribute 2: Time 1 to Time 2, Group 1
#> 
#>        T2 [0] T2 [1]
#> T1 [0]  0.571  0.429
#> T1 [1]  0.393  0.607
#> 
#> , , Attribute 3: Time 1 to Time 2, Group 1
#> 
#>        T2 [0] T2 [1]
#> T1 [0]  0.438  0.562
#> T1 [1]  0.185  0.815
#> 
#> , , Attribute 4: Time 1 to Time 2, Group 1
#> 
#>        T2 [0] T2 [1]
#> T1 [0]  0.334  0.666
#> T1 [1]  0.166  0.834
#> 
#> , , Attribute 1: Time 1 to Time 2, Group 2
#> 
#>        T2 [0] T2 [1]
#> T1 [0]  0.435  0.565
#> T1 [1]  0.231  0.769
#> 
#> , , Attribute 2: Time 1 to Time 2, Group 2
#> 
#>        T2 [0] T2 [1]
#> T1 [0]  0.362  0.638
#> T1 [1]  0.104  0.896
#> 
#> , , Attribute 3: Time 1 to Time 2, Group 2
#> 
#>        T2 [0] T2 [1]
#> T1 [0]  0.361  0.639
#> T1 [1]  0.073  0.927
#> 
#> , , Attribute 4: Time 1 to Time 2, Group 2
#> 
#>        T2 [0] T2 [1]
#> T1 [0]  0.353  0.647
#> T1 [1]  0.208  0.792
#> 

############################################################################
# Example 5: Assess measurement invariance
############################################################################

# Compare model 1 (no group invariance) with model 2 (group invariance)
TDCM::tdcm.compare(mg.model1, mg.model2)
#>       Model   loglike Deviance Npars      AIC      BIC  Chisq  df      p
#> 1 mg.model1    -37153 74306.01   734 75774.01 79765.78 117.34 112 0.3462
#> 2 mg.model2 -37211.67 74423.33   622 75667.33 79050.01     NA  NA     NA

# Compare model 1 (no time invariance) with model 3 (time invariance)
TDCM::tdcm.compare(mg.model1, mg.model3)
#>       Model   loglike Deviance Npars      AIC      BIC  Chisq  df      p
#> 1 mg.model1    -37153 74306.01   734 75774.01 79765.78 153.22 112 0.0059
#> 2 mg.model3 -37229.61 74459.22   622 75703.22  79085.9     NA  NA     NA

#############################################################################
# Example 6: DINA multigroup TDCM with time and group invariance assumed
############################################################################

# Estimate model
mg.model6 <- TDCM::mg.tdcm(data, q.matrix, num.time.points = 2,
                           rule = "DINA",
                           groups = groups, time.invariance = TRUE,
                           group.invariance = TRUE)
#> [1] Preparing data for mg.tdcm()...
#> [1] Estimating the multigroup TDCM in mg.tdcm()...
#> [1] Depending on model complexity, estimation time may vary...
#> [1] Multigroup TDCM estimation complete.
#> [1] Use mg.tdcm.summary() to display results.

# summarize results
results6 <- TDCM::mg.tdcm.summary(mg.model6)
#> [1] Summarizing results...
#> [1] Routine finished. Check results.
results6$item.parameters
#>         λ0     λ1,1  λ1,2  λ1,3  λ1,4  λ2,12 λ2,13 λ2,14 λ2,23 λ2,24 λ2,34
#> Item 1  -2.006 2.361   --    --    --    --    --    --    --    --    -- 
#> Item 2  -2.216 2.573   --    --    --    --    --    --    --    --    -- 
#> Item 3  -2.376 2.689   --    --    --    --    --    --    --    --    -- 
#> Item 4  -1.579   --    --    --    --  3.366   --    --    --    --    -- 
#> Item 5  -1.602   --    --    --    --    --  3.367   --    --    --    -- 
#> Item 6  -2.038   --  2.409   --    --    --    --    --    --    --    -- 
#> Item 7  -2.023   --  2.449   --    --    --    --    --    --    --    -- 
#> Item 8  -2.182   --  2.513   --    --    --    --    --    --    --    -- 
#> Item 9  -1.487   --    --    --    --    --    --    --  3.346   --    -- 
#> Item 10 -1.452   --    --    --    --    --    --    --    --  3.153   -- 
#> Item 11 -2.075   --    --  2.417   --    --    --    --    --    --    -- 
#> Item 12 -2.376   --    --  2.731   --    --    --    --    --    --    -- 
#> Item 13 -2.03    --    --  2.39    --    --    --    --    --    --    -- 
#> Item 14 -1.512   --    --    --    --    --    --    --    --    --  3.159
#> Item 15 -1.595   --    --    --    --    --  3.388   --    --    --    -- 
#> Item 16 -2.221   --    --    --  2.633   --    --    --    --    --    -- 
#> Item 17 -2.411   --    --    --  2.985   --    --    --    --    --    -- 
#> Item 18 -2.138   --    --    --  2.518   --    --    --    --    --    -- 
#> Item 19 -1.434   --    --    --    --    --    --  3.061   --    --    -- 
#> Item 20 -1.49    --    --    --    --    --    --    --    --  3.31    -- 
results6$growth
#> , , Group 1
#> 
#>             T1[1] T2[1]
#> Attribute 1 0.357 0.496
#> Attribute 2 0.449 0.557
#> Attribute 3 0.257 0.668
#> Attribute 4 0.449 0.744
#> 
#> , , Group 2
#> 
#>             T1[1] T2[1]
#> Attribute 1 0.337 0.670
#> Attribute 2 0.432 0.770
#> Attribute 3 0.249 0.723
#> Attribute 4 0.465 0.736
#> 
results6$growth.effects
#> , , Group 1
#> 
#>             T1[1] T2[1] Growth Odds Ratio Cohen`s h
#> Attribute 1 0.357 0.496  0.139       1.77      0.28
#> Attribute 2 0.449 0.557  0.108       1.54      0.22
#> Attribute 3 0.257 0.668  0.411       5.82      0.85
#> Attribute 4 0.449 0.744  0.295       3.57      0.61
#> 
#> , , Group 2
#> 
#>             T1[1] T2[1] Growth Odds Ratio Cohen`s h
#> Attribute 1 0.337 0.670  0.333       3.99      0.68
#> Attribute 2 0.432 0.770  0.338       4.40      0.71
#> Attribute 3 0.249 0.723  0.474       7.87      0.99
#> Attribute 4 0.465 0.736  0.271       3.21      0.56
#> 
results6$transition.probabilities
#> , , Attribute 1: Time 1 to Time 2, Group 1
#> 
#>        T2 [0] T2 [1]
#> T1 [0]  0.584  0.416
#> T1 [1]  0.361  0.639
#> 
#> , , Attribute 2: Time 1 to Time 2, Group 1
#> 
#>        T2 [0] T2 [1]
#> T1 [0]  0.525  0.475
#> T1 [1]  0.343  0.657
#> 
#> , , Attribute 3: Time 1 to Time 2, Group 1
#> 
#>        T2 [0] T2 [1]
#> T1 [0]  0.386  0.614
#> T1 [1]  0.178  0.822
#> 
#> , , Attribute 4: Time 1 to Time 2, Group 1
#> 
#>        T2 [0] T2 [1]
#> T1 [0]  0.334  0.666
#> T1 [1]  0.160  0.840
#> 
#> , , Attribute 1: Time 1 to Time 2, Group 2
#> 
#>        T2 [0] T2 [1]
#> T1 [0]  0.399  0.601
#> T1 [1]  0.193  0.807
#> 
#> , , Attribute 2: Time 1 to Time 2, Group 2
#> 
#>        T2 [0] T2 [1]
#> T1 [0]  0.337  0.663
#> T1 [1]  0.089  0.911
#> 
#> , , Attribute 3: Time 1 to Time 2, Group 2
#> 
#>        T2 [0] T2 [1]
#> T1 [0]  0.333  0.667
#> T1 [1]  0.109  0.891
#> 
#> , , Attribute 4: Time 1 to Time 2, Group 2
#> 
#>        T2 [0] T2 [1]
#> T1 [0]  0.345  0.655
#> T1 [1]  0.170  0.830
#> 

#############################################################################
# Example 7: DINO multigroup with time and group invariance assumed
############################################################################

# Estimate model
mg.model7 <- TDCM::mg.tdcm(data, q.matrix, num.time.points = 2,
                           rule = "DINO",
                           groups = groups, time.invariance = TRUE,
                           group.invariance = TRUE)
#> [1] Preparing data for mg.tdcm()...
#> [1] Estimating the multigroup TDCM in mg.tdcm()...
#> [1] Depending on model complexity, estimation time may vary...
#> Warning: NAs introduced by coercion
#> Warning: NAs introduced by coercion
#> Warning: NAs introduced by coercion
#> Warning: NAs introduced by coercion
#> Warning: NAs introduced by coercion
#> Warning: NAs introduced by coercion
#> Warning: NAs introduced by coercion
#> Warning: NAs introduced by coercion
#> Warning: NAs introduced by coercion
#> Warning: NAs introduced by coercion
#> Warning: NAs introduced by coercion
#> Warning: NAs introduced by coercion
#> Warning: NAs introduced by coercion
#> Warning: NAs introduced by coercion
#> Warning: NAs introduced by coercion
#> Warning: NAs introduced by coercion
#> Warning: NAs introduced by coercion
#> Warning: NAs introduced by coercion
#> Warning: NAs introduced by coercion
#> Warning: NAs introduced by coercion
#> Warning: NAs introduced by coercion
#> Warning: NAs introduced by coercion
#> Warning: NAs introduced by coercion
#> Warning: NAs introduced by coercion
#> Warning: NAs introduced by coercion
#> Warning: NAs introduced by coercion
#> Warning: NAs introduced by coercion
#> Warning: NAs introduced by coercion
#> Warning: NAs introduced by coercion
#> Warning: NAs introduced by coercion
#> Warning: NAs introduced by coercion
#> Warning: NAs introduced by coercion
#> [1] Multigroup TDCM estimation complete.
#> [1] Use mg.tdcm.summary() to display results.

# summarize results
results7 <- TDCM::mg.tdcm.summary(mg.model7)
#> [1] Summarizing results...
#> [1] Routine finished. Check results.
results7$item.parameters
#>         λ0     λ1,1  λ1,2  λ1,3  λ1,4  λ2,12 λ2,13 λ2,14 λ2,23 λ2,24 λ2,34
#> Item 1  -1.713 2.417   --    --    --    --    --    --    --    --    -- 
#> Item 2  -1.804 2.456   --    --    --    --    --    --    --    --    -- 
#> Item 3  -1.91  2.508   --    --    --    --    --    --    --    --    -- 
#> Item 4  -1.892   --    --    --    --  2.934   --    --    --    --    -- 
#> Item 5  -2.056   --    --    --    --    --  3.071   --    --    --    -- 
#> Item 6  -1.549   --  2.3     --    --    --    --    --    --    --    -- 
#> Item 7  -1.508   --  2.32    --    --    --    --    --    --    --    -- 
#> Item 8  -1.596   --  2.245   --    --    --    --    --    --    --    -- 
#> Item 9  -1.824   --    --    --    --    --    --    --  3.01    --    -- 
#> Item 10 -1.737   --    --    --    --    --    --    --    --  2.979   -- 
#> Item 11 -1.709   --    --  2.344   --    --    --    --    --    --    -- 
#> Item 12 -1.844   --    --  2.467   --    --    --    --    --    --    -- 
#> Item 13 -1.704   --    --  2.342   --    --    --    --    --    --    -- 
#> Item 14 -1.736   --    --    --    --    --    --    --    --    --  2.79 
#> Item 15 -2.038   --    --    --    --    --  3.067   --    --    --    -- 
#> Item 16 -1.465   --    --    --  2.183   --    --    --    --    --    -- 
#> Item 17 -1.442   --    --    --  2.328   --    --    --    --    --    -- 
#> Item 18 -1.405   --    --    --  2.086   --    --    --    --    --    -- 
#> Item 19 -1.798   --    --    --    --    --    --  2.759   --    --    -- 
#> Item 20 -1.795   --    --    --    --    --    --    --    --  3.109   -- 
results7$growth
#> , , Group 1
#> 
#>             T1[1] T2[1]
#> Attribute 1 0.238 0.391
#> Attribute 2 0.287 0.427
#> Attribute 3 0.180 0.532
#> Attribute 4 0.287 0.584
#> 
#> , , Group 2
#> 
#>             T1[1] T2[1]
#> Attribute 1 0.235 0.580
#> Attribute 2 0.280 0.627
#> Attribute 3 0.175 0.649
#> Attribute 4 0.303 0.592
#> 
results7$growth.effects
#> , , Group 1
#> 
#>             T1[1] T2[1] Growth Odds Ratio Cohen`s h
#> Attribute 1 0.238 0.391  0.153       2.06      0.33
#> Attribute 2 0.287 0.427  0.140       1.85      0.29
#> Attribute 3 0.180 0.532  0.352       5.18      0.76
#> Attribute 4 0.287 0.584  0.297       3.49      0.61
#> 
#> , , Group 2
#> 
#>             T1[1] T2[1] Growth Odds Ratio Cohen`s h
#> Attribute 1 0.235 0.580  0.345       4.50      0.72
#> Attribute 2 0.280 0.627  0.347       4.32      0.71
#> Attribute 3 0.175 0.649  0.474       8.72      1.01
#> Attribute 4 0.303 0.592  0.289       3.34      0.59
#> 
results7$transition.probabilities
#> , , Attribute 1: Time 1 to Time 2, Group 1
#> 
#>        T2 [0] T2 [1]
#> T1 [0]  0.678  0.322
#> T1 [1]  0.388  0.612
#> 
#> , , Attribute 2: Time 1 to Time 2, Group 1
#> 
#>        T2 [0] T2 [1]
#> T1 [0]  0.621  0.379
#> T1 [1]  0.455  0.545
#> 
#> , , Attribute 3: Time 1 to Time 2, Group 1
#> 
#>        T2 [0] T2 [1]
#> T1 [0]  0.520  0.480
#> T1 [1]  0.232  0.768
#> 
#> , , Attribute 4: Time 1 to Time 2, Group 1
#> 
#>        T2 [0] T2 [1]
#> T1 [0]  0.476  0.524
#> T1 [1]  0.267  0.733
#> 
#> , , Attribute 1: Time 1 to Time 2, Group 2
#> 
#>        T2 [0] T2 [1]
#> T1 [0]  0.459  0.541
#> T1 [1]  0.290  0.710
#> 
#> , , Attribute 2: Time 1 to Time 2, Group 2
#> 
#>        T2 [0] T2 [1]
#> T1 [0]  0.465  0.535
#> T1 [1]  0.136  0.864
#> 
#> , , Attribute 3: Time 1 to Time 2, Group 2
#> 
#>        T2 [0] T2 [1]
#> T1 [0]  0.404  0.596
#> T1 [1]  0.102  0.898
#> 
#> , , Attribute 4: Time 1 to Time 2, Group 2
#> 
#>        T2 [0] T2 [1]
#> T1 [0]  0.459  0.541
#> T1 [1]  0.289  0.711
#> 

#############################################################################
# Example 8: CRUM multigroup with time and group invariance assumed
############################################################################

# Estimate model
mg.model8 <- TDCM::mg.tdcm(data, q.matrix, num.time.points = 2,
                           rule = "CRUM",
                           groups = groups, time.invariance = TRUE,
                           group.invariance = TRUE)
#> [1] Preparing data for mg.tdcm()...
#> [1] Estimating the multigroup TDCM in mg.tdcm()...
#> [1] Depending on model complexity, estimation time may vary...
#> [1] Multigroup TDCM estimation complete.
#> [1] Use mg.tdcm.summary() to display results.

# summarize results
results8 <- TDCM::mg.tdcm.summary(mg.model8)
#> [1] Summarizing results...
#> [1] Routine finished. Check results.
results8$item.parameters
#>         λ0     λ1,1  λ1,2  λ1,3  λ1,4  λ2,12 λ2,13 λ2,14 λ2,23 λ2,24 λ2,34
#> Item 1  -1.857 2.399   --    --    --    --    --    --    --    --    -- 
#> Item 2  -2.009 2.549   --    --    --    --    --    --    --    --    -- 
#> Item 3  -2.128 2.6     --    --    --    --    --    --    --    --    -- 
#> Item 4  -2.233 2.226 1.847   --    --    --    --    --    --    --    -- 
#> Item 5  -2.278 2.122   --  1.969   --    --    --    --    --    --    -- 
#> Item 6  -1.899   --  2.434   --    --    --    --    --    --    --    -- 
#> Item 7  -1.854   --  2.43    --    --    --    --    --    --    --    -- 
#> Item 8  -1.976   --  2.432   --    --    --    --    --    --    --    -- 
#> Item 9  -2.202   --  2.064 2.122   --    --    --    --    --    --    -- 
#> Item 10 -2.333   --  2.06    --  2.064   --    --    --    --    --    -- 
#> Item 11 -1.939   --    --  2.489   --    --    --    --    --    --    -- 
#> Item 12 -2.124   --    --  2.654   --    --    --    --    --    --    -- 
#> Item 13 -1.842   --    --  2.338   --    --    --    --    --    --    -- 
#> Item 14 -2.122   --    --  2.235 1.604   --    --    --    --    --    -- 
#> Item 15 -2.241 2.072   --  2.098   --    --    --    --    --    --    -- 
#> Item 16 -2.095   --    --    --  2.567   --    --    --    --    --    -- 
#> Item 17 -2.346   --    --    --  3.014   --    --    --    --    --    -- 
#> Item 18 -2.015   --    --    --  2.457   --    --    --    --    --    -- 
#> Item 19 -2.18  2.162   --    --  1.696   --    --    --    --    --    -- 
#> Item 20 -2.406   --  2.203   --  2.105   --    --    --    --    --    -- 
results8$growth
#> , , Group 1
#> 
#>             T1[1] T2[1]
#> Attribute 1 0.295 0.429
#> Attribute 2 0.393 0.493
#> Attribute 3 0.217 0.605
#> Attribute 4 0.417 0.733
#> 
#> , , Group 2
#> 
#>             T1[1] T2[1]
#> Attribute 1 0.289 0.615
#> Attribute 2 0.378 0.730
#> Attribute 3 0.197 0.691
#> Attribute 4 0.442 0.699
#> 
results8$growth.effects
#> , , Group 1
#> 
#>             T1[1] T2[1] Growth Odds Ratio Cohen`s h
#> Attribute 1 0.295 0.429  0.134       1.80      0.28
#> Attribute 2 0.393 0.493  0.100       1.50      0.20
#> Attribute 3 0.217 0.605  0.388       5.53      0.81
#> Attribute 4 0.417 0.733  0.316       3.84      0.65
#> 
#> , , Group 2
#> 
#>             T1[1] T2[1] Growth Odds Ratio Cohen`s h
#> Attribute 1 0.289 0.615  0.326       3.93      0.67
#> Attribute 2 0.378 0.730  0.352       4.45      0.72
#> Attribute 3 0.197 0.691  0.494       9.12      1.04
#> Attribute 4 0.442 0.699  0.257       2.93      0.53
#> 
results8$transition.probabilities
#> , , Attribute 1: Time 1 to Time 2, Group 1
#> 
#>        T2 [0] T2 [1]
#> T1 [0]  0.642  0.358
#> T1 [1]  0.402  0.598
#> 
#> , , Attribute 2: Time 1 to Time 2, Group 1
#> 
#>        T2 [0] T2 [1]
#> T1 [0]  0.575  0.425
#> T1 [1]  0.402  0.598
#> 
#> , , Attribute 3: Time 1 to Time 2, Group 1
#> 
#>        T2 [0] T2 [1]
#> T1 [0]  0.452  0.548
#> T1 [1]  0.192  0.808
#> 
#> , , Attribute 4: Time 1 to Time 2, Group 1
#> 
#>        T2 [0] T2 [1]
#> T1 [0]  0.336  0.664
#> T1 [1]  0.170  0.830
#> 
#> , , Attribute 1: Time 1 to Time 2, Group 2
#> 
#>        T2 [0] T2 [1]
#> T1 [0]  0.443  0.557
#> T1 [1]  0.240  0.760
#> 
#> , , Attribute 2: Time 1 to Time 2, Group 2
#> 
#>        T2 [0] T2 [1]
#> T1 [0]  0.365  0.635
#> T1 [1]  0.113  0.887
#> 
#> , , Attribute 3: Time 1 to Time 2, Group 2
#> 
#>        T2 [0] T2 [1]
#> T1 [0]  0.368  0.632
#> T1 [1]  0.069  0.931
#> 
#> , , Attribute 4: Time 1 to Time 2, Group 2
#> 
#>        T2 [0] T2 [1]
#> T1 [0]  0.359  0.641
#> T1 [1]  0.228  0.772
#> 

#############################################################################
# Example 9: RRUM multigroup with time and group invariance assumed
############################################################################

# Estimate model
mg.model9 <- TDCM::mg.tdcm(data, q.matrix, num.time.points = 2,
                           rule = "RRUM",
                           groups = groups, time.invariance = TRUE,
                           group.invariance = TRUE)
#> [1] Preparing data for mg.tdcm()...
#> [1] Estimating the multigroup TDCM in mg.tdcm()...
#> [1] Depending on model complexity, estimation time may vary...
#> [1] Multigroup TDCM estimation complete.
#> [1] Use mg.tdcm.summary() to display results.

# summarize results
results9 <- TDCM::mg.tdcm.summary(mg.model9)
#> [1] Summarizing results...
#> [1] Routine finished. Check results.
results9$item.parameters
#>         λ0     λ1,1  λ1,2  λ1,3  λ1,4  λ2,12 λ2,13 λ2,14 λ2,23 λ2,24 λ2,34
#> Item 1  -2.055 1.571   --    --    --    --    --    --    --    --    -- 
#> Item 2  -2.212 1.732   --    --    --    --    --    --    --    --    -- 
#> Item 3  -2.33  1.825   --    --    --    --    --    --    --    --    -- 
#> Item 4  -2.12  1.005 0.979   --    --    --    --    --    --    --    -- 
#> Item 5  -2.19  1.033   --  1.027   --    --    --    --    --    --    -- 
#> Item 6  -2.081   --  1.602   --    --    --    --    --    --    --    -- 
#> Item 7  -2.064   --  1.605   --    --    --    --    --    --    --    -- 
#> Item 8  -2.157   --  1.652   --    --    --    --    --    --    --    -- 
#> Item 9  -2.092   --  1.107 0.866   --    --    --    --    --    --    -- 
#> Item 10 -2.138   --  0.874   --  1.124   --    --    --    --    --    -- 
#> Item 11 -2.126   --    --  1.646   --    --    --    --    --    --    -- 
#> Item 12 -2.337   --    --  1.857   --    --    --    --    --    --    -- 
#> Item 13 -2.061   --    --  1.572   --    --    --    --    --    --    -- 
#> Item 14 -2.078   --    --  0.991 0.929   --    --    --    --    --    -- 
#> Item 15 -2.13  1.001   --  1.008   --    --    --    --    --    --    -- 
#> Item 16 -2.279   --    --    --  1.785   --    --    --    --    --    -- 
#> Item 17 -2.468   --    --    --  2.038   --    --    --    --    --    -- 
#> Item 18 -2.187   --    --    --  1.677   --    --    --    --    --    -- 
#> Item 19 -2.102 0.983   --    --  0.96    --    --    --    --    --    -- 
#> Item 20 -2.13    --  0.921   --  1.076   --    --    --    --    --    -- 
results9$growth
#> , , Group 1
#> 
#>             T1[1] T2[1]
#> Attribute 1 0.317 0.446
#> Attribute 2 0.411 0.512
#> Attribute 3 0.229 0.631
#> Attribute 4 0.430 0.739
#> 
#> , , Group 2
#> 
#>             T1[1] T2[1]
#> Attribute 1 0.310 0.637
#> Attribute 2 0.392 0.745
#> Attribute 3 0.209 0.703
#> Attribute 4 0.453 0.719
#> 
results9$growth.effects
#> , , Group 1
#> 
#>             T1[1] T2[1] Growth Odds Ratio Cohen`s h
#> Attribute 1 0.317 0.446  0.129       1.73      0.27
#> Attribute 2 0.411 0.512  0.101       1.50      0.20
#> Attribute 3 0.229 0.631  0.402       5.76      0.84
#> Attribute 4 0.430 0.739  0.309       3.75      0.64
#> 
#> , , Group 2
#> 
#>             T1[1] T2[1] Growth Odds Ratio Cohen`s h
#> Attribute 1 0.310 0.637  0.327       3.91      0.67
#> Attribute 2 0.392 0.745  0.353       4.53      0.73
#> Attribute 3 0.209 0.703  0.494       8.96      1.04
#> Attribute 4 0.453 0.719  0.266       3.09      0.55
#> 
results9$transition.probabilities
#> , , Attribute 1: Time 1 to Time 2, Group 1
#> 
#>        T2 [0] T2 [1]
#> T1 [0]  0.628  0.372
#> T1 [1]  0.393  0.607
#> 
#> , , Attribute 2: Time 1 to Time 2, Group 1
#> 
#>        T2 [0] T2 [1]
#> T1 [0]  0.563  0.437
#> T1 [1]  0.381  0.619
#> 
#> , , Attribute 3: Time 1 to Time 2, Group 1
#> 
#>        T2 [0] T2 [1]
#> T1 [0]  0.426  0.574
#> T1 [1]  0.178  0.822
#> 
#> , , Attribute 4: Time 1 to Time 2, Group 1
#> 
#>        T2 [0] T2 [1]
#> T1 [0]  0.333  0.667
#> T1 [1]  0.166  0.834
#> 
#> , , Attribute 1: Time 1 to Time 2, Group 2
#> 
#>        T2 [0] T2 [1]
#> T1 [0]  0.425  0.575
#> T1 [1]  0.228  0.772
#> 
#> , , Attribute 2: Time 1 to Time 2, Group 2
#> 
#>        T2 [0] T2 [1]
#> T1 [0]  0.357  0.643
#> T1 [1]  0.097  0.903
#> 
#> , , Attribute 3: Time 1 to Time 2, Group 2
#> 
#>        T2 [0] T2 [1]
#> T1 [0]  0.354  0.646
#> T1 [1]  0.080  0.920
#> 
#> , , Attribute 4: Time 1 to Time 2, Group 2
#> 
#>        T2 [0] T2 [1]
#> T1 [0]  0.351  0.649
#> T1 [1]  0.197  0.803
#> 

#############################################################################
# Example 10: Multigroup TDCM with and without forgetting
############################################################################

##----------------------------------------------------------------------------
# With forgetting
#----------------------------------------------------------------------------
## Consider a default model in which students can retain or lose their mastery status
## from one time point to another

# Estimate model
mg.model10_forgetting <- TDCM::mg.tdcm(data, q.matrix, num.time.points = 2,
                           rule = "LCDM",
                           groups = groups, time.invariance = TRUE,
                           group.invariance = TRUE)
#> [1] Preparing data for mg.tdcm()...
#> [1] Estimating the multigroup TDCM in mg.tdcm()...
#> [1] Depending on model complexity, estimation time may vary...
#> [1] Multigroup TDCM estimation complete.
#> [1] Use mg.tdcm.summary() to display results.

# Summarize results with mg.tdcm.summary().
results_forgetting <- TDCM::mg.tdcm.summary(mg.model10_forgetting,transition.option = 1)
#> [1] Summarizing results...
#> [1] Routine finished. Check results.
results_forgetting$transition.probabilities
#> , , Attribute 1: Time 1 to Time 2, Group 1
#> 
#>        T2 [0] T2 [1]
#> T1 [0]  0.634  0.366
#> T1 [1]  0.399  0.601
#> 
#> , , Attribute 2: Time 1 to Time 2, Group 1
#> 
#>        T2 [0] T2 [1]
#> T1 [0]  0.571  0.429
#> T1 [1]  0.393  0.607
#> 
#> , , Attribute 3: Time 1 to Time 2, Group 1
#> 
#>        T2 [0] T2 [1]
#> T1 [0]  0.438  0.562
#> T1 [1]  0.185  0.815
#> 
#> , , Attribute 4: Time 1 to Time 2, Group 1
#> 
#>        T2 [0] T2 [1]
#> T1 [0]  0.334  0.666
#> T1 [1]  0.166  0.834
#> 
#> , , Attribute 1: Time 1 to Time 2, Group 2
#> 
#>        T2 [0] T2 [1]
#> T1 [0]  0.435  0.565
#> T1 [1]  0.231  0.769
#> 
#> , , Attribute 2: Time 1 to Time 2, Group 2
#> 
#>        T2 [0] T2 [1]
#> T1 [0]  0.362  0.638
#> T1 [1]  0.104  0.896
#> 
#> , , Attribute 3: Time 1 to Time 2, Group 2
#> 
#>        T2 [0] T2 [1]
#> T1 [0]  0.361  0.639
#> T1 [1]  0.073  0.927
#> 
#> , , Attribute 4: Time 1 to Time 2, Group 2
#> 
#>        T2 [0] T2 [1]
#> T1 [0]  0.353  0.647
#> T1 [1]  0.208  0.792
#> 
# , , Attribute 1: Time 1 to Time 2, Group 1
#
#         T2 [0] T2 [1]
# T1 [0]  0.634  0.366
# T1 [1]  0.399  0.601
#
# , , Attribute 2: Time 1 to Time 2, Group 1
#
#         T2 [0] T2 [1]
# T1 [0]  0.571  0.429
# T1 [1]  0.393  0.607
#
# , , Attribute 3: Time 1 to Time 2, Group 1
#
#         T2 [0] T2 [1]
# T1 [0]  0.438  0.562
# T1 [1]  0.185  0.815
#
# , , Attribute 4: Time 1 to Time 2, Group 1
#
#         T2 [0] T2 [1]
# T1 [0]  0.334  0.666
# T1 [1]  0.166  0.834
#
# , , Attribute 1: Time 1 to Time 2, Group 2
#
#         T2 [0] T2 [1]
# T1 [0]  0.435  0.565
# T1 [1]  0.231  0.769
#
# , , Attribute 2: Time 1 to Time 2, Group 2
#
#         T2 [0] T2 [1]
# T1 [0]  0.362  0.638
# T1 [1]  0.104  0.896
#
# , , Attribute 3: Time 1 to Time 2, Group 2
#
#         T2 [0] T2 [1]
# T1 [0]  0.361  0.639
# T1 [1]  0.073  0.927
#
# , , Attribute 4: Time 1 to Time 2, Group 2
#
#         T2 [0] T2 [1]
# T1 [0]  0.353  0.647
# T1 [1]  0.208  0.792

##----------------------------------------------------------------------------
# Without forgetting
#----------------------------------------------------------------------------
## Consider a model in which students cannot lose their mastery status for attribute 4
## from one time point to another.

# Estimate  model
mg.model10_noforgetting <- TDCM::mg.tdcm(data, q.matrix, num.time.points = 2,
                                      rule = "LCDM",
                                      groups = groups, time.invariance = TRUE,
                                      group.invariance = TRUE,
                                      forget.at=c(4))
#> [1] Preparing data for mg.tdcm()...
#> [1] Estimating the multigroup TDCM in mg.tdcm()...
#> [1] Depending on model complexity, estimation time may vary...
#> [1] Multigroup TDCM estimation complete.
#> [1] Use mg.tdcm.summary() to display results.

# Summarize results with mg.tdcm.summary().
results_noforgetting <- TDCM::mg.tdcm.summary(mg.model10_noforgetting,transition.option = 1 )
#> [1] Summarizing results...
#> [1] Routine finished. Check results.
results_noforgetting$transition.probabilities
#> , , Attribute 1: Time 1 to Time 2, Group 1
#> 
#>        T2 [0] T2 [1]
#> T1 [0]  0.635  0.365
#> T1 [1]  0.396  0.604
#> 
#> , , Attribute 2: Time 1 to Time 2, Group 1
#> 
#>        T2 [0] T2 [1]
#> T1 [0]  0.570  0.430
#> T1 [1]  0.406  0.594
#> 
#> , , Attribute 3: Time 1 to Time 2, Group 1
#> 
#>        T2 [0] T2 [1]
#> T1 [0]  0.435  0.565
#> T1 [1]  0.199  0.801
#> 
#> , , Attribute 4: Time 1 to Time 2, Group 1
#> 
#>        T2 [0] T2 [1]
#> T1 [0]  0.376  0.624
#> T1 [1]  0.000  1.000
#> 
#> , , Attribute 1: Time 1 to Time 2, Group 2
#> 
#>        T2 [0] T2 [1]
#> T1 [0]  0.435  0.565
#> T1 [1]  0.241  0.759
#> 
#> , , Attribute 2: Time 1 to Time 2, Group 2
#> 
#>        T2 [0] T2 [1]
#> T1 [0]  0.365  0.635
#> T1 [1]  0.122  0.878
#> 
#> , , Attribute 3: Time 1 to Time 2, Group 2
#> 
#>        T2 [0] T2 [1]
#> T1 [0]  0.361  0.639
#> T1 [1]  0.075  0.925
#> 
#> , , Attribute 4: Time 1 to Time 2, Group 2
#> 
#>        T2 [0] T2 [1]
#> T1 [0]  0.415  0.585
#> T1 [1]  0.000  1.000
#> 
# , , Attribute 1: Time 1 to Time 2, Group 1
#
#         T2 [0] T2 [1]
# T1 [0]  0.635  0.365
# T1 [1]  0.396  0.604
#
# , , Attribute 2: Time 1 to Time 2, Group 1
#
#         T2 [0] T2 [1]
# T1 [0]  0.570  0.430
# T1 [1]  0.406  0.594
#
# , , Attribute 3: Time 1 to Time 2, Group 1
#
#         T2 [0] T2 [1]
# T1 [0]  0.435  0.565
# T1 [1]  0.199  0.801
#
# , , Attribute 4: Time 1 to Time 2, Group 1
#
#         T2 [0] T2 [1]
# T1 [0]  0.376  0.624
# T1 [1]  0.000  1.000
#
# , , Attribute 1: Time 1 to Time 2, Group 2
#
#         T2 [0] T2 [1]
# T1 [0]  0.435  0.565
# T1 [1]  0.241  0.759
#
# , , Attribute 2: Time 1 to Time 2, Group 2
#
#         T2 [0] T2 [1]
# T1 [0]  0.365  0.635
# T1 [1]  0.122  0.878
#
# , , Attribute 3: Time 1 to Time 2, Group 2
#
#        T2 [0] T2 [1]
# T1 [0]  0.361  0.639
# T1 [1]  0.075  0.925
#
# , , Attribute 4: Time 1 to Time 2, Group 2
#
#         T2 [0] T2 [1]
# T1 [0]  0.415  0.585
# T1 [1]  0.000  1.000
# }
```
