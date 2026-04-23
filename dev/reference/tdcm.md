# Estimating the Transition Diagnostic Classification Model (TDCM)

`tdcm()` estimates the transition diagnostic classification model (TDCM;
Madison & Bradshaw, 2018a), which is a longitudinal extension of the
log-linear cognitive diagnosis model (LCDM; Henson, Templin, & Willse,
2009). For the multigroup TDCM, see [`mg.tdcm()`](mg.tdcm.md). This
function supports the estimation of various longitudinal DCMs by
allowing different rule specifications via the `rule` option and link
functions via the `linkfct` option, with LCDM as the default rule and
link function. The rule can be modified to estimate the DINA model, DINO
model, CRUM (i.e., ACDM, or main effects model), or reduced interaction
versions of the LCDM. Additionally, the link function can be adjusted to
specify the GDINA model.

## Usage

``` r
tdcm(
  data,
  q.matrix,
  num.time.points,
  invariance = TRUE,
  rule = "LCDM",
  linkfct = "logit",
  num.q.matrix = 1,
  num.items = c(),
  anchor = c(),
  forget.att = c(),
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
  which attributes. If there are multiple Q-matrices, then they must
  have the same number of attributes and must be stacked on top of each
  other for estimation (to specify multiple Q-matrices, see
  `num.q.matrix`, `num.items`, and `anchor`).

- num.time.points:

  A required integer \\\ge 2\\ specifying the number of time points
  (i.e., measurement occasions).

- invariance:

  logical. If `TRUE` (the default), then item parameters will be
  constrained to be equal at each time point. If `FALSE`, item
  parameters are not assumed to be equal over time.

- rule:

  A `string` or a `vector` indicating the specific DCM to be employed. A
  vector of supported `rule` values is provided by
  [tdcm.rules](tdcm.rules.md). Currently accepted values are: "LCDM",
  "DINA", "DINO", "CRUM", "RRUM", "LCDM1" for the LCDM with only main
  effects, "LCDM2" for the LCDM with two-way interactions, "LCDM3", and
  so on. If `rule` is supplied as a single string, then that DCM will be
  assumed for each item. If entered as a vector, a rule can be specified
  for each item. The rule vector must have length equal to the total
  number of items across all time points.

- linkfct:

  A `string` or a `vector` indicating the LCDM link function. Currently
  accepts "logit" (default) to estimate the LCDM, "identity" to estimate
  the GDINA model, and "log" link function to estimate the reduced
  reparameterized unified model (RRUM). The link function vector must
  have length equal to the total number of items across all time points.

- num.q.matrix:

  An optional integer specifying the number of Q-matrices. For many
  applications, the same assessment is administered at each time point
  and this number is 1 (the default). If there are different Q-matrices
  for each time point, then this argument must be specified and should
  be equal to the number of time points. For example, if there are three
  time points, and the Q-matrices for each time point are different,
  then `num.q.matrix = 3`. If there are three time points, and the
  Q-matrix is different only for time point 3, then `num.q.matrix` is
  still specified as `3`.

- num.items:

  An integer specifying the number of items. When there are multiple
  Q-matrices, the number of items in each Q-matrix is specified as a
  vector. For example, if there are three time points, and the
  Q-matrices for each time point have 8, 10, and 12 items, respectively.
  Then `num.items = c(8, 10, 12)`.

- anchor:

  An optional `vector` specifying how items are linked across time
  points to maintain item invariance when different tests are
  administered. By default, `anchor` is an empty `vector`, indicating
  the absence of anchor items. **Note:** When `anchor` is specified,
  invariance is automatically set to `FALSE` for non-anchor items. Each
  pair in the `anchor` vector consists of a **reference item** and a
  **linked item**, where the linked item is mapped to its corresponding
  reference item. The reference item does not necessarily appear in the
  first test; it can be from any time point.

  **Example:** Suppose we have three different 10-item tests with their
  corresponding Q-matrices. However, some items remain the same across
  time points: - **Item 1** (from the first test), **Item 11** (from the
  second test), and **Item 21** (from the third test) correspond to the
  same item. Since **Item 1** serves as the reference, **Items 11** and
  **21** can be linked to it using: `anchor = c(1, 11, 1, 21)` - If we
  additionally assume that **Item 14** (from the second test) and **Item
  24** (from the third test) correspond to the same item, **Item 14**
  serves as the reference, and **Item 24** is linked to it. Thus, the
  final anchor vector is specified as:
  `anchor = c(1, 11, 1, 21, 14, 24)`

- forget.att:

  An optional vector allowing for constraining of individual attribute
  proficiency loss, or forgetting.

  - By default, forgetting is allowed for all measured attributes,
    meaning that probability of transitioning from mastery to
    non-mastery can be different than zero (\\P(1 \rightarrow 0) \neq
    0\\). - If a vector of attributes is provided, \\P(1 \rightarrow 0)
    = 0\\ for those specific attributes, meaning that forgetting is not
    permitted. For example, if `forget.att= c(2,4)`, then forgetting for
    Attributes 2 and 4 is not allowed, while other attributes can
    exhibit forgetting.

- progress:

  logical. If `FALSE`, the function will print the progress of
  estimation. If `TRUE` (default), no progress information is printed.

## Value

An object of class `gdina` with entries as described in
[`CDM::gdina()`](https://rdrr.io/pkg/CDM/man/gdina.html). To see a
TDCM-specific summary of the object (e.g.,growth, transitions), use
[`tdcm.summary()`](tdcm.summary.md).

## Details

The Transition Diagnostic Classification Model (TDCM) is a confirmatory
and constrained latent transition model that measures examinees' growth
or decline in attribute mastery over time (Madison & Bradshaw, 2018a).
Assume that \\X\_{eit}\\ corresponds to the binary response of examinee
\\e \in \\1, \dots, N\\\\ to item \\i \in \\1, \dots, I\\\\ across time
points \\t \in \\1, \dots, T\\\\, and \\A_t\\ denotes the number of
attributes measured at time \\t\\. The probability of the item response
vector \\X_e = (x\_{e11}, x\_{e12}, \dots, x\_{e1I}, x\_{e21}, \dots,
x\_{eTI})\\ is given by:

\$\$ P(X_e = x_e) = \sum\_{c_1=1}^{C} \sum\_{c_2=1}^{C} \cdots
\sum\_{c_T=1}^{C} v\_{c_1} \tau\_{c_2 \| c_1} \tau\_{c_3 \| c_2} \cdots
\tau\_{c_T \| c\_{T-1}} \prod\_{t=1}^{T} \prod\_{i=1}^{I} \pi\_{i
c_t}^{x\_{eit}} (1 - \pi\_{i c_t})^{1 - x\_{eit}}, \$\$

where:

- \\v\_{c_1}\\ represents the probability of belonging to attribute
  profile \\c\\ at time 1.

- \\\tau\_{c_t \| c\_{t-1}}\\ represents the probability of
  transitioning attribute profiles from time point \\t-1\\ to time point
  \\t\\.

- \\\pi\_{ic_t}\\ is the item response function, which models the
  probability of answering item \\i\\ correctly at time \\t\\ given
  attribute profile \\c\\.

### Model Assumptions and Variations\*\*

#### Accounting for Measurement Invariance

Measurement invariance indicates whether the **item response function**
remains **consistent over time** or changes across time points.
Depending on the testing conditions, different measurement invariance
assumptions can be assumed:

#### No Measurement Invariance

If measurement invariance is **not** assumed, each item has a
**different** response function over time: \\\pi\_{i c_1} \neq \pi\_{i
c_2} \neq \dots \neq \pi\_{i c_T}\\. Thus, the probability of the item
response vector is:

\$\$ P(X_e = x_e) = \sum\_{c_1=1}^{C} \sum\_{c_2=1}^{C} \cdots
\sum\_{c_T=1}^{C} v\_{c_1} \tau\_{c_2 \| c_1} \tau\_{c_3 \| c_2} \cdots
\tau\_{c_T \| c\_{T-1}} \prod\_{t=1}^{T} \prod\_{i=1}^{I} \pi\_{i
c_t}^{x\_{eit}} (1 - \pi\_{i c_t})^{1 - x\_{eit}}, \$\$

and

\$\$ \pi\_{ic_t} = P(X\_{ic_t} = 1\|\alpha\_{c_t}) =
\frac{exp(\lambda\_{i,0}+ \boldsymbol{\lambda\_{i}^{(t)T}}
\boldsymbol{h(\alpha\_{c_t}, q_i^{(t)})})}{1 + exp(\lambda\_{i,0}+
\boldsymbol{\lambda\_{i}^{(t)T}} \boldsymbol{h(\alpha\_{c_t},
q_i^{(t)})})}, \$\$

where:

- \\q_i^{(t)}\\ is the q-matrix for item \\i\\ at time point \\t\\.

- \\\lambda\_{i,0}\\ is the intercept parameter for item \\i\\ and
  corresponds to the logit of a correct response when none of the
  attributes in the Q-matrix are mastered.

- \\\boldsymbol{\lambda_i}^{(t)}\\ is a column vector of main and
  interaction effects for item \\i\\ at time point \\t\\.

- \\\boldsymbol{h(\alpha\_{c_t}, q_i^{(t)})}\\ is a function mapping the
  attribute profile \\\alpha\_{c_t}\\ and the Q-matrix for item \\i\\ at
  time point \\t\\.

#### Full Measurement Invariance

If measurement invariance **is** assumed (default option), items
maintain a **constant response function across time**: \\\forall i \in
I, \pi\_{i c_1}=\pi\_{i c_2} = \dots = \pi\_{i c_T}\\, \\\forall t \in
T\\. Therefore, the probability of the item response vector simplifies
the to:

\$\$ P(X_e = x_e) = \sum\_{c_1=1}^{C} \sum\_{c_2=1}^{C} \cdots
\sum\_{c_T=1}^{C} v\_{c_1} \tau\_{c_3 \| c_2} \cdots \tau\_{c_T \|
c\_{T-1}} \prod\_{t=1}^{T} \prod\_{i=1}^{I} \pi\_{i c}^{x\_{eit}} (1 -
\pi\_{i c})^{1 - x\_{eit}}, \$\$

and

\$\$ \pi\_{ic} = P(X\_{ic} = 1\|\alpha\_{c}) = \frac{exp(\lambda\_{i,0}+
\boldsymbol{\lambda\_{i}^T} \boldsymbol{h(\alpha\_{c}, q_i)})}{1 +
exp(\lambda\_{i,0}+ \boldsymbol{\lambda\_{i}^T}
\boldsymbol{h(\alpha\_{c}, q_i)})}, \$\$

where:

- \\q_i\\ is the Q-matrix for item \\i\\. Recall that as item are the
  same across time points and measurement invariance is assumed, the
  Q-matrix should also remain the same across time.

- \\\lambda\_{i,0}\\ is the intercept parameter for item \\i\\.

- \\\boldsymbol{h(\alpha\_{c_t}, q_i)}\\ is a function mapping the
  attribute profile \\\alpha\_{c}\\ and the item Q-matrix.

#### Partial Measurement Invariance

When measurement invariance is **partially** assumed, some items (anchor
items) maintain the same item response function across time points,
while others (non-anchor items) vary over time.

Assume that \\i \in B\\ are **anchor items**, such that \\\forall i \in
B, \forall t \in T, \pi\_{i c_1}=\pi\_{i c_2} = \dots = \pi\_{i c_T}\\.
This implies that anchor items measure the same attributes across time
and their corresponding Q-matrix entries remain unchanged.

Assume also that \\i \in Z\\ are **non-anchor items**, such that
\\\forall i \in Z, \forall t \in \\2, \dots, T\\, \pi\_{i c_t} \neq
\pi\_{i c\_{t-1}}\\. This means that non-anchor items may change across
time or measure different attributes, leading to changes in their
corresponding Q-matrix entries. Then, the probability of the item
response vector is:

\$\$ P(X_e = x_e) = \sum\_{c_1=1}^{C} \sum\_{c_2=1}^{C} \cdots
\sum\_{c_T=1}^{C} v\_{c_1} \tau\_{c_2 \| c_1} \tau\_{c_3 \| c_2} \cdots
\tau\_{c_T \| c\_{T-1}} \prod\_{t=1}^{T} \prod\_{i \in B} \pi\_{i
c}^{x\_{eit}} (1 - \pi\_{i c})^{1 - x\_{eit}} \prod\_{t=1}^{T} \prod\_{i
\in Z} \pi\_{i c_t}^{x\_{eit}} (1 - \pi\_{i c_t})^{1 - x\_{eit}}. \$\$

### Modeling Forgetting in Attribute Transitions

Unlike standard latent transition models that assume monotonic
learning,TDCM allows for **both mastery acquisition and forgetting**. By
default, TDCM does not impose that mastery must always increase over
time. Instead, the transition probabilities \\\tau\_{c_t \| c\_{t-1}}\\
for examinee \\e\\ can represent a transition from:

- A transition from non-mastery status to master attribute status
  (learning).

- A transition from master attribute status to a non-mastery status
  (forgetting).

However, TDCM also allows for attribute-specific constrains, enabling to
restrict transition probabilities for certain attributes.

### Special Cases

In TDCM, the item response function \\\pi\_{ic\_{t}}\\ is parameterized
using the LCDM. LCDM is a general and flexible model that allows special
models to be derived by constraining specific parameters.

### **DINA Model**

The DINA model is a non-compensatory DCM, meaning that examinees can
correctly answer to an item only if they have mastered all attributes
required by that item. Given this characteristic, the DINA model is
derived by constraining the main effects of the LCDM to zero, such that
only the highest-order interaction term influences the item response
probability.

#### *Example*

Suppose item 1 measures Attributes 1 and 2, and item invariance is
assumed across time points. The item response function for item 1
following the LCDM can be expressed as:

\$\$ \pi\_{1c} = P(X\_{1c} = 1\|\alpha\_{c}) = \frac{exp(\lambda\_{1,0}+
\lambda\_{1,1(1)}\alpha\_{c1} + \lambda\_{1,1(2)}\alpha\_{c2} +
\lambda\_{1,2(1,2)}\alpha\_{c1}\alpha\_{c2})}{1 + exp(\lambda\_{1,0}+
\lambda\_{1,1(1)}\alpha\_{c1} + \lambda\_{1,1(2)}\alpha\_{c2} +
\lambda\_{1,2(1,2)}\alpha\_{c1}\alpha\_{c2})}, \$\$

Then, the DINA model is obtained by constraining the LCDM main effects
to zero, resulting in:

\$\$ \pi\_{1c} = P(X\_{1c} = 1\|\alpha\_{c}) = \frac{exp(\lambda\_{1,0}+
\lambda\_{1,2(1,2)}\alpha\_{c1}\alpha\_{c2})}{1 + exp(\lambda\_{1,0}+
\lambda\_{1,2(1,2)}\alpha\_{c1}\alpha\_{c2})}. \$\$

#### DINO Model

The DINO model is a compensatory DCM, meaning that examinees can
correctly answer an item if they have mastered at least one of the
attributes required by that item. Consequently, the main and interaction
terms in the LCDM are constrained to be equal, and we subtract the
interaction term to ensure the item response probability remains
unchanged when multiple attributes are mastered. Following the previous
example, the DINO model can be expressed as:

\$\$ \pi\_{1c} = P(X\_{1c} = 1\|\alpha\_{c}) = \frac{exp(\lambda\_{1,0}+
(\lambda\_{1,1(1)}\alpha\_{c1} + \lambda\_{1,1(2)}\alpha\_{c2} -
\lambda\_{1,2(1,2)}\alpha\_{c1}\alpha\_{c2}))}{1 + exp(\lambda\_{1,0}+
(\lambda\_{1,1(1)}\alpha\_{c1} + \lambda\_{1,1(2)}\alpha\_{c2} -
\lambda\_{1,2(1,2)}\alpha\_{c1}\alpha\_{c2}))}. \$\$

#### CRUM Model

The CRUM is a compensatory DCM where each attribute independently
contributes to the probability of a correct response. Unlike the DINO
model, mastering multiple attributes neither penalizes nor provides an
additional advantage. Thus, the probability of a correct response is
determined solely by the sum of individual main effects, constraining
the interaction term to zero.

Following the previous example, the CRUM model can be expressed as:

\$\$ \pi\_{1c} = P(X\_{1c} = 1\|\alpha\_{c}) = \frac{exp(\lambda\_{1,0}+
\lambda\_{1,1(1)}\alpha\_{c1} + \lambda\_{1,1(2)}\alpha\_{c2} )}{1 +
exp(\lambda\_{1,0}+ \lambda\_{1,1(1)}\alpha\_{c1} +
\lambda\_{1,1(2)}\alpha\_{c2} )}. \$\$

#### Estimation Methods

Estimation of the TDCM via the CDM package (George, et al., 2016), which
is based on an EM algorithm as described in de la Torre (2011). The
estimation approach is further detailed in Madison et al. (2023).

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
# Example 1: TDCM with full measurement invariance
############################################################################

# Load dataset: T=2, A=4
data(data.tdcm01, package = "TDCM")
data <- data.tdcm01$data
q.matrix <- data.tdcm01$q.matrix
# Estimate model
model1 <- TDCM::tdcm(data, q.matrix, num.time.points = 2, invariance = TRUE,
                    rule = "LCDM", num.q.matrix = 1)
#> [1] Preparing data for tdcm()...
#> [1] Estimating the TDCM in tdcm()...
#> [1] Depending on model complexity, estimation time may vary...
#> [1] TDCM estimation complete.
#> [1] Use tdcm.summary() to display results.
# Summarize results with tdcm.summary().
results <- TDCM::tdcm.summary(model1)
#> [1] Summarizing results...
#> [1] Routine finished. Check results.
results$item.parameters
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
results$growth
#>             T1[1] T2[1]
#> Attribute 1 0.190 0.370
#> Attribute 2 0.317 0.491
#> Attribute 3 0.392 0.579
#> Attribute 4 0.242 0.693
results$growth.effects
#>             T1[1] T2[1] Growth Odds Ratio Cohen`s h
#> Attribute 1 0.190 0.370  0.180       2.50      0.41
#> Attribute 2 0.317 0.491  0.174       2.08      0.36
#> Attribute 3 0.392 0.579  0.187       2.13      0.38
#> Attribute 4 0.242 0.693  0.451       7.07      0.94
results$transition.probabilities
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

#############################################################################
# Example 2: TDCM with no measurement invariance
############################################################################

# Load dataset: T=2, A=4
data(data.tdcm01, package = "TDCM")
data <- data.tdcm01$data
q.matrix <- data.tdcm01$q.matrix
# Estimate model
model2 <- TDCM::tdcm(data, q.matrix, num.time.points = 2, invariance = FALSE,
                    rule = "LCDM", num.q.matrix = 1)
#> [1] Preparing data for tdcm()...
#> [1] Estimating the TDCM in tdcm()...
#> [1] Depending on model complexity, estimation time may vary...
#> [1] TDCM estimation complete.
#> [1] Use tdcm.summary() to display results.
# Summarize results with tdcm.summary().
results2 <- TDCM::tdcm.summary(model2)
#> [1] Summarizing results...
#> [1] Routine finished. Check results.
results2$item.parameters
#>         λ0     λ1,1   λ1,2  λ1,3  λ1,4  λ2,12 λ2,13 λ2,14  λ2,23 λ2,24  λ2,34
#> Item 1  -1.921 2.691    --    --    --    --    --    --     --    --     -- 
#> Item 2  -2.058 2.586    --    --    --    --    --    --     --    --     -- 
#> Item 3  -1.968 2.529    --    --    --    --    --    --     --    --     -- 
#> Item 4  -2.062 1.688  1.741   --    --  0.246   --    --     --    --     -- 
#> Item 5  -2.129 1.504    --  1.831   --    --  0.99    --     --    --     -- 
#> Item 6  -1.911   --   2.365   --    --    --    --    --     --    --     -- 
#> Item 7  -1.839   --   2.31    --    --    --    --    --     --    --     -- 
#> Item 8  -1.988   --   2.569   --    --    --    --    --     --    --     -- 
#> Item 9  -2.29    --   1.799 1.851   --    --    --    --   1.075   --     -- 
#> Item 10 -2.012   --   1.703   --  1.391   --    --    --     --  1.222    -- 
#> Item 11 -1.929   --     --  2.467   --    --    --    --     --    --     -- 
#> Item 12 -2.038   --     --  2.6     --    --    --    --     --    --     -- 
#> Item 13 -1.886   --     --  2.391   --    --    --    --     --    --     -- 
#> Item 14 -1.968   --     --  1.73  1.995   --    --    --     --    --   0.653
#> Item 15 -1.894 -4.136   --  1.509   --    --  6.788   --     --    --     -- 
#> Item 16 -2.032   --     --    --  2.66    --    --    --     --    --     -- 
#> Item 17 -2.047   --     --    --  2.585   --    --    --     --    --     -- 
#> Item 18 -2.028   --     --    --  2.487   --    --    --     --    --     -- 
#> Item 19 -2.144 2.932    --    --  1.486   --    --  -0.07    --    --     -- 
#> Item 20 -2.129   --   1.772   --  1.845   --    --    --     --  0.865    -- 
#> Item 21 -1.861 2.492    --    --    --    --    --    --     --    --     -- 
#> Item 22 -2.05  2.45     --    --    --    --    --    --     --    --     -- 
#> Item 23 -1.871 2.475    --    --    --    --    --    --     --    --     -- 
#> Item 24 -1.739 0.471  1.269   --    --  1.919   --    --     --    --     -- 
#> Item 25 -2.16  1.323    --  1.743   --    --  1.1     --     --    --     -- 
#> Item 26 -1.801   --   2.042   --    --    --    --    --     --    --     -- 
#> Item 27 -1.838   --   2.206   --    --    --    --    --     --    --     -- 
#> Item 28 -2.005   --   2.47    --    --    --    --    --     --    --     -- 
#> Item 29 -1.721   --   0.285 1.092   --    --    --    --   2.758   --     -- 
#> Item 30 -2.011   --   2.294   --  1.275   --    --    --     --  0.627    -- 
#> Item 31 -1.738   --     --  2.205   --    --    --    --     --    --     -- 
#> Item 32 -2.011   --     --  2.495   --    --    --    --     --    --     -- 
#> Item 33 -2.253   --     --  2.727   --    --    --    --     --    --     -- 
#> Item 34 -2.235   --     --  1.68  2.182   --    --    --     --    --   0.365
#> Item 35 -1.674 0.857    --  1.119   --    --  1.896   --     --    --     -- 
#> Item 36 -2.512   --     --    --  3.103   --    --    --     --    --     -- 
#> Item 37 -2.183   --     --    --  2.85    --    --    --     --    --     -- 
#> Item 38 -2.173   --     --    --  2.534   --    --    --     --    --     -- 
#> Item 39 -2.081 2.803    --    --  1.445   --    --  -0.227   --    --     -- 
#> Item 40 -1.907   --   3.754   --  1.177   --    --    --     --  -1.099   -- 
results2$growth
#>             T1[1] T2[1]
#> Attribute 1 0.186 0.369
#> Attribute 2 0.326 0.493
#> Attribute 3 0.383 0.579
#> Attribute 4 0.237 0.700
results2$growth.effects
#>             T1[1] T2[1] Growth Odds Ratio Cohen`s h
#> Attribute 1 0.186 0.369  0.183       2.56      0.41
#> Attribute 2 0.326 0.493  0.167       2.01      0.34
#> Attribute 3 0.383 0.579  0.196       2.22      0.39
#> Attribute 4 0.237 0.700  0.463       7.51      0.97
results2$transition.probabilities
#> , , Attribute 1: Time 1 to Time 2
#> 
#>        T2 [0] T2 [1]
#> T1 [0]  0.683  0.317
#> T1 [1]  0.402  0.598
#> 
#> , , Attribute 2: Time 1 to Time 2
#> 
#>        T2 [0] T2 [1]
#> T1 [0]  0.580  0.420
#> T1 [1]  0.356  0.644
#> 
#> , , Attribute 3: Time 1 to Time 2
#> 
#>        T2 [0] T2 [1]
#> T1 [0]  0.542  0.458
#> T1 [1]  0.225  0.775
#> 
#> , , Attribute 4: Time 1 to Time 2
#> 
#>        T2 [0] T2 [1]
#> T1 [0]  0.364  0.636
#> T1 [1]  0.094  0.906
#> 


#############################################################################
# Example 3: TDCM with different Q-matrices for each time point and no
# anchor items
############################################################################

# Load dataset: T=3, A=2
data(data.tdcm03, package = "TDCM")
data <- data.tdcm03$data
q1 <- data.tdcm03$q.matrix.1
q2 <- data.tdcm03$q.matrix.2
q3 <- data.tdcm03$q.matrix.3
q <- data.tdcm03$q.matrix.stacked

# Estimate model
model3 <- TDCM::tdcm(data, q, num.time.points = 3, rule = "LCDM",
                    num.q.matrix = 3, num.items = c(10, 10, 10))
#> [1] Preparing data for tdcm()...
#> [1] Estimating the TDCM in tdcm()...
#> [1] Depending on model complexity, estimation time may vary...
#> [1] TDCM estimation complete.
#> [1] Use tdcm.summary() to display results.

#----------------------------------------------------------------------------
# Summarize results with tdcm.summary() for more than 2 time points.
#----------------------------------------------------------------------------

## There are three post hoc approaches to summarize the transition probabilities
## for each attribute across time using the tdcm.summary() function.
## Each of them is illustrated below.

## 1. When the transition.option argument in the tdcm.summary() is not specified,
## the function assumes by default that transition.option = 1.
## Thus, when summarizing the transition probabilities
## you will compare the results for the first and last time point.

### Summary with default option

results3_def_transition <- TDCM::tdcm.summary(model3)
#> [1] Summarizing results...
#> [1] Routine finished. Check results.
results3_def_transition$transition.probabilities
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

## 2. When the transition.option = 2, you can compare the transition probabilities
## from the first time point to every other time point. In this case, you can
## compare the transition probabilities between Time Point 1 and Time Point 2,
## and Time Point 1 with Time Point 3.

### Summary with transition.option = 2
results3_2transition <- TDCM::tdcm.summary(model3, transition.option = 2)
#> [1] Summarizing results...
#> [1] Routine finished. Check results.
results3_2transition$transition.probabilities
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

## 3. When the transition.option = 3, you can compare the transition probabilities
## sequentially, such that for each attribute, you can compare the transition
## probabilities between Time Point 1 and Time Point 2, Time Point 2 and Time Point 3

### Summary with transition.option = 3
results3_3transition <- TDCM::tdcm.summary(model3, transition.option = 3)
#> [1] Summarizing results...
#> [1] Routine finished. Check results.
results3_3transition$transition.probabilities
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

#############################################################################
# Example 4: Full TDCM with different Q-matrices for each time point and
# anchor items
############################################################################

# Load dataset: T=3, A=2
data <- data.tdcm03$data
q1 <- data.tdcm03$q.matrix.1
q2 <- data.tdcm03$q.matrix.2
q3 <- data.tdcm03$q.matrix.3
q <- data.tdcm03$q.matrix.stacked
## Estimate model
## Anchor items:
## - item 1, item 11, and item 21 are the same
## - item 14 and item 24 are the same.

model4 <- TDCM::tdcm(data, q, num.time.points = 3, rule = "LCDM",
                    num.q.matrix = 3, anchor = c(1,11,
                                                 1,21,
                                                 14,24),
                    num.items = c(10, 10, 10))
#> [1] Preparing data for tdcm()...
#> [1] Estimating the TDCM in tdcm()...
#> [1] Depending on model complexity, estimation time may vary...
#> [1] TDCM estimation complete.
#> [1] Use tdcm.summary() to display results.

# Summarize results with tdcm.summary().
results4 <- TDCM::tdcm.summary(model4)
#> [1] Summarizing results...
#> [1] Routine finished. Check results.
results4$item.parameters
#>         λ0     λ1,1  λ1,2  λ2,12 
#> Item 1  -1.324 2.175   --    --  
#> Item 2  -1.556 2.337   --    --  
#> Item 3  -1.556 2.56    --    --  
#> Item 4  -1.085 2.393   --    --  
#> Item 5  -0.997 1.608   --    --  
#> Item 6  -1.423   --  1.835   --  
#> Item 7  -0.823   --  1.566   --  
#> Item 8  -0.847   --  2.421   --  
#> Item 9  -1.027   --  3.253   --  
#> Item 10 -0.949   --  3.086   --  
#> Item 11 -1.324 2.175   --    --  
#> Item 12 -1.132 1.874   --    --  
#> Item 13 -0.979 1.576   --    --  
#> Item 14 -1.34  0.59  0.442 1.407 
#> Item 15 -1.503 0.395 0.58  1.417 
#> Item 16 -1.292 0.889 0.595 0.751 
#> Item 17 -1.788 1.802 1.155 0.233 
#> Item 18 -1.123   --  2.784   --  
#> Item 19 -1.144   --  3.226   --  
#> Item 20 -1.271   --  3.253   --  
#> Item 21 -1.324 2.175   --    --  
#> Item 22 -1.248 0.745 1.556 0.005 
#> Item 23 -0.994 0.825 0.709 0.474 
#> Item 24 -1.34  0.59  0.442 1.407 
#> Item 25 -1.647 0.833 1.033 1.191 
#> Item 26 -1.747 1.402 0.951 0.383 
#> Item 27 -3.139 3.3   3.177 -1.758
#> Item 28 -1.284 0.555 1.419 0.888 
#> Item 29 -1.358 0.719 1.194 0.49  
#> Item 30 -1.379   --  3.249   --  
results4$growth
#>             T1[1] T2[1] T3[1]
#> Attribute 1 0.306 0.506 0.790
#> Attribute 2 0.311 0.581 0.705
results4$growth.effects
#>             T1[1] T3[1] Growth Odds Ratio Cohen`s h
#> Attribute 1 0.306 0.790  0.484       8.53      1.02
#> Attribute 2 0.311 0.705  0.394       5.29      0.81
results4$transition.probabilities
#> , , Attribute 1: Time 1 to Time 3
#> 
#>        T3 [0] T3 [1]
#> T1 [0]  0.232  0.768
#> T1 [1]  0.162  0.838
#> 
#> , , Attribute 2: Time 1 to Time 3
#> 
#>        T3 [0] T3 [1]
#> T1 [0]  0.316  0.684
#> T1 [1]  0.248  0.752
#> 

#----------------------------------------------------------------------------
#Compare models from example 3 and 4 to assess measurement invariance
#----------------------------------------------------------------------------

## Additionally, we can measure the measurement invariance between a TDCM model
## that assumes full measurement invariance (model3) and a model that assumes partial
## measurement invariance (model4)

model_comparison <- tdcm.compare(model3, model4)


#############################################################################
# Example 5: DINA TDCM with full measurement invariance
############################################################################

# Load dataset: T=2, A=4
data(data.tdcm01, package = "TDCM")
data <- data.tdcm01$data
q.matrix <- data.tdcm01$q.matrix

# Estimate model
model5 <- TDCM::tdcm(data, q.matrix, num.time.points = 2, invariance = TRUE,
                    rule = "DINA", num.q.matrix = 1)
#> [1] Preparing data for tdcm()...
#> [1] Estimating the TDCM in tdcm()...
#> [1] Depending on model complexity, estimation time may vary...
#> [1] TDCM estimation complete.
#> [1] Use tdcm.summary() to display results.
# Summarize results with tdcm.summary().
results5 <- TDCM::tdcm.summary(model5)
#> [1] Summarizing results...
#> [1] Routine finished. Check results.
results5$item.parameters
#>         λ0     λ1,1  λ1,2  λ1,3  λ1,4  λ2,12 λ2,13 λ2,14 λ2,23 λ2,24 λ2,34
#> Item 1  -2.039 2.47    --    --    --    --    --    --    --    --    -- 
#> Item 2  -2.202 2.411   --    --    --    --    --    --    --    --    -- 
#> Item 3  -2.03  2.331   --    --    --    --    --    --    --    --    -- 
#> Item 4  -1.463   --    --    --    --  2.901   --    --    --    --    -- 
#> Item 5  -1.562   --    --    --    --    --  3.474   --    --    --    -- 
#> Item 6  -1.912   --  2.167   --    --    --    --    --    --    --    -- 
#> Item 7  -1.886   --  2.206   --    --    --    --    --    --    --    -- 
#> Item 8  -2.018   --  2.41    --    --    --    --    --    --    --    -- 
#> Item 9  -1.605   --    --    --    --    --    --    --  3.816   --    -- 
#> Item 10 -1.536   --    --    --    --    --    --    --    --  3.415   -- 
#> Item 11 -1.867   --    --  2.285   --    --    --    --    --    --    -- 
#> Item 12 -2.146   --    --  2.616   --    --    --    --    --    --    -- 
#> Item 13 -2.174   --    --  2.617   --    --    --    --    --    --    -- 
#> Item 14 -1.32    --    --    --    --    --    --    --    --    --  3.446
#> Item 15 -1.457   --    --    --    --    --  3.361   --    --    --    -- 
#> Item 16 -2.252   --    --    --  2.67    --    --    --    --    --    -- 
#> Item 17 -2.35    --    --    --  2.844   --    --    --    --    --    -- 
#> Item 18 -2.153   --    --    --  2.398   --    --    --    --    --    -- 
#> Item 19 -1.582   --    --    --    --    --    --  3.436   --    --    -- 
#> Item 20 -1.508   --    --    --    --    --    --    --    --  3.358   -- 
results5$growth
#>             T1[1] T2[1]
#> Attribute 1 0.238 0.431
#> Attribute 2 0.349 0.526
#> Attribute 3 0.410 0.598
#> Attribute 4 0.290 0.721
results5$growth.effects
#>             T1[1] T2[1] Growth Odds Ratio Cohen`s h
#> Attribute 1 0.238 0.431  0.193       2.43      0.41
#> Attribute 2 0.349 0.526  0.177       2.07      0.36
#> Attribute 3 0.410 0.598  0.188       2.14      0.38
#> Attribute 4 0.290 0.721  0.431       6.33      0.89
results5$transition.probabilities
#> , , Attribute 1: Time 1 to Time 2
#> 
#>        T2 [0] T2 [1]
#> T1 [0]  0.628  0.372
#> T1 [1]  0.379  0.621
#> 
#> , , Attribute 2: Time 1 to Time 2
#> 
#>        T2 [0] T2 [1]
#> T1 [0]  0.552  0.448
#> T1 [1]  0.328  0.672
#> 
#> , , Attribute 3: Time 1 to Time 2
#> 
#>        T2 [0] T2 [1]
#> T1 [0]  0.526  0.474
#> T1 [1]  0.223  0.777
#> 
#> , , Attribute 4: Time 1 to Time 2
#> 
#>        T2 [0] T2 [1]
#> T1 [0]  0.352  0.648
#> T1 [1]  0.098  0.902
#> 

#############################################################################
# Example 6: DINO TDCM with full measurement invariance
############################################################################

# Load dataset: T=2, A=4
data(data.tdcm01, package = "TDCM")
data <- data.tdcm01$data
q.matrix <- data.tdcm01$q.matrix

# Estimate model
model6 <- TDCM::tdcm(data, q.matrix, num.time.points = 2, invariance = TRUE,
                    rule = "DINO", num.q.matrix = 1)
#> [1] Preparing data for tdcm()...
#> [1] Estimating the TDCM in tdcm()...
#> [1] Depending on model complexity, estimation time may vary...
#> [1] TDCM estimation complete.
#> [1] Use tdcm.summary() to display results.
# Summarize results with tdcm.summary().
results6 <- TDCM::tdcm.summary(model6)
#> [1] Summarizing results...
#> [1] Routine finished. Check results.
results6$item.parameters
#>         λ0     λ1,1  λ1,2  λ1,3  λ1,4  λ2,12 λ2,13 λ2,14 λ2,23 λ2,24 λ2,34
#> Item 1  -1.837 2.548   --    --    --    --    --    --    --    --    -- 
#> Item 2  -2.04  2.556   --    --    --    --    --    --    --    --    -- 
#> Item 3  -1.884 2.498   --    --    --    --    --    --    --    --    -- 
#> Item 4  -1.84    --    --    --    --  2.572   --    --    --    --    -- 
#> Item 5  -1.993   --    --    --    --    --  3.093   --    --    --    -- 
#> Item 6  -1.618   --  2.135   --    --    --    --    --    --    --    -- 
#> Item 7  -1.633   --  2.288   --    --    --    --    --    --    --    -- 
#> Item 8  -1.703   --  2.419   --    --    --    --    --    --    --    -- 
#> Item 9  -1.801   --    --    --    --    --    --    --  3.121   --    -- 
#> Item 10 -1.853   --    --    --    --    --    --    --    --  3.019   -- 
#> Item 11 -1.444   --    --  2.269   --    --    --    --    --    --    -- 
#> Item 12 -1.499   --    --  2.269   --    --    --    --    --    --    -- 
#> Item 13 -1.481   --    --  2.157   --    --    --    --    --    --    -- 
#> Item 14 -1.669   --    --    --    --    --    --    --    --    --  3.108
#> Item 15 -1.734   --    --    --    --    --  2.771   --    --    --    -- 
#> Item 16 -1.624   --    --    --  2.403   --    --    --    --    --    -- 
#> Item 17 -1.575   --    --    --  2.34    --    --    --    --    --    -- 
#> Item 18 -1.67    --    --    --  2.236   --    --    --    --    --    -- 
#> Item 19 -1.976   --    --    --    --    --    --  2.846   --    --    -- 
#> Item 20 -1.798   --    --    --    --    --    --    --    --  2.934   -- 
results6$growth
#>             T1[1] T2[1]
#> Attribute 1 0.182 0.351
#> Attribute 2 0.244 0.426
#> Attribute 3 0.269 0.454
#> Attribute 4 0.189 0.581
results6$growth.effects
#>             T1[1] T2[1] Growth Odds Ratio Cohen`s h
#> Attribute 1 0.182 0.351  0.169       2.43      0.39
#> Attribute 2 0.244 0.426  0.182       2.30      0.39
#> Attribute 3 0.269 0.454  0.185       2.26      0.39
#> Attribute 4 0.189 0.581  0.392       5.95      0.83
results6$transition.probabilities
#> , , Attribute 1: Time 1 to Time 2
#> 
#>        T2 [0] T2 [1]
#> T1 [0]  0.702  0.298
#> T1 [1]  0.414  0.586
#> 
#> , , Attribute 2: Time 1 to Time 2
#> 
#>        T2 [0] T2 [1]
#> T1 [0]  0.635  0.365
#> T1 [1]  0.386  0.614
#> 
#> , , Attribute 3: Time 1 to Time 2
#> 
#>        T2 [0] T2 [1]
#> T1 [0]  0.641  0.359
#> T1 [1]  0.289  0.711
#> 
#> , , Attribute 4: Time 1 to Time 2
#> 
#>        T2 [0] T2 [1]
#> T1 [0]  0.483  0.517
#> T1 [1]  0.146  0.854
#> 

#############################################################################
# Example 7: CRUM TDCM with full measurement invariance
############################################################################

# Load dataset: T=2, A=4
data(data.tdcm01, package = "TDCM")
data <- data.tdcm01$data
q.matrix <- data.tdcm01$q.matrix

# Estimate model
model7 <- TDCM::tdcm(data, q.matrix, num.time.points = 2, invariance = TRUE,
                    rule = "CRUM", num.q.matrix = 1)
#> [1] Preparing data for tdcm()...
#> [1] Estimating the TDCM in tdcm()...
#> [1] Depending on model complexity, estimation time may vary...
#> [1] TDCM estimation complete.
#> [1] Use tdcm.summary() to display results.
# Summarize results with tdcm.summary().
results7 <- TDCM::tdcm.summary(model7)
#> [1] Summarizing results...
#> [1] Routine finished. Check results.
results7$item.parameters
#>         λ0     λ1,1  λ1,2  λ1,3  λ1,4  λ2,12 λ2,13 λ2,14 λ2,23 λ2,24 λ2,34
#> Item 1  -1.887 2.6     --    --    --    --    --    --    --    --    -- 
#> Item 2  -2.06  2.543   --    --    --    --    --    --    --    --    -- 
#> Item 3  -1.93  2.552   --    --    --    --    --    --    --    --    -- 
#> Item 4  -2.005 1.772 1.804   --    --    --    --    --    --    --    -- 
#> Item 5  -2.199 2.337   --  1.882   --    --    --    --    --    --    -- 
#> Item 6  -1.801   --  2.181   --    --    --    --    --    --    --    -- 
#> Item 7  -1.791   --  2.264   --    --    --    --    --    --    --    -- 
#> Item 8  -1.93    --  2.503   --    --    --    --    --    --    --    -- 
#> Item 9  -2.288   --  2.257 2.194   --    --    --    --    --    --    -- 
#> Item 10 -2.09    --  2.41    --  1.698   --    --    --    --    --    -- 
#> Item 11 -1.8     --    --  2.329   --    --    --    --    --    --    -- 
#> Item 12 -1.997   --    --  2.559   --    --    --    --    --    --    -- 
#> Item 13 -2.022   --    --  2.55    --    --    --    --    --    --    -- 
#> Item 14 -2.135   --    --  1.906 2.352   --    --    --    --    --    -- 
#> Item 15 -1.95  2.215   --  1.648   --    --    --    --    --    --    -- 
#> Item 16 -2.173   --    --    --  2.801   --    --    --    --    --    -- 
#> Item 17 -2.045   --    --    --  2.658   --    --    --    --    --    -- 
#> Item 18 -2.079   --    --    --  2.514   --    --    --    --    --    -- 
#> Item 19 -2.097 2.662   --    --  1.492   --    --    --    --    --    -- 
#> Item 20 -1.993   --  2.36    --  1.629   --    --    --    --    --    -- 
results7$growth
#>             T1[1] T2[1]
#> Attribute 1 0.184 0.365
#> Attribute 2 0.305 0.479
#> Attribute 3 0.379 0.560
#> Attribute 4 0.238 0.689
results7$growth.effects
#>             T1[1] T2[1] Growth Odds Ratio Cohen`s h
#> Attribute 1 0.184 0.365  0.181       2.55      0.41
#> Attribute 2 0.305 0.479  0.174       2.09      0.36
#> Attribute 3 0.379 0.560  0.181       2.09      0.36
#> Attribute 4 0.238 0.689  0.451       7.09      0.94
results7$transition.probabilities
#> , , Attribute 1: Time 1 to Time 2
#> 
#>        T2 [0] T2 [1]
#> T1 [0]  0.684  0.316
#> T1 [1]  0.418  0.582
#> 
#> , , Attribute 2: Time 1 to Time 2
#> 
#>        T2 [0] T2 [1]
#> T1 [0]  0.590  0.410
#> T1 [1]  0.365  0.635
#> 
#> , , Attribute 3: Time 1 to Time 2
#> 
#>        T2 [0] T2 [1]
#> T1 [0]  0.569  0.431
#> T1 [1]  0.227  0.773
#> 
#> , , Attribute 4: Time 1 to Time 2
#> 
#>        T2 [0] T2 [1]
#> T1 [0]  0.375  0.625
#> T1 [1]  0.107  0.893
#> 

#############################################################################
# Example 8: RRUM TDCM with full measurement invariance
############################################################################

# Load dataset: T=2, A=4
data(data.tdcm01, package = "TDCM")
data <- data.tdcm01$data
q.matrix <- data.tdcm01$q.matrix

# Estimate model
model8 <- TDCM::tdcm(data, q.matrix, num.time.points = 2, invariance = TRUE,
                    rule = "RRUM", num.q.matrix = 1)
#> [1] Preparing data for tdcm()...
#> [1] Estimating the TDCM in tdcm()...
#> [1] Depending on model complexity, estimation time may vary...
#> [1] TDCM estimation complete.
#> [1] Use tdcm.summary() to display results.
# Summarize results with tdcm.summary().
results8 <- TDCM::tdcm.summary(model8)
#> [1] Summarizing results...
#> [1] Routine finished. Check results.
results8$item.parameters
#>         λ0     λ1,1  λ1,2  λ1,3  λ1,4  λ2,12 λ2,13 λ2,14 λ2,23 λ2,24 λ2,34
#> Item 1  -2.064 1.65    --    --    --    --    --    --    --    --    -- 
#> Item 2  -2.208 1.707   --    --    --    --    --    --    --    --    -- 
#> Item 3  -2.088 1.635   --    --    --    --    --    --    --    --    -- 
#> Item 4  -2.059 0.769 1.125   --    --    --    --    --    --    --    -- 
#> Item 5  -2.238 0.823   --  1.301   --    --    --    --    --    --    -- 
#> Item 6  -2.015   --  1.472   --    --    --    --    --    --    --    -- 
#> Item 7  -2       --  1.489   --    --    --    --    --    --    --    -- 
#> Item 8  -2.136   --  1.662   --    --    --    --    --    --    --    -- 
#> Item 9  -2.172   --  0.929 1.155   --    --    --    --    --    --    -- 
#> Item 10 -2.08    --  1.11    --  0.857   --    --    --    --    --    -- 
#> Item 11 -1.992   --    --  1.507   --    --    --    --    --    --    -- 
#> Item 12 -2.18    --    --  1.708   --    --    --    --    --    --    -- 
#> Item 13 -2.209   --    --  1.725   --    --    --    --    --    --    -- 
#> Item 14 -2.034   --    --  0.85  1.079   --    --    --    --    --    -- 
#> Item 15 -2.012 0.857   --  1.042   --    --    --    --    --    --    -- 
#> Item 16 -2.38    --    --    --  1.917   --    --    --    --    --    -- 
#> Item 17 -2.328   --    --    --  1.876   --    --    --    --    --    -- 
#> Item 18 -2.252   --    --    --  1.713   --    --    --    --    --    -- 
#> Item 19 -2.115 1.144   --    --  0.86    --    --    --    --    --    -- 
#> Item 20 -1.969   --  1.127   --  0.717   --    --    --    --    --    -- 
results8$growth
#>             T1[1] T2[1]
#> Attribute 1 0.195 0.377
#> Attribute 2 0.330 0.499
#> Attribute 3 0.395 0.581
#> Attribute 4 0.262 0.708
results8$growth.effects
#>             T1[1] T2[1] Growth Odds Ratio Cohen`s h
#> Attribute 1 0.195 0.377  0.182       2.50      0.41
#> Attribute 2 0.330 0.499  0.169       2.02      0.34
#> Attribute 3 0.395 0.581  0.186       2.12      0.37
#> Attribute 4 0.262 0.708  0.446       6.83      0.93
results8$transition.probabilities
#> , , Attribute 1: Time 1 to Time 2
#> 
#>        T2 [0] T2 [1]
#> T1 [0]  0.673  0.327
#> T1 [1]  0.419  0.581
#> 
#> , , Attribute 2: Time 1 to Time 2
#> 
#>        T2 [0] T2 [1]
#> T1 [0]  0.574  0.426
#> T1 [1]  0.351  0.649
#> 
#> , , Attribute 3: Time 1 to Time 2
#> 
#>        T2 [0] T2 [1]
#> T1 [0]  0.545  0.455
#> T1 [1]  0.224  0.776
#> 
#> , , Attribute 4: Time 1 to Time 2
#> 
#>        T2 [0] T2 [1]
#> T1 [0]  0.358  0.642
#> T1 [1]  0.105  0.895
#> 

#############################################################################
# Example 9: TDCM with and without forgetting
############################################################################

# Load dataset: T=2, A=4,
data(data.tdcm01, package = "TDCM")
data <- data.tdcm01$data
q.matrix <- data.tdcm01$q.matrix

##----------------------------------------------------------------------------
# With forgetting
#----------------------------------------------------------------------------
## Consider a default model in which students can retain or lose their mastery status
## from one time point to another

# Estimate the model
model11_forgetting <- TDCM::tdcm(data, q.matrix, num.time.points = 2, invariance = TRUE,
                                rule = "LCDM", num.q.matrix = 1)
#> [1] Preparing data for tdcm()...
#> [1] Estimating the TDCM in tdcm()...
#> [1] Depending on model complexity, estimation time may vary...
#> [1] TDCM estimation complete.
#> [1] Use tdcm.summary() to display results.

# Summarize results with tdcm.summary().
results_forgetting <- TDCM::tdcm.summary(model11_forgetting, transition.option = 3)
#> [1] Summarizing results...
#> [1] Routine finished. Check results.
results_forgetting$transition.probabilities
#> , , Attribute 1: Time 1 to Time 2
#> 
#>       [0]   [1]
#> [0] 0.680 0.320
#> [1] 0.417 0.583
#> 
#> , , Attribute 2: Time 1 to Time 2
#> 
#>       [0]   [1]
#> [0] 0.581 0.419
#> [1] 0.353 0.647
#> 
#> , , Attribute 3: Time 1 to Time 2
#> 
#>       [0]   [1]
#> [0] 0.549 0.451
#> [1] 0.221 0.779
#> 
#> , , Attribute 4: Time 1 to Time 2
#> 
#>       [0]   [1]
#> [0] 0.371 0.629
#> [1] 0.104 0.896
#> 

#, , Attribute 1: Time 1 to Time 2
#
#     [0]   [1]
#[0] 0.680 0.320
#[1] 0.417 0.583
#
#, , Attribute 2: Time 1 to Time 2
#
#     [0]   [1]
#[0] 0.581 0.419
#[1] 0.353 0.647
#
#, , Attribute 3: Time 1 to Time 2
#
#     [0]   [1]
#[0] 0.549 0.451
#[1] 0.221 0.779
#
#, , Attribute 4: Time 1 to Time 2
#
#     [0]   [1]
#[0] 0.371 0.629
#[1] 0.104 0.896

##----------------------------------------------------------------------------
# Without forgetting
#----------------------------------------------------------------------------
## Consider a model in which students cannot lose their mastery status for Attribute 4
## from one time point to another.

# Estimate the model
model11_noforgetting <- TDCM::tdcm(data, q.matrix, num.time.points = 2, invariance = TRUE,
                                  rule = "LCDM", num.q.matrix = 1, forget.att = c(4))
#> [1] Preparing data for tdcm()...
#> [1] Estimating the TDCM in tdcm()...
#> [1] Depending on model complexity, estimation time may vary...
#> [1] TDCM estimation complete.
#> [1] Use tdcm.summary() to display results.

# Summarize results with tdcm.summary().
results_noforgetting <- TDCM::tdcm.summary(model11_noforgetting, transition.option = 3)
#> [1] Summarizing results...
#> [1] Routine finished. Check results.
results_noforgetting$transition.probabilities
#> , , Attribute 1: Time 1 to Time 2
#> 
#>       [0]   [1]
#> [0] 0.678 0.322
#> [1] 0.416 0.584
#> 
#> , , Attribute 2: Time 1 to Time 2
#> 
#>       [0]   [1]
#> [0] 0.578 0.422
#> [1] 0.359 0.641
#> 
#> , , Attribute 3: Time 1 to Time 2
#> 
#>       [0]   [1]
#> [0] 0.546 0.454
#> [1] 0.226 0.774
#> 
#> , , Attribute 4: Time 1 to Time 2
#> 
#>       [0]   [1]
#> [0] 0.382 0.618
#> [1] 0.000 1.000
#> 

#, , Attribute 1: Time 1 to Time 2
#
#     [0]   [1]
#[0] 0.678 0.322
#[1] 0.416 0.584
#
#, , Attribute 2: Time 1 to Time 2
#
#     [0]   [1]
#[0] 0.578 0.422
#[1] 0.359 0.641
#
#, , Attribute 3: Time 1 to Time 2
#
#     [0]   [1]
#[0] 0.546 0.454
#[1] 0.226 0.774
#
#, , Attribute 4: Time 1 to Time 2
#
#      [0]   [1]
#[0] 0.382 0.618
#[1] 0.000 1.000

# }
```
