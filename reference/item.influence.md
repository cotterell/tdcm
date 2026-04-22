# Estimating item influence measures.

Function to estimate estimate item influence measures. Code adapted from
(Jurich & Madison, 2023). This function is not available for
longitudinal DCMs.

## Usage

``` r
item.influence(model, data, fullcorrelation = FALSE, progress = TRUE)
```

## Arguments

- model:

  a previously calibrated model; an object of class `gdina`.

- data:

  a required \\N \times I\\ matrix. Binary item responses are in the
  columns.

- fullcorrelation:

  optional logical argument indicating a full or reduced
  response-classification correlation matrix.

- progress:

  An optional logical indicating whether the function should print the
  progress of estimation.

## Value

A list containing several item influence measures.

## Details

For DCMs, item influence quantifies how much an item impacts
classifications. Given an estimated DCM and item response data, this
function estimates five item influence measures, including item pull,
item override, proportion of attribute information,
response-classification correlation (corr1), and response-posterior
correlation (corr2).

## Note

Currently, this function currently only runs on DCMs estimated at a
single time point. It will not run properly for TDCM objects.

## References

Jurich, D. & Madison, M. J. (2023). Measuring item influence for
diagnostic classification models. *Educational Assessment*.

## Examples

``` r
# \donttest{
## Item influence illustration
#load data (simulated based on Jurich and Bradshaw (2014))
qmatrix <- CDM::data.sda6$q.matrix
responses <- CDM::data.sda6$data

#Estimate the full LCDM
model1 <- CDM::gdina(responses, qmatrix, linkfct = "logit", method = "ML")
#> -----------------------------------------------------------------
#> CDM 8.3-14 (2025-07-13 14:03:01) 
#> GDINA Model 
#>  Link function: logit 
#>   ** 2026-04-22 21:08:35.157719 
#> -----------------------------------------------------------------
#> ...........................................................
#> Iteration 1     2026-04-22 21:08:35.171086 
#> Deviance = 41250.26
#> Maximum parameter change: 0.255116 
#> ...........................................................
#> Iteration 2     2026-04-22 21:08:35.203542 
#> Deviance = 37307.05 | Deviance change = 3943.205
#> Maximum parameter change: 0.237805 
#> ...........................................................
#> Iteration 3     2026-04-22 21:08:35.222997 
#> Deviance = 36844.82 | Deviance change = 462.2334
#> Maximum parameter change: 0.058056 
#> ...........................................................
#> Iteration 4     2026-04-22 21:08:35.249651 
#> Deviance = 36784.82 | Deviance change = 60.00073
#> Maximum parameter change: 0.037476 
#> ...........................................................
#> Iteration 5     2026-04-22 21:08:35.266255 
#> Deviance = 36765.74 | Deviance change = 19.07727
#> Maximum parameter change: 0.134754 
#> ...........................................................
#> Iteration 6     2026-04-22 21:08:35.300484 
#> Deviance = 36639.5 | Deviance change = 126.237
#> Maximum parameter change: 0.052503 
#> ...........................................................
#> Iteration 7     2026-04-22 21:08:35.316938 
#> Deviance = 36619.88 | Deviance change = 19.628
#> Maximum parameter change: 0.058732 
#> ...........................................................
#> Iteration 8     2026-04-22 21:08:35.340797 
#> Deviance = 36562.93 | Deviance change = 56.9458
#> Maximum parameter change: 0.028076 
#> ...........................................................
#> Iteration 9     2026-04-22 21:08:35.357289 
#> Deviance = 36557.01 | Deviance change = 5.918637
#> Maximum parameter change: 0.069777 
#> ...........................................................
#> Iteration 10     2026-04-22 21:08:35.381433 
#> Deviance = 36533.7 | Deviance change = 23.31049
#> Maximum parameter change: 0.006856 
#> ...........................................................
#> Iteration 11     2026-04-22 21:08:35.394277 
#> Deviance = 36535.91 | Deviance change = -2.21303
#> **** Deviances decreases! Check for nonconvergence.   ****
#> Maximum parameter change: 0.034248 
#> ...........................................................
#> Iteration 12     2026-04-22 21:08:35.417978 
#> Deviance = 36529.54 | Deviance change = 6.379348
#> Maximum parameter change: 0.005495 
#> ...........................................................
#> Iteration 13     2026-04-22 21:08:35.43096 
#> Deviance = 36528.75 | Deviance change = 0.7851915
#> Maximum parameter change: 0.003748 
#> ...........................................................
#> Iteration 14     2026-04-22 21:08:35.454977 
#> Deviance = 36528.29 | Deviance change = 0.4589058
#> Maximum parameter change: 0.00302 
#> ...........................................................
#> Iteration 15     2026-04-22 21:08:35.467609 
#> Deviance = 36527.71 | Deviance change = 0.5760876
#> Maximum parameter change: 0.009911 
#> ...........................................................
#> Iteration 16     2026-04-22 21:08:35.49274 
#> Deviance = 36527.23 | Deviance change = 0.4854475
#> Maximum parameter change: 0.00118 
#> ...........................................................
#> Iteration 17     2026-04-22 21:08:35.505283 
#> Deviance = 36526.96 | Deviance change = 0.2715357
#> Maximum parameter change: 0.010884 
#> ...........................................................
#> Iteration 18     2026-04-22 21:08:35.530335 
#> Deviance = 36526.76 | Deviance change = 0.2015728
#> Maximum parameter change: 0.001425 
#> ...........................................................
#> Iteration 19     2026-04-22 21:08:35.542397 
#> Deviance = 36526.66 | Deviance change = 0.1012999
#> Maximum parameter change: 0.00123 
#> ...........................................................
#> Iteration 20     2026-04-22 21:08:35.555848 
#> Deviance = 36526.58 | Deviance change = 0.0787773
#> Maximum parameter change: 0.001022 
#> ...........................................................
#> Iteration 21     2026-04-22 21:08:35.567949 
#> Deviance = 36526.51 | Deviance change = 0.0670459
#> Maximum parameter change: 0.000859 
#> ...........................................................
#> Iteration 22     2026-04-22 21:08:35.580085 
#> Deviance = 36526.45 | Deviance change = 0.0594842
#> Maximum parameter change: 0.00073 
#> ...........................................................
#> Iteration 23     2026-04-22 21:08:35.592427 
#> Deviance = 36526.4 | Deviance change = 0.0540514
#> Maximum parameter change: 0.000624 
#> ...........................................................
#> Iteration 24     2026-04-22 21:08:35.604824 
#> Deviance = 36526.35 | Deviance change = 0.0498587
#> Maximum parameter change: 0.000537 
#> ...........................................................
#> Iteration 25     2026-04-22 21:08:35.617465 
#> Deviance = 36526.3 | Deviance change = 0.0464631
#> Maximum parameter change: 0.000465 
#> ...........................................................
#> Iteration 26     2026-04-22 21:08:35.630021 
#> Deviance = 36526.26 | Deviance change = 0.0436157
#> Maximum parameter change: 0.000405 
#> ...........................................................
#> Iteration 27     2026-04-22 21:08:35.648445 
#> Deviance = 36526.21 | Deviance change = 0.0411638
#> Maximum parameter change: 0.000355 
#> ...........................................................
#> Iteration 28     2026-04-22 21:08:35.660736 
#> Deviance = 36526.18 | Deviance change = 0.0390077
#> Maximum parameter change: 0.000312 
#> ...........................................................
#> Iteration 29     2026-04-22 21:08:35.672945 
#> Deviance = 36526.14 | Deviance change = 0.0370793
#> Maximum parameter change: 0.000277 
#> ...........................................................
#> Iteration 30     2026-04-22 21:08:35.685112 
#> Deviance = 36526.1 | Deviance change = 0.0353313
#> Maximum parameter change: 0.000246 
#> ...........................................................
#> Iteration 31     2026-04-22 21:08:35.697218 
#> Deviance = 36526.07 | Deviance change = 0.0337297
#> Maximum parameter change: 0.00022 
#> ...........................................................
#> Iteration 32     2026-04-22 21:08:35.709161 
#> Deviance = 36526.04 | Deviance change = 0.0322497
#> Maximum parameter change: 0.000212 
#> ...........................................................
#> Iteration 33     2026-04-22 21:08:35.721144 
#> Deviance = 36526.01 | Deviance change = 0.0308728
#> Maximum parameter change: 0.000207 
#> ...........................................................
#> Iteration 34     2026-04-22 21:08:35.733114 
#> Deviance = 36525.98 | Deviance change = 0.0295852
#> Maximum parameter change: 0.000202 
#> ...........................................................
#> Iteration 35     2026-04-22 21:08:35.745232 
#> Deviance = 36525.95 | Deviance change = 0.028376
#> Maximum parameter change: 0.000197 
#> ...........................................................
#> Iteration 36     2026-04-22 21:08:35.757363 
#> Deviance = 36525.92 | Deviance change = 0.0272367
#> Maximum parameter change: 0.000192 
#> ...........................................................
#> Iteration 37     2026-04-22 21:08:35.769721 
#> Deviance = 36525.9 | Deviance change = 0.0261606
#> Maximum parameter change: 0.000187 
#> ...........................................................
#> Iteration 38     2026-04-22 21:08:35.782129 
#> Deviance = 36525.87 | Deviance change = 0.025142
#> Maximum parameter change: 0.000183 
#> ...........................................................
#> Iteration 39     2026-04-22 21:08:35.794326 
#> Deviance = 36525.85 | Deviance change = 0.0241762
#> Maximum parameter change: 0.000178 
#> ...........................................................
#> Iteration 40     2026-04-22 21:08:35.806413 
#> Deviance = 36525.82 | Deviance change = 0.0232592
#> Maximum parameter change: 0.000174 
#> ...........................................................
#> Iteration 41     2026-04-22 21:08:35.824194 
#> Deviance = 36525.8 | Deviance change = 0.0223876
#> Maximum parameter change: 0.00017 
#> ...........................................................
#> Iteration 42     2026-04-22 21:08:35.835757 
#> Deviance = 36525.78 | Deviance change = 0.0215582
#> Maximum parameter change: 0.000166 
#> ...........................................................
#> Iteration 43     2026-04-22 21:08:35.847331 
#> Deviance = 36525.76 | Deviance change = 0.0207684
#> Maximum parameter change: 0.000162 
#> ...........................................................
#> Iteration 44     2026-04-22 21:08:35.858871 
#> Deviance = 36525.74 | Deviance change = 0.0200156
#> Maximum parameter change: 0.000158 
#> ...........................................................
#> Iteration 45     2026-04-22 21:08:35.870445 
#> Deviance = 36525.72 | Deviance change = 0.0192977
#> Maximum parameter change: 0.000154 
#> ...........................................................
#> Iteration 46     2026-04-22 21:08:35.882018 
#> Deviance = 36525.7 | Deviance change = 0.0186127
#> Maximum parameter change: 0.00015 
#> ...........................................................
#> Iteration 47     2026-04-22 21:08:35.893546 
#> Deviance = 36525.68 | Deviance change = 0.0179586
#> Maximum parameter change: 0.000147 
#> ...........................................................
#> Iteration 48     2026-04-22 21:08:35.905145 
#> Deviance = 36525.66 | Deviance change = 0.0173337
#> Maximum parameter change: 0.000143 
#> ...........................................................
#> Iteration 49     2026-04-22 21:08:35.916923 
#> Deviance = 36525.65 | Deviance change = 0.0167363
#> Maximum parameter change: 0.00014 
#> ...........................................................
#> Iteration 50     2026-04-22 21:08:35.928669 
#> Deviance = 36525.63 | Deviance change = 0.0161651
#> Maximum parameter change: 0.000137 
#> ...........................................................
#> Iteration 51     2026-04-22 21:08:35.940182 
#> Deviance = 36525.62 | Deviance change = 0.0156186
#> Maximum parameter change: 0.000134 
#> ...........................................................
#> Iteration 52     2026-04-22 21:08:35.951695 
#> Deviance = 36525.6 | Deviance change = 0.0150956
#> Maximum parameter change: 0.000131 
#> ...........................................................
#> Iteration 53     2026-04-22 21:08:35.963078 
#> Deviance = 36525.59 | Deviance change = 0.0145947
#> Maximum parameter change: 0.000128 
#> ...........................................................
#> Iteration 54     2026-04-22 21:08:35.974313 
#> Deviance = 36525.57 | Deviance change = 0.0141148
#> Maximum parameter change: 0.000125 
#> ...........................................................
#> Iteration 55     2026-04-22 21:08:35.99139 
#> Deviance = 36525.56 | Deviance change = 0.0136549
#> Maximum parameter change: 0.000122 
#> ...........................................................
#> Iteration 56     2026-04-22 21:08:36.002296 
#> Deviance = 36525.55 | Deviance change = 0.013214
#> Maximum parameter change: 0.000119 
#> ...........................................................
#> Iteration 57     2026-04-22 21:08:36.013064 
#> Deviance = 36525.53 | Deviance change = 0.0127911
#> Maximum parameter change: 0.000117 
#> ...........................................................
#> Iteration 58     2026-04-22 21:08:36.023759 
#> Deviance = 36525.52 | Deviance change = 0.0123853
#> Maximum parameter change: 0.000114 
#> ...........................................................
#> Iteration 59     2026-04-22 21:08:36.034459 
#> Deviance = 36525.51 | Deviance change = 0.0119958
#> Maximum parameter change: 0.000112 
#> ...........................................................
#> Iteration 60     2026-04-22 21:08:36.045156 
#> Deviance = 36525.5 | Deviance change = 0.0116218
#> Maximum parameter change: 0.000109 
#> ...........................................................
#> Iteration 61     2026-04-22 21:08:36.055819 
#> Deviance = 36525.49 | Deviance change = 0.0112625
#> Maximum parameter change: 0.000107 
#> ...........................................................
#> Iteration 62     2026-04-22 21:08:36.066491 
#> Deviance = 36525.47 | Deviance change = 0.0109173
#> Maximum parameter change: 0.000105 
#> ...........................................................
#> Iteration 63     2026-04-22 21:08:36.077239 
#> Deviance = 36525.46 | Deviance change = 0.0105854
#> Maximum parameter change: 0.000102 
#> ...........................................................
#> Iteration 64     2026-04-22 21:08:36.088063 
#> Deviance = 36525.45 | Deviance change = 0.0102663
#> Maximum parameter change: 1e-04 
#> ...........................................................
#> Iteration 65     2026-04-22 21:08:36.098956 
#> Deviance = 36525.44 | Deviance change = 0.0099593
#> Maximum parameter change: 9.8e-05 
#> -----------------------------------------------------------------
#> Time difference of 1.00143 secs

#Estimate item influence measures
influence <- TDCM::item.influence(model1, responses)
#> [1] Calclating item influence measures. Progress = 0%.
#> [1] Calclating item influence measures. Progress = 24%.
#> [1] Calclating item influence measures. Progress = 47%.
#> [1] Calclating item influence measures. Progress = 71%.
#> [1] Calclating item influence measures. Progress = 94%.
#> [1] Routine finished. Check results.

#Summarize influence statistics
influence$Pull #item pull
#>    Item Attribute pull0 pull1
#> 1     1         1 0.649 0.652
#> 2     2         1 0.678 0.736
#> 3     3         1 0.597 0.694
#> 4     4         1 0.863 0.683
#> 5     5         2 0.498 0.702
#> 6     6         2 0.468 0.813
#> 7     7         2 0.515 0.763
#> 8     8         2 0.594 0.757
#> 9     9         3 0.601 0.633
#> 10   10         3 0.578 0.715
#> 11   11         3 0.501 0.708
#> 12   12         3 0.551 0.677
#> 13   13         3 1.000 0.823
#> 14   14         3 0.864 0.754
#> 15   15         4 0.581 0.771
#> 16   16         4 0.633 0.860
#> 17   17         4 0.754 0.753
influence$Override #item override
#>    Item Attribute override
#> 1     1         1    0.070
#> 2     2         1    0.098
#> 3     3         1    0.044
#> 4     4         1    0.113
#> 5     5         2    0.031
#> 6     6         2    0.069
#> 7     7         2    0.070
#> 8     8         2    0.090
#> 9     9         3    0.022
#> 10   10         3    0.035
#> 11   11         3    0.024
#> 12   12         3    0.027
#> 13   13         3    0.156
#> 14   14         3    0.087
#> 15   15         4    0.098
#> 16   16         4    0.220
#> 17   17         4    0.094
influence$Information #proportion of attribute information
#>    Item Attribute propinfo
#> 1     1         1    0.155
#> 2     2         1    0.236
#> 3     3         1    0.132
#> 4     4         1    0.476
#> 5     5         2    0.116
#> 6     6         2    0.248
#> 7     7         2    0.269
#> 8     8         2    0.367
#> 9     9         3    0.027
#> 10   10         3    0.076
#> 11   11         3    0.039
#> 12   12         3    0.044
#> 13   13         3    0.593
#> 14   14         3    0.221
#> 15   15         4    0.220
#> 16   16         4    0.399
#> 17   17         4    0.381
influence$Correlation1 #correlation of responses and classifications
#>    Item Attribute Correl1
#> 1     1         1   0.300
#> 2     2         1   0.407
#> 3     3         1   0.266
#> 4     4         1   0.521
#> 5     5         2   0.186
#> 6     6         2   0.289
#> 7     7         2   0.287
#> 8     8         2   0.341
#> 9     9         3   0.193
#> 10   10         3   0.295
#> 11   11         3   0.209
#> 12   12         3   0.227
#> 13   13         3   0.758
#> 14   14         3   0.560
#> 15   15         4   0.359
#> 16   16         4   0.504
#> 17   17         4   0.471
influence$Correlation2 #correlation of responses and posterior probabilities
#>    Item Attribute Correl2
#> 1     1         1   0.360
#> 2     2         1   0.441
#> 3     3         1   0.331
#> 4     4         1   0.603
#> 5     5         2   0.219
#> 6     6         2   0.309
#> 7     7         2   0.329
#> 8     8         2   0.388
#> 9     9         3   0.213
#> 10   10         3   0.357
#> 11   11         3   0.254
#> 12   12         3   0.272
#> 13   13         3   0.820
#> 14   14         3   0.579
#> 15   15         4   0.441
#> 16   16         4   0.581
#> 17   17         4   0.578

# }
```
