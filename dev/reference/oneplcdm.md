# One-parameter log-linear cognitive diagnosis model.

Function to estimate the 1-PLCDM (Madison et al., 2023; Maas et al.,
2023).

## Usage

``` r
oneplcdm(data, q.matrix, progress = TRUE)
```

## Arguments

- data:

  a required \\N \times I\\ matrix. Binary item responses are in the
  columns.

- q.matrix:

  a required \\I \times A\\ matrix indicating which items measure which
  attributes.

- progress:

  An optional logical indicating whether the function should print the
  progress of estimation.

## Value

An object of class `gdina` with entries as indicated in the CDM package.

## Details

Estimates the single-attribute and multi-attribute 1-PLCDM described in
Madison et al. (2024). Example shows that attribute subscores are
sufficient statistics for classifications.

## Note

Currently, this model cannot be embedded within the TDCM via the `rule`
argument.

## References

George, A. C., Robitzsch, A., Kiefer, T., Gross, J., & Ünlü , A. (2016).
The R package CDM for cognitive diagnosis models. *Journal of
Statistical Software, 74*(2), 1-24.

Henson, R., Templin, J., & Willse, J. (2009). Defining a family of
cognitive diagnosis models using log linear models with latent
variables. *Psychometrika, 74*, 191-21.

Madison, M.J., Wind, S., Maas, L., Yamaguchi, K. & Haab, S. (2024). A
one-parameter diagnostic classification model with familiar measurement
properties. *Journal of Educational Measurement*.

Maas, L., Madison, M. J., & Brinkhuis, M. J. (2024). Properties and
performance of the one-parameter log-linear cognitive diagnosis model.
*Frontiers*.

## Examples

``` r
# \donttest{
## Example 1: A = 4
data(data.tdcm05)
dat5 <- data.tdcm05$data
qmat5 <- data.tdcm05$q.matrix

# calibrate LCDM
m1 <- CDM::gdina(dat5, qmat5, linkfct = "logit", method = "ML")
#> -----------------------------------------------------------------
#> CDM 8.3-14 (2025-07-13 14:03:01) 
#> GDINA Model 
#>  Link function: logit 
#>   ** 2026-04-24 04:04:27.211985 
#> -----------------------------------------------------------------
#> ...........................................................
#> Iteration 1     2026-04-24 04:04:27.226177 
#> Deviance = 19770.5
#> Maximum parameter change: 0.084303 
#> ...........................................................
#> Iteration 2     2026-04-24 04:04:27.262326 
#> Deviance = 17905.28 | Deviance change = 1865.213
#> Maximum parameter change: 0.05588 
#> ...........................................................
#> Iteration 3     2026-04-24 04:04:27.287726 
#> Deviance = 17666.32 | Deviance change = 238.9634
#> Maximum parameter change: 0.074029 
#> ...........................................................
#> Iteration 4     2026-04-24 04:04:27.32259 
#> Deviance = 17522.41 | Deviance change = 143.912
#> Maximum parameter change: 0.02881 
#> ...........................................................
#> Iteration 5     2026-04-24 04:04:27.341381 
#> Deviance = 17509.64 | Deviance change = 12.76482
#> Maximum parameter change: 0.021972 
#> ...........................................................
#> Iteration 6     2026-04-24 04:04:27.362228 
#> Deviance = 17503.68 | Deviance change = 5.9658
#> Maximum parameter change: 0.004612 
#> ...........................................................
#> Iteration 7     2026-04-24 04:04:27.384638 
#> Deviance = 17500.71 | Deviance change = 2.966632
#> Maximum parameter change: 0.014631 
#> ...........................................................
#> Iteration 8     2026-04-24 04:04:27.410295 
#> Deviance = 17491.82 | Deviance change = 8.893127
#> Maximum parameter change: 0.003278 
#> ...........................................................
#> Iteration 9     2026-04-24 04:04:27.429072 
#> Deviance = 17494.42 | Deviance change = -2.603457
#> **** Deviances decreases! Check for nonconvergence.   ****
#> Maximum parameter change: 0.021912 
#> ...........................................................
#> Iteration 10     2026-04-24 04:04:27.453399 
#> Deviance = 17484.83 | Deviance change = 9.589414
#> Maximum parameter change: 0.003986 
#> ...........................................................
#> Iteration 11     2026-04-24 04:04:27.471235 
#> Deviance = 17484.8 | Deviance change = 0.0370989
#> Maximum parameter change: 0.010959 
#> ...........................................................
#> Iteration 12     2026-04-24 04:04:27.497311 
#> Deviance = 17482.69 | Deviance change = 2.110455
#> Maximum parameter change: 0.00172 
#> ...........................................................
#> Iteration 13     2026-04-24 04:04:27.512235 
#> Deviance = 17482.63 | Deviance change = 0.0551816
#> Maximum parameter change: 0.003247 
#> ...........................................................
#> Iteration 14     2026-04-24 04:04:27.531278 
#> Deviance = 17482.5 | Deviance change = 0.1285058
#> Maximum parameter change: 0.000442 
#> ...........................................................
#> Iteration 15     2026-04-24 04:04:27.545975 
#> Deviance = 17482.5 | Deviance change = 0.0050662
#> Maximum parameter change: 0.000736 
#> ...........................................................
#> Iteration 16     2026-04-24 04:04:27.564291 
#> Deviance = 17482.5 | Deviance change = -0.0019762
#> **** Deviances decreases! Check for nonconvergence.   ****
#> Maximum parameter change: 0.00023 
#> ...........................................................
#> Iteration 17     2026-04-24 04:04:27.578704 
#> Deviance = 17482.5 | Deviance change = -0.0017578
#> **** Deviances decreases! Check for nonconvergence.   ****
#> Maximum parameter change: 0.000182 
#> ...........................................................
#> Iteration 18     2026-04-24 04:04:27.594633 
#> Deviance = 17482.5 | Deviance change = -0.0020594
#> **** Deviances decreases! Check for nonconvergence.   ****
#> Maximum parameter change: 0.000145 
#> ...........................................................
#> Iteration 19     2026-04-24 04:04:27.608029 
#> Deviance = 17482.5 | Deviance change = -0.0020351
#> **** Deviances decreases! Check for nonconvergence.   ****
#> Maximum parameter change: 0.000118 
#> ...........................................................
#> Iteration 20     2026-04-24 04:04:27.621262 
#> Deviance = 17482.51 | Deviance change = -0.001883
#> **** Deviances decreases! Check for nonconvergence.   ****
#> Maximum parameter change: 9.9e-05 
#> -----------------------------------------------------------------
#> Time difference of 0.4735239 secs

# calibrate 1-PLCDM
m2 <- TDCM::oneplcdm(dat5, qmat5)
#> [1] Estimating 1-PLCDM...
#> [1] Estimation is complete. Use the CDM summary function to display results.
summary(m2)
#> -----------------------------------------------------------------
#> CDM 8.3-14 (2025-07-13 14:03:01) 
#> 
#> Call:
#> CDM::gdina(data = data, q.matrix = q.matrix, linkfct = "logit", 
#>     method = "ML", delta.designmatrix = delta.designmatrix, HOGDINA = 0, 
#>     progress = FALSE)
#> 
#> Date of Analysis: 2026-04-24 04:04:29.462082 
#> Time difference of 1.579844 secs
#> Computation Time: 1.579844 
#> 
#> Higher Order Generalized DINA Model 
#> 
#> Number of iterations = 47
#> Iteration with minimal deviance = 4 
#> 
#> Estimation method: ML
#> Optimizer: CDM
#> Monotonicity constraints: FALSE
#> Number of items at boundary monotonicity constraint: NA
#> 
#> Parameter regularization: FALSE
#> 
#> Deviance = 18247.97  | Log likelihood = -9123.99 
#> 
#> Number of persons = 750 
#> Number of groups = 1 
#> Number of items = 20 
#> Number of estimated parameters = 28 
#> Number of estimated item parameters = 24 
#> Number of estimated skill class parameters = 4 ( 16 latent skill classes)
#> 
#> AIC = 18304  | penalty = 56    | AIC = -2*LL + 2*p  
#> BIC = 18433  | penalty = 185.36    | BIC = -2*LL + log(n)*p   
#> CAIC = 18461  | penalty = 213.36    | CAIC = -2*LL + [log(n)+1]*p  (consistent AIC)   
#> 
#> -----------------------------------------------------------------
#> Used Q-matrix 
#> 
#>        Att1 Att2 Att3 Att4
#> Item1     1    0    0    0
#> Item2     1    0    0    0
#> Item3     1    0    0    0
#> Item4     1    0    0    0
#> Item5     1    0    0    0
#> Item6     0    1    0    0
#> Item7     0    1    0    0
#> Item8     0    1    0    0
#> Item9     0    1    0    0
#> Item10    0    1    0    0
#> Item11    0    0    1    0
#> Item12    0    0    1    0
#> Item13    0    0    1    0
#> Item14    0    0    1    0
#> Item15    0    0    1    0
#> Item16    0    0    0    1
#> Item17    0    0    0    1
#> Item18    0    0    0    1
#> Item19    0    0    0    1
#> Item20    0    0    0    1
#> 
#> -----------------------------------------------------------------
#> 
#> Item Parameter Estimates 
#> 
#>     link   item itemno partype  rule     est     se partype.attr
#> 1  logit  Item1      1       0 GDINA -1.5972 0.1281             
#> 2  logit  Item1      1       1 GDINA  1.7928 0.1710         Att1
#> 3  logit  Item2      2       0 GDINA -1.8484 0.1398             
#> 4  logit  Item2      2       1 GDINA  1.7928 0.1796         Att1
#> 5  logit  Item3      3       0 GDINA -1.2660 0.1157             
#> 6  logit  Item3      3       1 GDINA  1.7928 0.1643         Att1
#> 7  logit  Item4      4       0 GDINA -1.4336 0.1216             
#> 8  logit  Item4      4       1 GDINA  1.7928 0.1670         Att1
#> 9  logit  Item5      5       0 GDINA -1.5713 0.1270             
#> 10 logit  Item5      5       1 GDINA  1.7928 0.1703         Att1
#> 11 logit  Item6      6       0 GDINA -1.4673 0.1418             
#> 12 logit  Item6      6       1 GDINA  2.4114 0.1784         Att2
#> 13 logit  Item7      7       0 GDINA -1.5011 0.1434             
#> 14 logit  Item7      7       1 GDINA  2.4114 0.1791         Att2
#> 15 logit  Item8      8       0 GDINA -0.9655 0.1238             
#> 16 logit  Item8      8       1 GDINA  2.4114 0.1750         Att2
#> 17 logit  Item9      9       0 GDINA -1.0838 0.1274             
#> 18 logit  Item9      9       1 GDINA  2.4114 0.1746         Att2
#> 19 logit Item10     10       0 GDINA -1.2169 0.1318             
#> 20 logit Item10     10       1 GDINA  2.4114 0.1750         Att2
#> 21 logit Item11     11       0 GDINA -1.2240 0.1131             
#> 22 logit Item11     11       1 GDINA  2.9666 0.1967         Att3
#> 23 logit Item12     12       0 GDINA -1.6220 0.1277             
#> 24 logit Item12     12       1 GDINA  2.9666 0.1906         Att3
#> 25 logit Item13     13       0 GDINA -1.4200 0.1197             
#> 26 logit Item13     13       1 GDINA  2.9666 0.1924         Att3
#> 27 logit Item14     14       0 GDINA -1.1768 0.1117             
#> 28 logit Item14     14       1 GDINA  2.9666 0.1981         Att3
#> 29 logit Item15     15       0 GDINA -1.4495 0.1208             
#> 30 logit Item15     15       1 GDINA  2.9666 0.1919         Att3
#> 31 logit Item16     16       0 GDINA -1.2130 0.1774             
#> 32 logit Item16     16       1 GDINA  2.7803 0.2092         Att4
#> 33 logit Item17     17       0 GDINA -1.6921 0.2057             
#> 34 logit Item17     17       1 GDINA  2.7803 0.2272         Att4
#> 35 logit Item18     18       0 GDINA -1.3103 0.1823             
#> 36 logit Item18     18       1 GDINA  2.7803 0.2116         Att4
#> 37 logit Item19     19       0 GDINA -1.5943 0.1990             
#> 38 logit Item19     19       1 GDINA  2.7803 0.2223         Att4
#> 39 logit Item20     20       0 GDINA -1.3030 0.1819             
#> 40 logit Item20     20       1 GDINA  2.7803 0.2114         Att4
#> 
#> Note: Standard errors are not (yet) correctly implemented!
#> 
#> RMSD (RMSEA) Item Fit Statistics
#>  Item1  Item2  Item3  Item4  Item5  Item6  Item7  Item8  Item9 Item10 Item11 
#>  0.161  0.144  0.184  0.160  0.192  0.196  0.121  0.143  0.137  0.160  0.078 
#> Item12 Item13 Item14 Item15 Item16 Item17 Item18 Item19 Item20 
#>  0.077  0.070  0.065  0.069  0.254  0.173  0.147  0.112  0.125 
#> 
#> Mean of RMSEA item fit: 0.138 
#> -----------------------------------------------------------------
#> Model Implied Conditional Item Probabilities 
#> 
#>      item  rule nessskill itemno skillcomb   prob
#> 1   Item1 GDINA      Att1      1        A0 0.1684
#> 2   Item1 GDINA      Att1      1        A1 0.5487
#> 3   Item2 GDINA      Att1      2        A0 0.1361
#> 4   Item2 GDINA      Att1      2        A1 0.4861
#> 5   Item3 GDINA      Att1      3        A0 0.2200
#> 6   Item3 GDINA      Att1      3        A1 0.6287
#> 7   Item4 GDINA      Att1      4        A0 0.1925
#> 8   Item4 GDINA      Att1      4        A1 0.5888
#> 9   Item5 GDINA      Att1      5        A0 0.1720
#> 10  Item5 GDINA      Att1      5        A1 0.5551
#> 11  Item6 GDINA      Att2      6        A0 0.1874
#> 12  Item6 GDINA      Att2      6        A1 0.7199
#> 13  Item7 GDINA      Att2      7        A0 0.1823
#> 14  Item7 GDINA      Att2      7        A1 0.7131
#> 15  Item8 GDINA      Att2      8        A0 0.2758
#> 16  Item8 GDINA      Att2      8        A1 0.8094
#> 17  Item9 GDINA      Att2      9        A0 0.2528
#> 18  Item9 GDINA      Att2      9        A1 0.7904
#> 19 Item10 GDINA      Att2     10        A0 0.2285
#> 20 Item10 GDINA      Att2     10        A1 0.7675
#> 21 Item11 GDINA      Att3     11        A0 0.2272
#> 22 Item11 GDINA      Att3     11        A1 0.8510
#> 23 Item12 GDINA      Att3     12        A0 0.1649
#> 24 Item12 GDINA      Att3     12        A1 0.7932
#> 25 Item13 GDINA      Att3     13        A0 0.1947
#> 26 Item13 GDINA      Att3     13        A1 0.8244
#> 27 Item14 GDINA      Att3     14        A0 0.2356
#> 28 Item14 GDINA      Att3     14        A1 0.8569
#> 29 Item15 GDINA      Att3     15        A0 0.1901
#> 30 Item15 GDINA      Att3     15        A1 0.8201
#> 31 Item16 GDINA      Att4     16        A0 0.2292
#> 32 Item16 GDINA      Att4     16        A1 0.8274
#> 33 Item17 GDINA      Att4     17        A0 0.1555
#> 34 Item17 GDINA      Att4     17        A1 0.7480
#> 35 Item18 GDINA      Att4     18        A0 0.2124
#> 36 Item18 GDINA      Att4     18        A1 0.8131
#> 37 Item19 GDINA      Att4     19        A0 0.1688
#> 38 Item19 GDINA      Att4     19        A1 0.7660
#> 39 Item20 GDINA      Att4     20        A0 0.2137
#> 40 Item20 GDINA      Att4     20        A1 0.8142
#> -----------------------------------------------------------------
#> 
#> Skill Probabilities 
#> 
#>      skill.prob0 skill.prob1
#> Att1      0.5803      0.4197
#> Att2      0.4352      0.5648
#> Att3      0.5938      0.4062
#> Att4      0.2399      0.7601
#> -----------------------------------------------------------------
#> 
#> Polychoric Correlations 
#> 
#> Group 1
#>      Att1 Att2 Att3 Att4
#> Att1    1    0    0    0
#> Att2    0    1    0    0
#> Att3    0    0    1    0
#> Att4    0    0    0    1
#> 
#>  -----------------------------------------------------------------
#> 
#> Skill Pattern Probabilities 
#> 
#>   0000   1000   0100   1100   0010   1010   0110   1110   0001   1001   0101 
#> 0.0360 0.0260 0.0467 0.0338 0.0246 0.0178 0.0319 0.0231 0.1140 0.0824 0.1479 
#>   1101   0011   1011   0111   1111 
#> 0.1070 0.0780 0.0564 0.1012 0.0732 
#> 
#>  -----------------------------------------------------------------
#> Higher Order GDINA Model 
#>   Attribute Response Function Parameters 
#> 
#>       b.Gr1 a.Gr1 int.Gr1
#> Att1  0.744     0    -Inf
#> Att2 -0.047     0     Inf
#> Att3  0.190     0    -Inf
#> Att4 -0.698     0     Inf
#demonstrate 1-PLCDM sum score sufficiency for each attribute
subscores <- cbind(rowSums(dat5[, 1:5]), rowSums(dat5[, 6:10]),
rowSums(dat5[, 11:15]), rowSums(dat5[, 16:20]))
colnames(subscores) <- c("Att1", "Att2", "Att3", "Att4")
proficiency <- cbind(m2$pattern[, 6] > .50, m2$pattern[, 7] > .50,
m2$pattern[, 8] > .50, m2$pattern[, 9] > .5) * 1
table(subscores[, 1], proficiency[, 1])
#>    
#>       0   1
#>   0 150   0
#>   1 250   0
#>   2   0 156
#>   3   0  95
#>   4   0  71
#>   5   0  28
table(subscores[, 2], proficiency[, 2])
#>    
#>       0   1
#>   0  88   0
#>   1 149   0
#>   2 106   0
#>   3   0 138
#>   4   0 163
#>   5   0 106
table(subscores[, 3], proficiency[, 3])
#>    
#>       0   1
#>   0 136   0
#>   1 209   0
#>   2  74   0
#>   3   0  86
#>   4   0 131
#>   5   0 114
table(subscores[, 4], proficiency[, 4])
#>    
#>       0   1
#>   0  68   0
#>   1  76   0
#>   2   0  62
#>   3   0 127
#>   4   0 254
#>   5   0 163

#plot sum score sufficiency for each attribute
posterior1pl <- m2$pattern[, 6:9]
posteriorlcdm <- m1$pattern[, 6:9]
oldpar <- par(mfrow = c(2, 2))
for (i in 1:4) {
 plot(subscores[, i], posteriorlcdm[, i], pch = 19,las = 1, cex.lab = 1.5,
 xlab = "Sum Scores", ylab = "P(proficiency)",
 cex.main = 1.5, col = "grey", xaxt = "n", yaxt = "n", cex = 1.2,
 main = paste("Attribute ", i, sep = ""))
 graphics::axis(side = 1, at = c(0, 1, 2, 3, 4, 5), )
 graphics::axis(side = 2, at = c(0, .1, .2, .3, .4, .5, .6, .7, .8, .9, 1.0), las = 1)
 graphics::points(subscores[, i], posterior1pl[, i], col = "black", pch = 18, cex = 1.5)
 graphics::abline(a = .50, b = 0, col = "red")
 graphics::legend("bottomright", c("1-PLCDM", "LCDM"), col = c("black", "grey"),
 pch = c(18 ,19), box.lwd = 0, box.col = "white", bty = 'n')
}

par(oldpar)
# }
```
