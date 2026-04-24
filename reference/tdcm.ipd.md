# Assessing item parameter drift (IPD) in the Transition Diagnostic Classification Model (TDCM)

The `tdcm.ipd()` function assesses item parameter drift (IPD) in the
TDCM (e.g., Madison & Bradshaw, 2018) by applying the Wald test for
differential item functioning (de la Torre, 2011; Hou, de la Torre &
Nandakumar, 2014). The *p*-values are also calculated by a Holm
adjustment for multiple comparisons. In the case of two time points, an
effect size of item parameter drift (labeled as UA in the ipd.stats
value) is defined as the weighted absolute difference of item response
functions.

## Usage

``` r
tdcm.ipd(model)
```

## Arguments

- model:

  A `tdcm` object returned from the
  [`tdcm`](https://cotterell.github.io/tdcm/reference/tdcm.md) function.

## Value

A list with the following items:

- `$ipd.stats`: Data frame containing results of item-wise Wald tests.

- `$coef`: Data frame containing item parameter estimates for each time
  point.

- `$estimates`: List of \\\lambda\\ vectors containing all item
  parameter estimates.

- `$item.probs.time`: List with predicted item response probabilities
  for each time point.

## References

de la Torre, J. (2011). The Generalized DINA model framework.
*Psychometrika, 76*, 179–199. <doi:10.1007/s11336-011-9207-7>.

Hou, L., de la Torre, J., & Nandakumar, R. (2014). Differential item
functioning assessment in cognitive diagnostic modeling: Application of
the Wald test to investigate DIF in the DINA model. *Journal of
Educational Measurement, 51*, 98-125. <doi:10.1111/jedm.12036>.

Madison, M. J., & Bradshaw, L. (2018a). Assessing growth in a diagnostic
classification model framework. *Psychometrika*, **83**(4), 963-990.
<doi:10.1007/s11336-018-9638-5>.

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

# Run IPD analysis
ipd = tdcm.ipd(model1)
ipd$ipd.stats
#>      item         X2 df         p p.holm          UA
#> 1   Item1 0.22156469  2 0.8951336      1 0.008980171
#> 2   Item2 0.07761982  2 0.9619335      1 0.005209066
#> 3   Item3 0.88000141  2 0.6440360      1 0.011576302
#> 4   Item4 3.53498844  4 0.4725783      1 0.040003125
#> 5   Item5 0.46599238  4 0.9767261      1 0.018472372
#> 6   Item6 0.82278150  2 0.6627279      1 0.015561819
#> 7   Item7 0.38751170  2 0.8238590      1 0.010677069
#> 8   Item8 0.33877604  2 0.8441813      1 0.009624126
#> 9   Item9 4.81487276  4 0.3068255      1 0.029992050
#> 10 Item10 0.34295555  4 0.9868751      1 0.017882758
#> 11 Item11 1.62618390  2 0.4434847      1 0.013465488
#> 12 Item12 0.69515863  2 0.7063960      1 0.012707218
#> 13 Item13 1.76810231  2 0.4131060      1 0.010936366
#> 14 Item14 2.99630231  4 0.5584444      1 0.039664282
#> 15 Item15 1.82736575  4 0.7674720      1 0.021706705
#> 16 Item16 0.92517994  2 0.6296508      1 0.009828840
#> 17 Item17 0.66787532  2 0.7160984      1 0.013790663
#> 18 Item18 0.36405703  2 0.8335776      1 0.010721126
#> 19 Item19 0.65966825  4 0.9562079      1 0.034882144
#> 20 Item20 4.92455253  4 0.2951269      1 0.096273056
ipd$coef
#>             Link   Item Item Number Param Type Rule        Est         SE
#> Item1_0    logit  Item1           1          0 LCDM -1.9181441 0.07881600
#> Item1_1    logit  Item1           1          1 LCDM  2.6121266 0.11946880
#> Item2_0    logit  Item2           2          0 LCDM -2.0705259 0.08351611
#> Item2_1    logit  Item2           2          1 LCDM  2.5172348 0.12042195
#> Item3_0    logit  Item3           3          0 LCDM -1.9362663 0.07935031
#> Item3_1    logit  Item3           3          1 LCDM  2.5263715 0.11875002
#> Item4_0    logit  Item4           4          0 LCDM -1.9199198 0.09208705
#> Item4_1    logit  Item4           4          1 LCDM  1.0225220 0.22366649
#> Item4_2    logit  Item4           4          2 LCDM  1.4950561 0.13917614
#> Item4_1-2  logit  Item4           4        1-2 LCDM  1.1698458 0.28125714
#> Item5_0    logit  Item5           5          0 LCDM -2.1618846 0.10571189
#> Item5_1    logit  Item5           5          1 LCDM  1.6092665 0.28406789
#> Item5_2    logit  Item5           5          2 LCDM  1.7547667 0.14124883
#> Item5_1-2  logit  Item5           5        1-2 LCDM  0.8682878 0.33125169
#> Item6_0    logit  Item6           6          0 LCDM -1.8692012 0.08575761
#> Item6_1    logit  Item6           6          1 LCDM  2.1831830 0.11099537
#> Item7_0    logit  Item7           7          0 LCDM -1.8581021 0.08541033
#> Item7_1    logit  Item7           7          1 LCDM  2.2645963 0.11109938
#> Item8_0    logit  Item8           8          0 LCDM -1.9975654 0.08997004
#> Item8_1    logit  Item8           8          1 LCDM  2.4915166 0.11507136
#> Item9_0    logit  Item9           9          0 LCDM -2.0659941 0.10704271
#> Item9_1    logit  Item9           9          1 LCDM  1.2450458 0.20495828
#> Item9_2    logit  Item9           9          2 LCDM  1.5332934 0.16050040
#> Item9_1-2  logit  Item9           9        1-2 LCDM  1.6512037 0.27433883
#> Item10_0   logit Item10          10          0 LCDM -2.0087885 0.10431434
#> Item10_1   logit Item10          10          1 LCDM  1.7363525 0.18545437
#> Item10_2   logit Item10          10          2 LCDM  1.2912781 0.16233032
#> Item10_1-2 logit Item10          10        1-2 LCDM  1.1061509 0.25679840
#> Item11_0   logit Item11          11          0 LCDM -1.8747240 0.09181673
#> Item11_1   logit Item11          11          1 LCDM  2.3642124 0.11313816
#> Item12_0   logit Item12          12          0 LCDM -2.0744631 0.09901659
#> Item12_1   logit Item12          12          1 LCDM  2.5902837 0.11917466
#> Item13_0   logit Item13          13          0 LCDM -2.0577346 0.09837709
#> Item13_1   logit Item13          13          1 LCDM  2.5397540 0.11849136
#> Item14_0   logit Item14          14          0 LCDM -2.0583473 0.11117160
#> Item14_1   logit Item14          14          1 LCDM  1.6825024 0.17037831
#> Item14_2   logit Item14          14          2 LCDM  2.0351168 0.17411341
#> Item14_1-2 logit Item14          14        1-2 LCDM  0.3926852 0.24636899
#> Item15_0   logit Item15          15          0 LCDM -1.8192094 0.09282435
#> Item15_1   logit Item15          15          1 LCDM  0.7280376 0.30701391
#> Item15_2   logit Item15          15          2 LCDM  1.3301282 0.13248632
#> Item15_1-2 logit Item15          15        1-2 LCDM  1.9201833 0.35342728
#> Item16_0   logit Item16          16          0 LCDM -2.0919725 0.09846987
#> Item16_1   logit Item16          16          1 LCDM  2.6443799 0.11940543
#> Item17_0   logit Item17          17          0 LCDM -2.0909092 0.09842870
#> Item17_1   logit Item17          17          1 LCDM  2.7072410 0.11972119
#> Item18_0   logit Item18          18          0 LCDM -2.0697180 0.09762110
#> Item18_1   logit Item18          18          1 LCDM  2.4378212 0.11792010
#> Item19_0   logit Item19          19          0 LCDM -2.1233179 0.10295081
#> Item19_1   logit Item19          19          1 LCDM  2.4300883 0.26986268
#> Item19_2   logit Item19          19          2 LCDM  1.5012144 0.14252146
#> Item19_1-2 logit Item19          19        1-2 LCDM  0.2183443 0.31979702
#> Item20_0   logit Item20          20          0 LCDM -2.0188010 0.10471455
#> Item20_1   logit Item20          20          1 LCDM  1.7623339 0.18555052
#> Item20_2   logit Item20          20          2 LCDM  1.3839745 0.16132176
#> Item20_1-2 logit Item20          20        1-2 LCDM  0.8721858 0.25315557
#>                 Attr  Est_Time1  SE_Time1  Est_Time2  SE_Time2
#> Item1_0              -1.9394905 0.1060563 -1.8913855 0.1178005
#> Item1_1         Att1  2.6786282 0.1875512  2.5621712 0.1613610
#> Item2_0              -2.0655097 0.1112725 -2.0770304 0.1263879
#> Item2_1         Att1  2.5448357 0.1858570  2.5068802 0.1654496
#> Item3_0              -2.0018560 0.1085837 -1.8568750 0.1163185
#> Item3_1         Att1  2.5647227 0.1855489  2.4612483 0.1595113
#> Item4_0              -2.0317093 0.1245540 -1.7710159 0.1370468
#> Item4_1         Att1  1.3941485 0.3583598  0.7317442 0.2921050
#> Item4_2         Att2  1.6181206 0.1967423  1.3361739 0.1982667
#> Item4_1-2  Att1-Att2  0.6447309 0.4466662  1.5514364 0.3677453
#> Item5_0              -2.1385808 0.1348117 -2.1983409 0.1703671
#> Item5_1         Att1  1.6948946 0.4603497  1.5857696 0.3710749
#> Item5_2         Att3  1.7263355 0.1915294  1.7958340 0.2138092
#> Item5_1-2  Att1-Att3  0.8998252 0.5433736  0.8328820 0.4288022
#> Item6_0              -1.8847639 0.1143494 -1.8489924 0.1296533
#> Item6_1         Att2  2.2751615 0.1599827  2.1121368 0.1582813
#> Item7_0              -1.8656031 0.1135484 -1.8483267 0.1296218
#> Item7_1         Att2  2.3256498 0.1599874  2.2191586 0.1586993
#> Item8_0              -1.9959266 0.1192080 -1.9998027 0.1371460
#> Item8_1         Att2  2.5411135 0.1648703  2.4596833 0.1653690
#> Item9_0              -2.2623561 0.1497788 -1.8209200 0.1538178
#> Item9_1         Att2  1.5535282 0.2771132  0.8618754 0.3065097
#> Item9_2         Att3  1.7528186 0.2271841  1.2660390 0.2274215
#> Item9_1-2  Att2-Att3  1.2804863 0.3943612  2.0802684 0.3907802
#> Item10_0             -2.0286783 0.1265802 -1.9657871 0.1841945
#> Item10_1        Att2  1.7437274 0.2088202  1.7660030 0.4399032
#> Item10_2        Att4  1.2483235 0.3044172  1.2643659 0.2309030
#> Item10_1-2 Att2-Att4  1.0831072 0.4170480  1.0926936 0.4856761
#> Item11_0             -1.9752677 0.1242769 -1.7428452 0.1364784
#> Item11_1        Att3  2.4809023 0.1619812  2.2213207 0.1611512
#> Item12_0             -2.1318363 0.1320243 -1.9967551 0.1497770
#> Item12_1        Att3  2.6869122 0.1684125  2.4859254 0.1726130
#> Item13_0             -1.9527347 0.1232259 -2.2245165 0.1637565
#> Item13_1        Att3  2.4425194 0.1610470  2.7012347 0.1848149
#> Item14_0             -1.9917488 0.1298363 -2.2259973 0.2156643
#> Item14_1        Att3  1.6523938 0.1950924  1.7176166 0.3534159
#> Item14_2        Att4  1.9845776 0.3290908  2.1988416 0.2624030
#> Item14_1-2 Att3-Att4  0.6946480 0.4378059  0.2661592 0.4060766
#> Item15_0             -1.9183836 0.1238926 -1.6806192 0.1404227
#> Item15_1        Att1  0.6719940 0.5303650  0.6677128 0.3827256
#> Item15_2        Att3  1.4251722 0.1849191  1.1952457 0.1916060
#> Item15_1-2 Att1-Att3  1.9256480 0.6009015  2.0062347 0.4440953
#> Item16_0             -2.0338403 0.1137357 -2.2521703 0.1971267
#> Item16_1        Att4  2.5942329 0.1749861  2.8018064 0.2121456
#> Item17_0             -2.0855375 0.1160334 -2.1047261 0.1858854
#> Item17_1        Att4  2.6092859 0.1760042  2.7539344 0.2021908
#> Item18_0             -2.0489588 0.1143995 -2.1238839 0.1872869
#> Item18_1        Att4  2.4731057 0.1737813  2.4725936 0.2023773
#> Item19_0             -2.1278381 0.1227006 -2.1126437 0.1892351
#> Item19_1        Att1  2.4743809 0.2953007  2.1626656 0.7048039
#> Item19_2        Att4  1.4714515 0.2351649  1.5015040 0.2204878
#> Item19_1-2 Att1-Att4  0.3732437 0.4582551  0.4183436 0.7317910
#> Item20_0             -2.0985278 0.1300515 -1.8574051 0.1769300
#> Item20_1        Att2  1.7255098 0.2118868  2.2725733 0.4429452
#> Item20_2        Att4  1.6061008 0.2950766  1.1845196 0.2247439
#> Item20_1-2 Att2-Att4  0.8391367 0.4120808  0.3726742 0.4850887
ipd$parameters
#> NULL

# }
```
