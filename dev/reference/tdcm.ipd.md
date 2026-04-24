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
  [`tdcm`](https://cotterell.github.io/tdcm/dev/reference/tdcm.md)
  function.

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
#> 1   Item1 0.22156494  2 0.8951334      1 0.008980171
#> 2   Item2 0.07761981  2 0.9619335      1 0.005209066
#> 3   Item3 0.88000391  2 0.6440352      1 0.011576302
#> 4   Item4 3.53489276  4 0.4725927      1 0.040003125
#> 5   Item5 0.46599051  4 0.9767262      1 0.018472372
#> 6   Item6 0.82277945  2 0.6627286      1 0.015561819
#> 7   Item7 0.38751089  2 0.8238594      1 0.010677069
#> 8   Item8 0.33877518  2 0.8441816      1 0.009624126
#> 9   Item9 4.81499266  4 0.3068125      1 0.029992050
#> 10 Item10 0.34295503  4 0.9868752      1 0.017882758
#> 11 Item11 1.62617913  2 0.4434858      1 0.013465488
#> 12 Item12 0.69516070  2 0.7063953      1 0.012707218
#> 13 Item13 1.76810278  2 0.4131059      1 0.010936366
#> 14 Item14 2.99629441  4 0.5584457      1 0.039664282
#> 15 Item15 1.82735865  4 0.7674733      1 0.021706705
#> 16 Item16 0.92517478  2 0.6296524      1 0.009828840
#> 17 Item17 0.66787410  2 0.7160989      1 0.013790663
#> 18 Item18 0.36405631  2 0.8335779      1 0.010721126
#> 19 Item19 0.65966514  4 0.9562083      1 0.034882144
#> 20 Item20 4.92449124  4 0.2951333      1 0.096273056
ipd$coef
#>             Link   Item Item Number Param Type Rule        Est         SE
#> Item1_0    logit  Item1           1          0 LCDM -1.9181441 0.07881590
#> Item1_1    logit  Item1           1          1 LCDM  2.6121266 0.11946865
#> Item2_0    logit  Item2           2          0 LCDM -2.0705259 0.08351593
#> Item2_1    logit  Item2           2          1 LCDM  2.5172348 0.12042192
#> Item3_0    logit  Item3           3          0 LCDM -1.9362663 0.07935035
#> Item3_1    logit  Item3           3          1 LCDM  2.5263715 0.11874995
#> Item4_0    logit  Item4           4          0 LCDM -1.9199198 0.09208604
#> Item4_1    logit  Item4           4          1 LCDM  1.0225220 0.22365473
#> Item4_2    logit  Item4           4          2 LCDM  1.4950561 0.13917342
#> Item4_1-2  logit  Item4           4        1-2 LCDM  1.1698458 0.28124031
#> Item5_0    logit  Item5           5          0 LCDM -2.1618846 0.10571188
#> Item5_1    logit  Item5           5          1 LCDM  1.6092665 0.28405892
#> Item5_2    logit  Item5           5          2 LCDM  1.7547667 0.14124852
#> Item5_1-2  logit  Item5           5        1-2 LCDM  0.8682878 0.33124386
#> Item6_0    logit  Item6           6          0 LCDM -1.8692012 0.08575760
#> Item6_1    logit  Item6           6          1 LCDM  2.1831830 0.11099531
#> Item7_0    logit  Item7           7          0 LCDM -1.8581021 0.08541016
#> Item7_1    logit  Item7           7          1 LCDM  2.2645963 0.11109917
#> Item8_0    logit  Item8           8          0 LCDM -1.9975654 0.08997007
#> Item8_1    logit  Item8           8          1 LCDM  2.4915166 0.11507130
#> Item9_0    logit  Item9           9          0 LCDM -2.0659941 0.10704360
#> Item9_1    logit  Item9           9          1 LCDM  1.2450458 0.20496168
#> Item9_2    logit  Item9           9          2 LCDM  1.5332934 0.16050118
#> Item9_1-2  logit  Item9           9        1-2 LCDM  1.6512037 0.27434343
#> Item10_0   logit Item10          10          0 LCDM -2.0087885 0.10431480
#> Item10_1   logit Item10          10          1 LCDM  1.7363525 0.18545847
#> Item10_2   logit Item10          10          2 LCDM  1.2912781 0.16233267
#> Item10_1-2 logit Item10          10        1-2 LCDM  1.1061509 0.25680926
#> Item11_0   logit Item11          11          0 LCDM -1.8747240 0.09181699
#> Item11_1   logit Item11          11          1 LCDM  2.3642124 0.11313837
#> Item12_0   logit Item12          12          0 LCDM -2.0744631 0.09901677
#> Item12_1   logit Item12          12          1 LCDM  2.5902837 0.11917473
#> Item13_0   logit Item13          13          0 LCDM -2.0577346 0.09837650
#> Item13_1   logit Item13          13          1 LCDM  2.5397540 0.11849079
#> Item14_0   logit Item14          14          0 LCDM -2.0583473 0.11117234
#> Item14_1   logit Item14          14          1 LCDM  1.6825024 0.17038050
#> Item14_2   logit Item14          14          2 LCDM  2.0351168 0.17411244
#> Item14_1-2 logit Item14          14        1-2 LCDM  0.3926852 0.24636611
#> Item15_0   logit Item15          15          0 LCDM -1.8192094 0.09282417
#> Item15_1   logit Item15          15          1 LCDM  0.7280376 0.30702143
#> Item15_2   logit Item15          15          2 LCDM  1.3301282 0.13248558
#> Item15_1-2 logit Item15          15        1-2 LCDM  1.9201833 0.35343756
#> Item16_0   logit Item16          16          0 LCDM -2.0919725 0.09846978
#> Item16_1   logit Item16          16          1 LCDM  2.6443799 0.11940550
#> Item17_0   logit Item17          17          0 LCDM -2.0909092 0.09842882
#> Item17_1   logit Item17          17          1 LCDM  2.7072410 0.11972129
#> Item18_0   logit Item18          18          0 LCDM -2.0697180 0.09762123
#> Item18_1   logit Item18          18          1 LCDM  2.4378212 0.11792020
#> Item19_0   logit Item19          19          0 LCDM -2.1233179 0.10295045
#> Item19_1   logit Item19          19          1 LCDM  2.4300883 0.26984706
#> Item19_2   logit Item19          19          2 LCDM  1.5012144 0.14251943
#> Item19_1-2 logit Item19          19        1-2 LCDM  0.2183443 0.31977057
#> Item20_0   logit Item20          20          0 LCDM -2.0188010 0.10471475
#> Item20_1   logit Item20          20          1 LCDM  1.7623339 0.18555134
#> Item20_2   logit Item20          20          2 LCDM  1.3839745 0.16132113
#> Item20_1-2 logit Item20          20        1-2 LCDM  0.8721858 0.25315534
#>                 Attr  Est_Time1  SE_Time1  Est_Time2  SE_Time2
#> Item1_0              -1.9394905 0.1060562 -1.8913855 0.1178006
#> Item1_1         Att1  2.6786282 0.1875511  2.5621712 0.1613609
#> Item2_0              -2.0655097 0.1112725 -2.0770304 0.1263876
#> Item2_1         Att1  2.5448357 0.1858570  2.5068802 0.1654492
#> Item3_0              -2.0018560 0.1085838 -1.8568750 0.1163181
#> Item3_1         Att1  2.5647227 0.1855493  2.4612483 0.1595110
#> Item4_0              -2.0317093 0.1245534 -1.7710159 0.1370463
#> Item4_1         Att1  1.3941485 0.3583639  0.7317442 0.2921121
#> Item4_2         Att2  1.6181206 0.1967414  1.3361739 0.1982648
#> Item4_1-2  Att1-Att2  0.6447309 0.4466783  1.5514364 0.3677522
#> Item5_0              -2.1385808 0.1348116 -2.1983409 0.1703673
#> Item5_1         Att1  1.6948946 0.4603268  1.5857696 0.3710850
#> Item5_2         Att3  1.7263355 0.1915280  1.7958340 0.2138092
#> Item5_1-2  Att1-Att3  0.8998252 0.5433431  0.8328820 0.4288094
#> Item6_0              -1.8847639 0.1143496 -1.8489924 0.1296536
#> Item6_1         Att2  2.2751615 0.1599833  2.1121368 0.1582816
#> Item7_0              -1.8656031 0.1135478 -1.8483267 0.1296221
#> Item7_1         Att2  2.3256498 0.1599871  2.2191586 0.1586998
#> Item8_0              -1.9959266 0.1192084 -1.9998027 0.1371462
#> Item8_1         Att2  2.5411135 0.1648709  2.4596833 0.1653694
#> Item9_0              -2.2623561 0.1497793 -1.8209200 0.1538150
#> Item9_1         Att2  1.5535282 0.2771144  0.8618754 0.3065003
#> Item9_2         Att3  1.7528186 0.2271841  1.2660390 0.2274169
#> Item9_1-2  Att2-Att3  1.2804863 0.3943623  2.0802684 0.3907709
#> Item10_0             -2.0286783 0.1265801 -1.9657871 0.1841921
#> Item10_1        Att2  1.7437274 0.2088214  1.7660030 0.4398953
#> Item10_2        Att4  1.2483235 0.3044225  1.2643659 0.2308997
#> Item10_1-2 Att2-Att4  1.0831072 0.4170557  1.0926936 0.4856658
#> Item11_0             -1.9752677 0.1242772 -1.7428452 0.1364786
#> Item11_1        Att3  2.4809023 0.1619815  2.2213207 0.1611512
#> Item12_0             -2.1318363 0.1320241 -1.9967551 0.1497766
#> Item12_1        Att3  2.6869122 0.1684124  2.4859254 0.1726126
#> Item13_0             -1.9527347 0.1232261 -2.2245165 0.1637563
#> Item13_1        Att3  2.4425194 0.1610473  2.7012347 0.1848150
#> Item14_0             -1.9917488 0.1298376 -2.2259973 0.2156657
#> Item14_1        Att3  1.6523938 0.1950955  1.7176166 0.3534194
#> Item14_2        Att4  1.9845776 0.3290978  2.1988416 0.2624041
#> Item14_1-2 Att3-Att4  0.6946480 0.4378204  0.2661592 0.4060766
#> Item15_0             -1.9183836 0.1238930 -1.6806192 0.1404240
#> Item15_1        Att1  0.6719940 0.5303575  0.6677128 0.3827267
#> Item15_2        Att3  1.4251722 0.1849192  1.1952457 0.1916068
#> Item15_1-2 Att1-Att3  1.9256480 0.6008989  2.0062347 0.4440957
#> Item16_0             -2.0338403 0.1137355 -2.2521703 0.1971275
#> Item16_1        Att4  2.5942329 0.1749859  2.8018064 0.2121464
#> Item17_0             -2.0855375 0.1160338 -2.1047261 0.1858858
#> Item17_1        Att4  2.6092859 0.1760047  2.7539344 0.2021909
#> Item18_0             -2.0489588 0.1143997 -2.1238839 0.1872890
#> Item18_1        Att4  2.4731057 0.1737811  2.4725936 0.2023796
#> Item19_0             -2.1278381 0.1227012 -2.1126437 0.1892324
#> Item19_1        Att1  2.4743809 0.2952999  2.1626656 0.7047962
#> Item19_2        Att4  1.4714515 0.2351695  1.5015040 0.2204843
#> Item19_1-2 Att1-Att4  0.3732437 0.4582621  0.4183436 0.7317818
#> Item20_0             -2.0985278 0.1300517 -1.8574051 0.1769326
#> Item20_1        Att2  1.7255098 0.2118873  2.2725733 0.4429592
#> Item20_2        Att4  1.6061008 0.2950766  1.1845196 0.2247485
#> Item20_1-2 Att2-Att4  0.8391367 0.4120811  0.3726742 0.4851088
ipd$parameters
#> NULL

# }
```
