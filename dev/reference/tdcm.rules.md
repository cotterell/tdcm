# TDCM Condensation Rules

A *condensation rule* is a formula that states how different attributes
combine to form an observed or latent response (Rupp, Templin, & Henson,
2010). The TDCM package includes support for `"LCDM"`, `"DINA"`,
`"DINO"`, `"CRUM"`, `"RRUM"`, `"LCDM1"` for the LCDM with only main
effects, `"LCDM2"` for the LCDM with two-way interactions, `"LCDM3"`,
and so on. Evaluate `TDCM::tdcm.rules$TDCM` for a complete list of
condensation rules supported by the TDCM package.

## Usage

``` r
tdcm.rules
```

## Format

An object of class `data.frame` with 15 rows and 2 columns.

## References

Rupp, A. A., Templin, J., & Henson, R. (2010). *Diagnostic Measurement:
Theory, Methods, and Applications*. New York: Guilford. ISBN:
9781606235430.

## Examples

``` r
TDCM::tdcm.rules$TDCM
#>  [1] "LCDM"   "CRUM"   "DINA"   "DINO"   "RRUM"   "LCDM1"  "LCDM2"  "LCDM3" 
#>  [9] "LCDM4"  "LCDM5"  "LCDM6"  "LCDM7"  "LCDM8"  "LCDM9"  "LCDM10"
```
