# Several data sets for the TDCM package.

Several data sets for the TDCM package.

## Usage

``` r
data.tdcm01

data.tdcm02

data.tdcm03

data.tdcm04

data.tdcm05
```

## Format

`data.tdcm01` is a simulated dataset with two time points, four
attributes, twenty items, one group of size 1000, and a single Q-matrix.
The format is a list of two:

- `data`: a data frame of binary item responses

- `q.matrix`: a data frame specifying the Q-matrix

`data.tdcm02` is a simulated dataset with three time points, two
attributes, ten items, one group of size 2500, and a single Q-matrix.
The format is a list of two:

- `data`: a data frame of binary item responses

- `q.matrix`: a data frame specifying the Q-matrix

`data.tdcm03` is a simulated dataset with three time points, two
attributes, one group of size 1500, and three different ten-item
Q-matrices for each time point. Anchor items are specified as items
1/11/21 and items 14/24. The format is a list of five:

- `data`: a data frame of binary item responses

- `q.matrix.1`: a data frame specifying the Q-matrix for the first time
  point

- `q.matrix.2`: a data frame specifying the Q-matrix for the second time
  point

- `q.matrix.3`: a data frame specifying the Q-matrix for the third time
  point

- `q.matrix.stacked`: data frame specifying the combined Q-matrix for
  all time points

`data.tdcm04` is a simulated dataset with two time points, four
attributes, twenty items, two group of size 800 and 900, respectively,
and a single Q-matrix. The format is a list of three:

- `data`: a data frame of binary item responses

- `q.matrix`: a data frame specifying the Q-matrix

- `groups`: a vector specifying the examinee group membership

`data.tdcm05` is a simulated dataset with one time point, four
attributes, and twenty items. For use with the 1-PLCDM. The format is a
list of two:

- `data`: a data frame of binary item responses

- `q.matrix`: a data frame specifying the Q-matrix

## Examples

``` r
# \donttest{

#############################
## Example 1: T = 2, A = 4 ##
data(data.tdcm01, package = "TDCM")
data <- data.tdcm01$data
q.matrix <- data.tdcm01$q.matrix
model <- TDCM::tdcm(data, q.matrix, num.time.points = 2)
#> [1] Preparing data for tdcm()...
#> [1] Estimating the TDCM in tdcm()...
#> [1] Depending on model complexity, estimation time may vary...
#> [1] TDCM estimation complete.
#> [1] Use tdcm.summary() to display results.
results <- TDCM::tdcm.summary(model)
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
results$growth.effects
#>             T1[1] T2[1] Growth Odds Ratio Cohen`s h
#> Attribute 1 0.190 0.370  0.180       2.50      0.41
#> Attribute 2 0.317 0.491  0.174       2.08      0.36
#> Attribute 3 0.392 0.579  0.187       2.13      0.38
#> Attribute 4 0.242 0.693  0.451       7.07      0.94

#############################
## Example 3: T = 3, A = 2 ##
data <- data.tdcm03$data
q1 <- data.tdcm03$q.matrix.1
q2 <- data.tdcm03$q.matrix.2
q3 <- data.tdcm03$q.matrix.3
q <- data.tdcm03$q.matrix.stacked

#TDCM with anchor items constrained
m1 <- tdcm(data, q, num.time.points = 3, num.q.matrix = 3,
anchor = c(1,11,1,21,14,24), num.items = c(10,10,10))
#> [1] Preparing data for tdcm()...
#> [1] Estimating the TDCM in tdcm()...
#> [1] Depending on model complexity, estimation time may vary...
#> [1] TDCM estimation complete.
#> [1] Use tdcm.summary() to display results.

#TDCM without anchor items
m2 <- tdcm(data, q, num.time.points = 3, num.q.matrix = 3, num.items = c(10,10,10))
#> [1] Preparing data for tdcm()...
#> [1] Estimating the TDCM in tdcm()...
#> [1] Depending on model complexity, estimation time may vary...
#> [1] TDCM estimation complete.
#> [1] Use tdcm.summary() to display results.

#Compare models to assess measurement invariance
tdcm.compare(m1, m2)
#>   Model   loglike Deviance Npars      AIC      BIC Chisq df     p
#> 1    m1 -27900.69 55801.39    98 55997.39 56518.08 10.16  8 0.254
#> 2    m2 -27895.61 55791.23   106 56003.23 56566.43    NA NA    NA

#Summarize model 1
r1 = tdcm.summary(m1, transition.option = 3)
#> [1] Summarizing results...
#> [1] Routine finished. Check results.
r1$item.parameters
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
r1$growth
#>             T1[1] T2[1] T3[1]
#> Attribute 1 0.306 0.506 0.790
#> Attribute 2 0.311 0.581 0.705
r1$growth.effects
#> , , Time 1 to Time 2
#> 
#>             Growth Odds Ratio Cohen`s h
#> Attribute 1   0.20       2.32      0.41
#> Attribute 2   0.27       3.07      0.55
#> 
#> , , Time 2 to Time 3
#> 
#>             Growth Odds Ratio Cohen`s h
#> Attribute 1  0.284       3.67      0.61
#> Attribute 2  0.124       1.72      0.26
#> 

# }
```
