
<!-- README.md is generated from README.Rmd. Please edit that file -->

# cSEM.DGP: Generate data for structural equation models

[![CRAN
status](https://www.r-pkg.org/badges/version/cSEM.DGP)](https://cran.r-project.org/package=cSEM.DGP)
[![Build
Status](https://travis-ci.com/M-E-Rademaker/cSEM.DGP.svg?branch=master)](https://travis-ci.com/M-E-Rademaker/cSEM.DGP)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/M-E-Rademaker/cSEM.DGP?branch=master&svg=true)](https://ci.appveyor.com/project/M-E-Rademaker/csem-dgp)

## Installation:

``` r
# install.packages("devtools")
devtools::install_github("M-E-Rademaker/cSEM.DGP")
```

Note: requires a development version of
[cSEM](https://github.com/M-E-Rademaker/cSEM) that includes commits done
before 21.11.2019.

If you have an older version. Run:

``` r
# install.packages("devtools")
devtools::install_github("M-E-Rademaker/cSEM")
```

## Purpose

Generate data for structural equation models including up to eight
constructs. Data generation is based on parameter values given in
[lavaan model syntax](http://lavaan.ugent.be/tutorial/syntax1.html).

In addition to supplying numeric values, variable values (symbolic
names) for parameters are allowed. To achieve this, the package makes
use of [lavaan](http://lavaan.ugent.be/)’s labeling capabilities. Users
may replace a given parameter in, i.e. the structural model by a
symbolic name and assign a vector of values to that name. These values
will be used to generate data for all possible combinations of these
values with the remaining fixed parameters.

## Examples

### Without variable parameters

Simply write your model in [lavaan model
syntax](http://lavaan.ugent.be/tutorial/syntax1.html). Add a fixed
numeric value for each parameter. Note, currently **you must either set
all parameters or none**. The type of output can be chosen: a data.frame
(`return_type = "data.frame"`, the default), a numeric matrix
(`return.type = "matrix"`), or a correlation matrix (`return.type =
"cor"`).

``` r
require(cSEM.DGP)

model <- "
# Structural model
eta3 ~ 0.6*eta1 + 0.4*eta2
eta4 ~ 0.4*eta1 + 0.35*eta3

# Measurement model
eta1 =~ 0.8*y11 + 0.9*y12 + 0.8*y13
eta2 =~ 0.7*y21 + 0.7*y22 
eta3 =~ 0.9*y31 + 0.8*y32
eta4 =~ 0.9*y41 + 0.8*y42

# Measurment error correlation
y11 ~~ 0.2*y12

# Correlation between exogenous constructs
eta1 ~~ 0.5*eta2
"

dat <- generateData(model, .return_type = "cor")
dat
```

    ##        y11    y12    y13     y21     y22    y31    y32     y41    y42
    ## y11 1.0000 0.9200 0.6400 0.28000 0.28000 0.5760 0.5120 0.48960 0.4352
    ## y12 0.9200 1.0000 0.7200 0.31500 0.31500 0.6480 0.5760 0.55080 0.4896
    ## y13 0.6400 0.7200 1.0000 0.28000 0.28000 0.5760 0.5120 0.48960 0.4352
    ## y21 0.2800 0.3150 0.2800 1.00000 0.49000 0.4410 0.3920 0.28035 0.2492
    ## y22 0.2800 0.3150 0.2800 0.49000 1.00000 0.4410 0.3920 0.28035 0.2492
    ## y31 0.5760 0.6480 0.5760 0.44100 0.44100 1.0000 0.7200 0.54270 0.4824
    ## y32 0.5120 0.5760 0.5120 0.39200 0.39200 0.7200 1.0000 0.48240 0.4288
    ## y41 0.4896 0.5508 0.4896 0.28035 0.28035 0.5427 0.4824 1.00000 0.7200
    ## y42 0.4352 0.4896 0.4352 0.24920 0.24920 0.4824 0.4288 0.72000 1.0000

### Including variable parameters

To include variable parameters:

1.  Replace a numeric value by a symbolic name (i.e., a character
    string) of your choice.
2.  Supply a named vector of values to `generateData()` where the name
    corresponds to the chosen character string.

Now, data for all possible combinations of these values with the
remaining fixed parameters will be generated.

``` r
# We want to vary the path coefficient between eta2 and eta1 and 
# the first loading of eta1.
model <- "
# Structural model
eta2 ~ gamma1*eta1
eta3 ~ gamma2*eta1 + 0.35*eta2

# Measurement model
eta1 =~ lambda*y11 + 0.9*y12 + 0.8*y13
eta2 =~ 0.7*y21 + 0.7*y22 + 0.9*y23
eta3 =~ 0.9*y31 + 0.8*y32 + 0.7*y33
"

dat <- generateData(model, .return_type = "cor", 
                    lambda = c(0.8, 0.9),
                    gamma1 = c(0.2, 0.3),
                    gamma2 = c(0, 0.2, 0.3),
                    .handle_negative_definite = "drop"
                    )
dat
```

    ## # A tibble: 12 x 5
    ##       Id gamma1 gamma2 lambda dgp              
    ##    <int>  <dbl>  <dbl>  <dbl> <list>           
    ##  1     1    0.2    0      0.8 <dbl[,9] [9 x 9]>
    ##  2     2    0.3    0      0.8 <dbl[,9] [9 x 9]>
    ##  3     3    0.2    0.2    0.8 <dbl[,9] [9 x 9]>
    ##  4     4    0.3    0.2    0.8 <dbl[,9] [9 x 9]>
    ##  5     5    0.2    0.3    0.8 <dbl[,9] [9 x 9]>
    ##  6     6    0.3    0.3    0.8 <dbl[,9] [9 x 9]>
    ##  7     7    0.2    0      0.9 <dbl[,9] [9 x 9]>
    ##  8     8    0.3    0      0.9 <dbl[,9] [9 x 9]>
    ##  9     9    0.2    0.2    0.9 <dbl[,9] [9 x 9]>
    ## 10    10    0.3    0.2    0.9 <dbl[,9] [9 x 9]>
    ## 11    11    0.2    0.3    0.9 <dbl[,9] [9 x 9]>
    ## 12    12    0.3    0.3    0.9 <dbl[,9] [9 x 9]>

The return type in this case is a [nested
tibble](https://tidyr.tidyverse.org/articles/nest.html). To access
e.g. the first data set type:

``` r
dat$dgp[[1]]
```

    ##        y11    y12    y13    y21    y22    y23    y31    y32    y33
    ## y11 1.0000 0.7200 0.6400 0.1120 0.1120 0.1440 0.0504 0.0448 0.0392
    ## y12 0.7200 1.0000 0.7200 0.1260 0.1260 0.1620 0.0567 0.0504 0.0441
    ## y13 0.6400 0.7200 1.0000 0.1120 0.1120 0.1440 0.0504 0.0448 0.0392
    ## y21 0.1120 0.1260 0.1120 1.0000 0.4900 0.6300 0.2205 0.1960 0.1715
    ## y22 0.1120 0.1260 0.1120 0.4900 1.0000 0.6300 0.2205 0.1960 0.1715
    ## y23 0.1440 0.1620 0.1440 0.6300 0.6300 1.0000 0.2835 0.2520 0.2205
    ## y31 0.0504 0.0567 0.0504 0.2205 0.2205 0.2835 1.0000 0.7200 0.6300
    ## y32 0.0448 0.0504 0.0448 0.1960 0.1960 0.2520 0.7200 1.0000 0.5600
    ## y33 0.0392 0.0441 0.0392 0.1715 0.1715 0.2205 0.6300 0.5600 1.0000
