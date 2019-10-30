
<!-- README.md is generated from README.Rmd. Please edit that file -->

# cSEM.DGP: Generate data for structural equation models

[![CRAN
status](https://www.r-pkg.org/badges/version/cSEM.DGP)](https://cran.r-project.org/package=cSEM.DGP)
[![Build
Status](https://travis-ci.com/M-E-Rademaker/cSEM.DGP.svg?branch=master)](https://travis-ci.com/M-E-Rademaker/cSEM.DGP)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/M-E-Rademaker/cSEM.DGP?branch=master&svg=true)](https://ci.appveyor.com/project/M-E-Rademaker/cSEM.DGP)

Please report bugs to [the package
developer](mailto:manuel.rademaker@uni-wuerzburg.de).

## Installation:

``` r
# install.packages("devtools")
devtools::install_github("M-E-Rademaker/cSEM.DGP")
```

Note: requires a development version of
[cSEM](https://github.com/M-E-Rademaker/cSEM) that includes commits done
before 12.09.2019.

If you have an older version. Run:

``` r
# install.packages("devtools")
devtools::install_github("M-E-Rademaker/cSEM")
```

## Purpose

Generate data for structural equation models including up to 8
constructs. Generation is based on parameter values given in [lavaan
model syntax](http://lavaan.ugent.be/tutorial/syntax1.html).

In addition to supplying numeric values, variable values for parameters
are allowed. To achieve this, the package makes use of
[lavaan](http://lavaan.ugent.be/)’s labeling capabilities. Users may
replace a given parameter in, i.e. the structural model by a symbolic
name and assign a vector of values to that name. These values will be
used to generate data for all possible combinations of these values with
the remaining fixed parameters.

## Examples

### Without variable paramters

Simply write your model as usual in lavaan model syntax. Add a fixed
numeric value for each parameter. Note, currently you must either set
all paramters or none. The type of output can be choosen. Either a
data.frame (`return_type = "data.frame"`, the default), a numeric matrix
(`return.type = "matrix"`), or a correlation matrix (`return.type =
"cor"`).

``` r
require(cSEM.DGP)

model <- "
# Structural model
eta2 ~ 0.6*eta1
eta3 ~ 0.4*eta1 + 0.35*eta2

# Measurement model
eta1 =~ 0.8*y11 + 0.9*y12 + 0.8*y13
eta2 =~ 0.7*y21 + 0.7*y22 + 0.9*y23
eta3 =~ 0.9*y31 + 0.8*y32 + 0.7*y33
"

dat <- generateData(model, .return_type = "cor")
dat
```

    ##        y11    y12    y13    y21    y22    y23    y31    y32    y33
    ## y11 1.0000 0.7200 0.6400 0.3360 0.3360 0.4320 0.4392 0.3904 0.3416
    ## y12 0.7200 1.0000 0.7200 0.3780 0.3780 0.4860 0.4941 0.4392 0.3843
    ## y13 0.6400 0.7200 1.0000 0.3360 0.3360 0.4320 0.4392 0.3904 0.3416
    ## y21 0.3360 0.3780 0.3360 1.0000 0.4900 0.6300 0.3717 0.3304 0.2891
    ## y22 0.3360 0.3780 0.3360 0.4900 1.0000 0.6300 0.3717 0.3304 0.2891
    ## y23 0.4320 0.4860 0.4320 0.6300 0.6300 1.0000 0.4779 0.4248 0.3717
    ## y31 0.4392 0.4941 0.4392 0.3717 0.3717 0.4779 1.0000 0.7200 0.6300
    ## y32 0.3904 0.4392 0.3904 0.3304 0.3304 0.4248 0.7200 1.0000 0.5600
    ## y33 0.3416 0.3843 0.3416 0.2891 0.2891 0.3717 0.6300 0.5600 1.0000

### Including variable paramters

To include variable paramters:

1.  Replace a numeric value by a character string of your choice.
2.  Supply a named vector of values to `generateData()` where the name
    corresponds to the character string of your choice.

Now, data for all possible combinations of these values with the
remaining fixed parameters will be generated

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
