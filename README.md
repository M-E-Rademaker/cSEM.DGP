
<!-- README.md is generated from README.Rmd. Please edit that file -->

# cSEM.DGP: Generate indicator correlation matrices

<!-- [![CRAN status](https://www.r-pkg.org/badges/version/cSEM.DGP)](https://cran.r-project.org/package=cSEM.DGP) -->

<!-- [![Build Status](https://travis-ci.com/M-E-Rademaker/cSEM.DGP.svg?branch=master)](https://travis-ci.com/M-E-Rademaker/cSEM.DGP) -->

WARNING: THIS IS WORK IN PROGRESS. Use the package with caution and
please report bugs to [the package
developers](mailto:manuel.rademaker@uni-wuerzburg.de;f.schuberth@utwente.nl).

## Purpose

Generate population indicator correlation matrices for a given
structural equation model based on the parameter given by the user. The
package makes use of lavaan’s labeling capabilities by allowing variable
parameters. User may replace a given parameter in i.e. the structural
model by a symbolic name and assign values to that name. These values
will be used to generate Sigma matrices for all possible combinations of
these values with the remaining fixed parameters.
