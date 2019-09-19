#' Generate data for structural equation models
#'
#' Generate data for structural equation models including up to 8 constructs. Generation
#' is based parameter values given in [lavaan model syntax](http://lavaan.ugent.be/tutorial/syntax1.html).
#' In addition to supplying numeric values, variable values for parameters are allowed.
#' To achieve this, the package makes use of [lavaan](http://lavaan.ugent.be/)'s
#' labeling capabilities.
#' Users may replace a given parameter in, i.e. the structural model
#' by a symbolic name and assign a vector of values to that name. These values will be used
#' to generate data for all possible combinations of these values with the remaining fixed parameters.
#'
#' @importFrom rlang .data
#' @keywords internal
"_PACKAGE"

# the rlang. data import is to quiet R CMD check complaining about undefined
# global variables
