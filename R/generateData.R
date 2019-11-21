#' Generate data from a lavaan model syntax
#'
#' Generate data based on the parameters of a structural equation model
#' in [lavaan model syntax][lavaan::model.syntax].
#'
#' Generate data for structural equation models including up to 8 constructs. To be precise,
#' we support a maximum of 5 exogenous constructs. Depending on the
#' number of exogenous constructs the following number of endogenous constructs
#' is allowed:
#' \enumerate{
#'  \item{If there is 1 exogenous construct  : a maximum of 7 endogenous constructs is allowed}
#'  \item{If there are 2 exogenous constructs: a maximum of 6 endogenous constructs is allowed}
#'  \item{If there are 3 exogenous constructs: a maximum of 5 endogenous constructs is allowed}
#'  \item{If there are 4 exogenous constructs: a maximum of 4 endogenous constructs is allowed}
#'  \item{If there are 5 exogenous constructs: a maximum of 4 endogenous constructs is allowed}
#' }
#' The reason for the limitation is that data is generated such that
#' the model-implied variances of the constructs are always unity. Since
#' the model-implied construct covariance matrix is a complex function
#' of the structural residual variances which are in turn a complex function
#' of the path coefficients. Since for a given number of constructs the number of
#' possible model specifications grows rapidly, we solved the variance equations
#' symbolically as a function of the path coefficients in Mathematica.
#' With more than 8 constructs the size of these symbolic representation becomes
#' computationally infeasible.
#'
#' Generation is based on parameter values given in [lavaan model syntax](http://lavaan.ugent.be/tutorial/syntax1.html).
#' Currently, linear models and models containing second order constructs are
#' supported. Supplying a model containing nonlinear terms causes an error.
#'
#' For the structural model equations (`~`) values are interpreted as path coefficients.
#' For measurement model equations values are taken to be loadings if the
#' concept is modeled as a common factor (`=~`). If the concept is modeled as
#' a composite (`<~`) values are interpreted as (unscaled) weights!
#' In the latter case, indicators are allowed to be arbitrarily correlated. Hence,
#' the correlation between indicators needs to be set as well. Indicator correlations
#' measurement error correlations, and correlations between exogenous constructs
#' are set using the (`~~`) operator. Note that when writing, for instance, `x1 ~~ 0.2*x2`
#' (where `x1` and `x2` are indicators of some construct `eta1`), the interpretation
#' depends on whether `eta1` is modeled as a composite or a common factor.
#' In the former case `x1 ~~ 0.2*x2` is a correlation between indicators, in the
#' latter case it is interpreted as a measurement error correlation.
#'
#' In addition to supplying numeric values, variable values for parameters are allowed.
#' To achieve this, the package makes use of [lavaan](http://lavaan.ugent.be/)'s
#' labeling capabilities. Users may replace a given parameter in, i.e. the structural model
#' by a symbolic name and assign a vector of values to that name by passing a
#' `"name" = vector_of_values` argument to [generateData()]. These values will be used
#' to generate data for all possible combinations of these values with the
#' remaining fixed parameters.
#'
#' If `.return_type` is `"data.frame"` or `"matrix"` normally distributed data
#' with zero mean and variance-covariance matrix equal to the indicator correlation
#' matrix which would be returned if `.return_type = "cor"` (i.e., the population
#' indicator correlation matrix) is generated.
#'
#' @usage generateData(
#'  .model                    = NULL,
#'  .empirical                = FALSE,
#'  .handle_negative_definite = c("stop", "drop", "set_NA"),
#'  .return_type              = c("data.frame", "matrix", "cor"),
#'  .N                        = 200,
#'  ...
#'  )
#'
#' @param .model A model in [lavaan model syntax][lavaan::model.syntax].
#' @param .empirical Logical. If `TRUE`, mu and Sigma of the normal distribution
#'   specify the empirical not the population mean and covariance matrix. Ignored if
#'   `return.type = "cor"`. Defaults to `FALSE`.
#' @param .handle_negative_definite Character string. How should negative definite
#'   indicator correlation matrices be handled? One of `"stop"`, `"drop"` or `"set_NA"`
#'   in which case an `NA` is produced. Defaults to `"stop"`.
#' @param .N Integer. The number of observations to generate. Ignored if
#'   `return.type = "cor"`. Defaults to `200`.
#' @param .return_type Character string. One of `"data.frame"`, `"matrix"` or `"cor"`
#'   in which case the indicator correlation matrix is returned. Defaults to `"data.frame"`.
#' @param ... `"name" = vector_of_values` pairs. `"name"` is a character string giving the
#'   label used for the parameter of interest. `vector_of_values` is a numeric vector of
#'   values to use for the paramter given by `"name"`.
#'
#' @return The generated data. Either as a data.frame (`return_type = "data.frame"`),
#'   a numeric matrix (`return.type = "matrix"`),
#'   or a correlation matrix (`return.type = "cor"`). If variable parameters
#'   have been set a nested tibble is returned.
#'
#' @export
#'
#' @example inst/examples/example_generateData.R

generateData <- function(
  .model                    = NULL,
  .empirical                = FALSE,
  .handle_negative_definite = c("stop", "drop", "set_NA"),
  .return_type              = c("data.frame", "matrix", "cor"),
  .N                        = 200,
  ...
  ) {
  ## Match arguments
  handle_negative_definite <- match.arg(.handle_negative_definite)
  return_type              <- match.arg(.return_type)

  ## Get the models
  model_list <- generatecSEMModel(.model, ...)

  ## Compute Sigma matrices
  sigma_list <- lapply(model_list$Models, generateSigma,
                       .handle_negative_definite = handle_negative_definite)

  ## Create tibble
  if(!is.null(model_list$Coef_df)) {
    info_frame <- merge(model_list$Coef_df, tibble::enframe(sigma_list, name = "Id", value = "dgp"), by = "Id")
    info_frame <- tibble::as_tibble(info_frame)
    ## Drop NA's if requested
    if(handle_negative_definite == "drop") {
      info_frame <- info_frame[!sapply(info_frame$dgp, anyNA), ]
    }
  } else {
    info_frame <- sigma_list[[1]]
  }

  # Get relevant objects
  if(return_type == "cor") {
    info_frame
  } else {

    if(!is.null(model_list$Coef_df)) {
      info_frame <- dplyr::mutate(info_frame, "dgp" = lapply(.data$dgp, function(x) {
        if(anyNA(x)) {
          NA
        } else {
          out <- MASS::mvrnorm(.N, mu = rep(0, nrow(x)), Sigma = x, empirical = .empirical)
          if(return_type == "data.frame") {
            out <- as.data.frame(out)
          }
          out
        }
      }))
    } else {
      if(anyNA(info_frame)) {
        NA
      } else {
        info_frame <- MASS::mvrnorm(.N, mu = rep(0, nrow(info_frame)), Sigma = info_frame, empirical = .empirical)
        if(return_type == "data.frame") {
          info_frame <- as.data.frame(info_frame)
        }
      }
    }

    # Return
    info_frame
  }
}
