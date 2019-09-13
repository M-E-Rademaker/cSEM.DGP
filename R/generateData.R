#' Generate data from a lavaan model syntax
#'
#' Generates data based on the parameters of a structural equation model
#' in [lavaan model syntax][lavaan::model.syntax].
#'
#' Generate data for structural equation models including up to 8 constructs. Generation
#' is based on parameter values given in [lavaan model syntax](http://lavaan.ugent.be/tutorial/syntax1.html).
#'
#' For the structural model, values are path coeffcients. For the measurement
#' model values in each equation are taken to be loadings, if the concept is modeled as a
#' common factor (`=~`). If the concept is modeled as a composite (`<~`) values are
#' interpreted as weights!
#' In this case the correlation between indicators needs to
#' be set as well. See the example below.
#'
#' In addition to supplying numeric values, variable values for parameters are allowed.
#' To achieve this, the package makes use of [lavaan](http://lavaan.ugent.be/)'s
#' labeling capabilities. Users may replace a given parameter in, i.e. the structural model
#' by a symbolic name and assign a vector of values to that name. These values will be used
#' to generate data for all possible combinations of these values with the remaining fixed parameters.
#'
#' If `.return_type` is `"data.frame"` or `"matrix"` normally distributed data
#' is generated based on the indicator correlation matrix which would be
#' returned if `.return_type = "cor"`.
#'
#' @usage generateData(
#'  .model                    = NULL,
#'  .empirical                = FALSE,
#'  .handle_negative_definite = c("stop", "ignore"),
#'  .return_type              = c("data.frame", "matrix", "cor"),
#'  .N                        = 200,
#'  ...
#'  )
#'
#' @param .model A model in [lavaan model syntax][lavaan::model.syntax].
#' @param .empirical Logical. If TRUE, mu and Sigma of the normal distribution
#'   specify the empirical not the population mean and covariance matrix.
#' @param .handle_negative_definite Character string. How should negative definite
#'   indicator correlation matrices be handled? One of `"stop"` or `"ignore"` in which case
#'   an `NA` is produced. Defaults to `"stop"`.
#' @param .N Integer. The number of observations to generate. Ignored if
#'   `return.type = "cor"`. Defaults to `200`.
#' @param .return_type Character string. One of `"data.frame"`, `matrix` or `"cor"`
#'   in which case the indicator correlation matrix is returned. Defaults to `".data.frame"`.
#' @param ... `"name" = values` pairs. `"name"` is a character value giving the
#'   label used for the parameter of interest. `values` is a numeric vector of
#'   values to use for the paramter given by `"name"`.
#'
#' @return The generated data. Either as a data.frame (`return_type = "data.frame"`),
#'   a numeric matrix (`return.type = "matrix"`),
#'   or a correlation matrix (`return.type = "cor"`).
#'
#' @export
#'
#' @examples
#' ## Model with only common factors
#' model <- "
#' # Structural model
#' eta2 ~ gamma*eta1
#' eta3 ~ 0.4*eta1 + 0.35*eta2
#'
#' # Measurement model
#' eta1 =~ lambda*y11 + 0.9*y12 + 0.8*y13
#' eta2 =~ 0.7*y21 + 0.7*y22 + 0.9*y23
#' eta3 =~ 0.9*y31 + 0.8*y32 + 0.7*y33
#' "
#'
#' dat <- generateData(model,
#'                        "gamma" = c(0.3, 0.6),
#'                        "lambda" = c(0.8, 0.85, 0.9), .N = 10)
#' dat[1:3]
#'
#' ## If the model contains composites, within-block indicator correlation
#' ## needs to be set as well.
#' ## Here too, variable values may be used
#'
#' model <- "
#' # Structural model
#' eta2 ~ 0.2*eta1
#' eta3 ~ 0.4*eta1 + 0.35*eta2
#'
#' # Measurement model
#' eta1 <~ 0.7*y11 + 0.9*y12 + 0.8*y13
#' eta2 =~ 0.7*y21 + 0.7*y22 + 0.9*y23
#' eta3 =~ 0.9*y31 + 0.8*y32 + 0.7*y33
#'
#' # Within block indicator correlation
#' y11 ~~ 0.2*y12
#' y11 ~~ 0.3*y13
#' y12 ~~ epsilon*y13
#' "
#'
#' dat <- generateData(model, "epsilon" = c(0.1, 0.2, 0.3), .return_type = "cor")
#' dat
generateData <- function(
  .model                    = NULL,
  .empirical                = FALSE,
  .handle_negative_definite = c("stop", "ignore"),
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
  sigma_list <- lapply(model_list, generateSigma,
                       .handle_negative_definite = handle_negative_definite)

  ## Get relevant objects
  if(return_type == "cor") {
    if(length(sigma_list) == 1) {
      sigma_list[[1]]
    } else {
      sigma_list
    }
  } else {
    data_list <- lapply(sigma_list, function(x) {
      out <- MASS::mvrnorm(.N, mu = rep(0, nrow(x)), Sigma = x, empirical = .empirical)
      if(return_type == "data.frame") {
        out <- as.data.frame(out)
      }
      out
    })

    if(length(data_list) == 1) {
      data_list[[1]]
    } else {
      data_list
    }
  }
}
