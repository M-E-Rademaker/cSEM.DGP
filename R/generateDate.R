#' Generate data from a lavaan model syntax
#'
#' Generates data based on the parameters of a structural equation model
#' in [lavaan model syntax][lavaan::model.syntax].
#'
#' @usage generateData(
#'  .model                    = NULL,
#'  .handle_negative_definite = c("stop", "ignore"),
#'  .N                        = 200,
#'  .return_type              = c("data.frame", "matrix", "cor"),
#'  ...
#'  )
#'
#' @param .model A model in [lavaan model syntax][lavaan::model.syntax].
#' @param .handle_negative_definite Character string. How should negative definite
#'   indicator correlation matrices be handled? One of `"stop"` or `"ignore"` in which case
#'   an `NA` is produced. Defaults to `"stop"`.
#' @param .N Integer. The number of observations to generate. Ignored if
#'   `return.type = "cor"`. Defaults to `200`.
#' @param .return_type Character string. One of `"data.frame"`, `matrix` or `"cor"`
#'   in which case the indicator correlation matrix is returned. Defaults to `".data.frame"`.
#'
#' @return The generated data. Either as a data.frame (`return_type = "data.frame"`),
#'   a numeric matrix (`return.type = "matrix"`),
#'   or a correlation matrix (`return.type = "cor"`).
#'
#' @export
#'
#' @examples
#' model <- "
#' # Structural model
#' eta2 ~ gamma1*eta1
#' eta3 ~ 0.4*eta1 + 0.35*eta2
#'
#' # Measurement model
#' eta1 =~ lambda1*y11 + 0.9*y12 + 0.8*y13
#' eta2 =~ 0.7*y21 + 0.7*y22 + 0.9*y23
#' eta3 =~ 0.9*y31 + 0.8*y32 + 0.7*y33
#' "
#'
#' Models <- generatecSEMModel(model,
#'                            "gamma1" = c(0.3, 0.6),
#'                            "lambda1" = c(0.8, 0.85, 0.9))
#' Models
#'

generateData <- function(
  .model                    = NULL,
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
  if(.return_type == "cor") {
    if(length(sigma_list) == 1) {
      sigma_list[[1]]
    } else {
      sigma_list
    }
  } else {
    stop("to do")
  }
}
