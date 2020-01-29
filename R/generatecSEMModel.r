#' Internal: generate cSEMModels
#'
#' Generate all possible [cSEMModel][cSEM::csem_model]s.
#'
#' @usage generatecSEMModel(.model, ...)
#'
#' @param .model A model in [lavaan model syntax][lavaan::model.syntax] possibly
#'   containing labels.
#' @param ... `"name" = values` pairs. `"name"` is a character string giving the
#'   label used for the parameter of interest. `values` is a numeric vector of
#'   values to use for the paramter given by `"name"`.
#
#' @return A list of [cSEMModel][cSEM::csem_model]s
#'
#' @keywords internal

generatecSEMModel <- function(
  .model,
  ...
  ) {

  ## Collect dotdotdot (...) arguments
  params <- as.list(list(...))
  param_names <- names(params)

  xx  <- cSEM::parseModel(.model)

  if(is.null(xx$measurement2)) {
    stop("No population values given. Please specify all population values", call. = FALSE)
  }

  if(xx$model_type == "Nonlinear") {
    stop("Currently, models containing nonlinear terms are not supported.", call. = FALSE)
  }

  ss  <- xx$structural2
  m   <- xx$measurement2
  e   <- xx$indicator_cor
  cc  <- xx$construct_cor

  ## Only the Phi matrix is required (correlation matrix between exogenous constructs)
  Phi <- matrix(0,
                nrow = length(xx$cons_exo),
                ncol = length(xx$cons_exo),
                dimnames = list(xx$cons_exo, xx$cons_exo)
  )

  # Get row and column names for constructs
  row_index <- intersect(rownames(Phi), rownames(cc))
  col_index <- intersect(colnames(Phi), colnames(cc))

  Phi[row_index, col_index] <- cc[row_index, col_index, drop = FALSE]

  # Set diagonal elements to 1
  diag(Phi) <- 1

  ## Structural model ----------------------------------------------------------
  # Which elements in param_names match elements in ss?
  param_names_path <- intersect(param_names, c(ss))

  # Get the array indices for these matches
  indices <- which(matrix(ss %in% param_names_path, dim(ss)), arr.ind = TRUE)

  #  Compute all combinations of the variables and create new data structural
  # models
  path_coefs <- NULL
  if(nrow(indices) > 0) {
    path_coefs <- expand.grid(params[param_names_path])

    sl <- lapply(1:nrow(path_coefs), function(x) {
      # Its crucial to order path_coef (using ss[indices])!!
      ss[indices] <- unlist(path_coefs[x, ss[indices]])
      class(ss) <- "numeric"
      ss
    })
  } else {
    class(ss) <- "numeric"
    sl <- list(ss)
    sl
  }

  ## Measurement/composite model -----------------------------------------------
  # Which elements in param_names match elements in m?
  param_names_measurement <- intersect(param_names, c(m))

  # Get the array indices for these matches
  indices <- which(matrix(m %in% param_names_measurement, dim(m)), arr.ind = TRUE)

  #  Compute all combinations of the variables and create new data structural
  # models
  measurement_coefs <- NULL
  if(nrow(indices) > 0) {
    measurement_coefs <- expand.grid(params[param_names_measurement])

    ml <- lapply(1:nrow(measurement_coefs), function(x) {
      m[indices] <- unlist(measurement_coefs[x, m[indices]])
      class(m) <- "numeric"
      m
    })
  } else {
    class(m) <- "numeric"
    ml <- list(m)
    ml
  }

  ## Indicator correlation (if composite)/ Measurement error correlation
  ## (if common factor) ---------------------------------------------
  # Which elements in param_names match elements in e?
  param_names_error <- intersect(param_names, c(e))

  # Get the array indices for these matches
  indices <- which(matrix(e %in% param_names_error, dim(e)), arr.ind = TRUE)

  #  Compute all combinations of the variables and create new data structural
  # models
  error_coefs <- NULL
  if(nrow(indices) > 0) {
    error_coefs <- expand.grid(params[param_names_error])

    el <- lapply(1:nrow(error_coefs), function(x) {
      e[indices] <- unlist(error_coefs[x, e[indices]])
      class(e) <- "numeric"
      e
    })
  } else {
    class(e) <- "numeric"
    el <- list(e)
    el
  }

  ## Structural error correlation ----------------------------------------------
  # Which elements in param_names match elements in Phi?
  param_names_Phi <- intersect(param_names, c(Phi))

  # Get the array indices for these matches
  indices <- which(matrix(Phi %in% param_names_Phi, dim(Phi)), arr.ind = TRUE)

  # Compute all combinations of the variables and create new data structural
  # models
  Phi_coefs <- NULL
  if(nrow(indices) > 0) {
    Phi_coefs <- expand.grid(params[param_names_Phi])

    Phil <- lapply(1:nrow(Phi_coefs), function(x) {
      Phi[indices] <- unlist(Phi_coefs[x, Phi[indices]])
      class(Phi) <- "numeric"
      Phi
    })
  } else {
    class(Phi) <- "numeric"
    Phil <- list(Phi)
    Phil
  }

  ## Merge
  coef_df <- NULL
  if(!is.null(path_coefs)) {
    coef_df <- path_coefs
    if(!is.null(measurement_coefs)) {
      coef_df <- merge(coef_df, measurement_coefs, sort = FALSE)
    }
    if(!is.null(error_coefs)) {
      coef_df <- merge(coef_df, error_coefs, sort = FALSE)
    }
    if(!is.null(Phi_coefs)) {
      coef_df <- merge(coef_df, Phi_coefs, sort = FALSE)
    }
  } else if(!is.null(measurement_coefs)) {
    coef_df <- measurement_coefs
    if(!is.null(error_coefs)) {
      coef_df <- merge(coef_df, error_coefs, sort = FALSE)
    }
    if(!is.null(Phi_coefs)) {
      coef_df <- merge(coef_df, Phi_coefs, sort = FALSE)
    }

  } else if(!is.null(error_coefs)) {
    coef_df <- error_coefs
    if(!is.null(Phi_coefs)) {
      coef_df <- merge(coef_df, Phi_coefs, sort = FALSE)
    }
  } else if(!is.null(Phi_coefs)) {
    coef_df <- Phi_coefs
  }

  # Add rownames as column
  if(!is.null(coef_df)) {
    coef_df <- data.frame("Id" = 1:nrow(coef_df), coef_df)
  }

  ## Combine Structural, measurement/composite and error correlation matrices
  ## and add the rest of the information
  sme <-
    unlist(lapply(sl, function(s) {
      unlist(lapply(ml, function(m) {
        unlist(lapply(el, function(e) {
          lapply(Phil, function(Phi) {
            l <- list(
              "structural"  = xx$structural,
              "measurement" = xx$measurement,
              "error_cor"   = xx$error_cor,
              "construct_type"  = xx$construct_type,
              "construct_order" = xx$construct_order,
              "model_type"      = xx$model_type,
              "cons_exo"        = xx$cons_exo,
              "cons_endo"       = xx$cons_endo,
              "vars_2nd"        = xx$vars_2nd,
              "vars_attached_to_2nd" = xx$vars_attached_to_2nd,
              "vars_not_attached_to_2nd" = xx$vars_not_attached_to_2nd,
              "structural2"   = s,
              "measurement2"  = m,
              "indicator_cor" = e,
              "phi"           = Phi
            )
            class(l) <- "cSEMModel"
            l
          })
        }), recursive = FALSE)
      }), recursive = FALSE)
    }), recursive = FALSE)

  # Return
  list("Models" = sme, "Coef_df" = coef_df)
}
