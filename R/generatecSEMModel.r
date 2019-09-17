#' Generate cSEMModels
#'
#' Generate all possible [cSEMModel][cSEM::csem_model]s.
#'
#' @usage generatecSEMModel(.model, ...)
#'
#' @param .model A model in [lavaan model syntax][lavaan::model.syntax] possibly
#'   containing labels.
#' @param ... `"name" = values` pairs. `"name"` is a character value giving the
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
  ss  <- xx$structural2
  m   <- xx$measurement2
  e   <- xx$error_cor2
  phi <- xx$Phi

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
      ss[indices] <- unlist(path_coefs[x, ])
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
      m[indices] <- unlist(measurement_coefs[x, ])
      class(m) <- "numeric"
      m
    })
  } else {
    class(m) <- "numeric"
    ml <- list(m)
    ml
  }

  ## Measurement error correlation ---------------------------------------------
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
      e[indices] <- unlist(error_coefs[x, ])
      class(e) <- "numeric"
      e
    })
  } else {
    class(e) <- "numeric"
    el <- list(e)
    el
  }

  ## Structural error correlation ----------------------------------------------
  # Which elements in param_names match elements in e?
  param_names_phi <- intersect(param_names, c(phi))

  # Get the array indices for these matches
  indices <- which(matrix(phi %in% param_names_phi, dim(phi)), arr.ind = TRUE)

  #  Compute all combinations of the variables and create new data structural
  # models
  phi_coefs <- NULL
  if(nrow(indices) > 0) {
    phi_coefs <- expand.grid(params[param_names_phi])

    phil <- lapply(1:nrow(phi_coefs), function(x) {
      phi[indices] <- unlist(phi_coefs[x, ])
      class(phi) <- "numeric"
      phi
    })
  } else {
    class(phi) <- "numeric"
    phil <- list(phi)
    phil
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
    if(!is.null(phi_coefs)) {
      coef_df <- merge(coef_df, phi_coefs, sort = FALSE)
    }
  } else if(!is.null(measurement_coefs)) {
    coef_df <- measurement_coefs
    if(!is.null(error_coefs)) {
      coef_df <- merge(coef_df, error_coefs, sort = FALSE)
    }
    if(!is.null(phi_coefs)) {
      coef_df <- merge(coef_df, phi_coefs, sort = FALSE)
    }

  } else if(!is.null(error_coefs)) {
    coef_df <- error_coefs
    if(!is.null(phi_coefs)) {
      coef_df <- merge(coef_df, phi_coefs, sort = FALSE)
    }
  } else if(!is.null(phi_coefs)) {
    coef_df <- phi_coefs
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
          lapply(phil, function(phi) {
            list(
              "structural"  = xx$structural,
              "measurement" = xx$measurement,
              "error_cor"   = xx$error_cor,
              "construct_type"  = xx$construct_type,
              "construct_order" = xx$construct_order,
              "model_type"      = xx$model_type,
              "structural2"  = s,
              "measurement2" = m,
              "error_cor2"   = e,
              "Phi"          = phi
            )
          })
        }), recursive = FALSE)
      }), recursive = FALSE)
    }), recursive = FALSE)

  # Return
  list("Models" = sme, "Coef_df" = coef_df)
}
