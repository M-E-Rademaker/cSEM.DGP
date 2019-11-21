#' Generate the indicator correlation matrix
#'
#' Generates the indicator correlation matrix based on the parameters of a
#' structural equation model in [lavaan model syntax][lavaan::model.syntax].
#'
#' @usage generateSigma(
#'  .model                    = NULL,
#'  .handle_negative_definite = c("stop", "drop", "set_NA")
#'  )
#'
#' @param .model A model in [lavaan model syntax][lavaan::model.syntax] or a
#'   [cSEMModel][cSEM::csem_model].
#' @param .handle_negative_definite Character string. How should negative definite
#'   indicator correlation matrices be handled? One of `"stop"`, `"drop"` or `"set_NA"`
#'   in which case an `NA` is produced. Defaults to `"stop"`.
#
#' @return A K by K matrix of indicator correlations. K is the number of indicators
#'
#' @keywords internal

generateSigma <- function(
  .model                    = NULL,
  .handle_negative_definite = c("stop", "drop", "set_NA")
) {
  ## Match arguments
  .handle_negative_definite <- match.arg(.handle_negative_definite)

  ## Get relevant objects
  model       <- cSEM::parseModel(.model)
  con_type    <- model$construct_type
  vars_exo    <- model$cons_exo
  vars_endo   <- model$cons_endo
  path_matrix <- model$structural2
  # Loadings
  Lambda      <- t(model$measurement)
  # Measurement errors
  Theta       <- model$error_cor
  # Path from exogenous to endogenous
  Gamma       <- path_matrix[vars_endo, vars_exo, drop = FALSE]
  # Path from endogenous to endogenous
  B           <- path_matrix[vars_endo, vars_endo, drop = FALSE]
  # Correlation between exogenous (Phi)
  Phi         <- model$phi

  ### Checks and errors --------------------------------------------------------
  #   A maximum of 5 exogenous constructs is allowed:
  #     1. If there is 1 exogenous construct  : a maximum of 7 endogenous constructs is allowed
  #     2. If there are 2 exogenous constructs: a maximum of 6 endogenous constructs is allowed
  #     3. If there are 3 exogenous constructs: a maximum of 5 endogenous constructs is allowed
  #     4. If there are 4 exogenous constructs: a maximum of 4 endogenous constructs is allowed
  #     5. If there are 5 exogenous constructs: a maximum of 4 endogenous constructs is allowed

  if(length(vars_exo) > 5) {
    stop("Models containing more than 5 exogenous constructs are not supported.",
         call. = FALSE)
  }
  if(length(vars_endo) > 7) {
    stop("Models containing more than 7 endogenous constructs are not supported.",
         call. = FALSE)
  }
  if(length(vars_exo) > 2 && length(vars_endo) > 6) {
    stop("Models containing more than 2 exogenous AND more than 6 endogenous constructs are not supported.",
         call. = FALSE)
  }
  if(length(vars_exo) > 3 && length(vars_endo) > 5) {
    stop("Models containing more than 3 exogenous AND more than 5 endogenous constructs are not supported.",
         call. = FALSE)
  }

  ## Modify and fill Lambda and Theta ------------------------------------------
  for(j in colnames(Lambda)) {

    if(con_type[j] == "Composite") {
      indicators <- colnames(model$measurement2[j, model$measurement2[j, ] != 0, drop = FALSE])
      # If j is a composite, the values of measurement2 are interpreted as weights
      w_j  <- model$measurement2[j, indicators]

      # If weights are given, the within-block indicator correlation matrix
      # must be given as well. If j is a composite values in indicator_cor
      # are interpreted as indicator correlations
      Sigma_jj <- as.matrix(model$indicator_cor[indicators, indicators])
      diag(Sigma_jj) <- 1

      if(nrow(Sigma_jj) > 1 && sum(Sigma_jj[lower.tri(Sigma_jj)]) == 0) {
        stop("Indicator correlation matrix of indicators: ",
             paste0("`", indicators, "`", collapse = ","),
             " (construct `", j, "`) is zero.\n",
             "Please specify the correlation using e.g., `",
             indicators[1],  " ~~ 0.4*", indicators[2], "`", call. = FALSE)
      }

      # Scale weigths
      ws_j <- w_j / c(sqrt(w_j %*% Sigma_jj %*% w_j))

      # Compute lambda
      lambda_j <- c(Sigma_jj %*% ws_j)

      # Replace corresponding elements in Lambda
      Lambda[indicators, j] <- lambda_j

      # Compute theta
      Theta[indicators, indicators] <-  Sigma_jj - lambda_j %*% t(lambda_j)


    } else {# Common factor
      indicators <- colnames(model$measurement2[j, model$measurement2[j, ] != 0, drop = FALSE])

      # Replace corresponding elements in Lambda
      lambda_j <- model$measurement2[j, indicators]
      Lambda[indicators, j] <- lambda_j

      # Get measurement error correlation
      # If j is a common factor values in indicator_cor
      # are interpreted as measurement error correlations
      Theta[indicators, indicators] <- as.matrix(model$indicator_cor[indicators, indicators])

      # Compute Theta
      if(!is.null(dim(Theta[indicators, indicators]))) {
        diag(Theta[indicators, indicators]) <-  1 - diag(lambda_j %*% t(lambda_j))
      } else {
        Theta[indicators, indicators] <- 1 - diag(lambda_j %*% t(lambda_j))
      }
    }
  }

  if(any(model$construct_order == "Second order")) {
    ## Second order model
    vars_2nd <- model$vars_2nd
    vars_attached_to_2nd <- model$vars_attached_to_2nd
    vars_not_attached_to_2nd <- model$vars_not_attached_to_2nd

    ## Lambda matrix for then "inner" model
    Lambda_2nd <- Lambda[vars_attached_to_2nd, vars_2nd, drop = FALSE]
    I <- diag(length(vars_not_attached_to_2nd))
    Lambda_inner <- cbind(
      rbind(Lambda_2nd, matrix(0, nrow = nrow(I), ncol = ncol(Lambda_2nd))),
      rbind(matrix(0, nrow = nrow(Lambda_2nd), ncol = ncol(I)), I)
    )
    rownames(Lambda_inner) <- c(vars_attached_to_2nd, vars_not_attached_to_2nd)
    colnames(Lambda_inner) <- c(vars_2nd, vars_not_attached_to_2nd)

    ## Theta matrix for the "inner" model
    Theta_2nd <- Theta[vars_attached_to_2nd, vars_attached_to_2nd, drop = FALSE]
    Theta_inner <- cbind(
      rbind(Theta_2nd, matrix(0, nrow = nrow(I), ncol = ncol(Theta_2nd))),
      rbind(matrix(0, nrow = nrow(Theta_2nd), ncol = ncol(I)), matrix(0, nrow = nrow(I), ncol = ncol(I)))
    )

    rownames(Theta_inner) <- c(vars_attached_to_2nd, vars_not_attached_to_2nd)
    colnames(Theta_inner) <- c(vars_attached_to_2nd, vars_not_attached_to_2nd)

    ## "Clean" Lambda and Theta
    selector1 <- !(rownames(Lambda) %in% vars_attached_to_2nd)
    Lambda <- Lambda[selector1, !(colnames(Lambda) %in% vars_2nd), drop = FALSE]

    Theta  <- Theta[selector1, selector1, drop = FALSE]

    vcv_construct_inner <- generateConstructCor(.Gamma = Gamma, .B = B, .Phi = Phi)

    vcv_construct <- Lambda_inner[colnames(Lambda), rownames(vcv_construct_inner)] %*%
      vcv_construct_inner %*% t(Lambda_inner[colnames(Lambda), rownames(vcv_construct_inner)]) +
      Theta_inner[colnames(Lambda), colnames(Lambda)]

  } else {
    # Compute the construct correlation matrix
    vcv_construct <- generateConstructCor(.Gamma = Gamma, .B = B, .Phi = Phi)

  }

  # Compute the indicator correlation matrix (Sigma)
  Sigma <- Lambda %*% vcv_construct %*% t(Lambda) + Theta

  Sigma[lower.tri(Sigma)] <- t(Sigma)[lower.tri(Sigma)]

  # Check if semi-positve definite
  if (!matrixcalc::is.positive.semi.definite(Sigma)) {
    if(.handle_negative_definite %in% c("drop", "set_NA")) {
      NA
    } else if(.handle_negative_definite == "stop") {
      stop("Indicator correlation matrix is not semi-positive definite.",
           call. = FALSE)
    }
  } else {
    Sigma
  }
}
