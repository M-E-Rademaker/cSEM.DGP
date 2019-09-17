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
#' @param .model A model in [lavaan model syntax][lavaan::model.syntax]
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
  model     <- cSEM::parseModel(.model)
  con_type  <- model$construct_type
  Lambda    <- t(model$measurement)
  Theta     <- model$error_cor

  ## Modify and fill Lambda
  for(j in colnames(Lambda)) {

    if(con_type[j] == "Composite") {
      indicators <- colnames(model$measurement2[j, model$measurement2[j, ] != 0, drop = FALSE])
      # If j is a composite, the values of measurement2 are interpreted as weights
      w_j  <- model$measurement2[j, indicators]

      # If weights are given, the within-block indicator correlation matrix
      # must be given as well
      Sigma_jj <- as.matrix(model$error_cor2[indicators, indicators])
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

      # Compute Theta
      Theta[indicators, indicators] <-  diag(1 - diag(lambda_j %*% t(lambda_j)))
    }
  }

  vars_exo  <- names(which(rowSums(model$structural) == 0))
  vars_endo <- setdiff(colnames(Lambda), vars_exo)

  # Get Path Coefficients
  path_matrix <- model$structural2

  # Define Gamma (for 10 constructs)
  gamma       <- matrix(0, nrow = 8, ncol = 8,
                        dimnames = list(c("eta1","eta2","eta3","eta4","eta5","eta6","eta7","eta8"),
                                        c("eta1","eta2","eta3","eta4","eta5","eta6","eta7","eta8")))
  # Insert the Defined Path Coefficients
  gamma[1:ncol(path_matrix), 1:nrow(path_matrix)] <- path_matrix

  # Get the Correlation between the exogenous constructs
  phi_matrix <- model$Phi

  # Compute the Covariance matrix between the endogenous and between the endogenous
  # and the exogenous constructs
  vcv_matrix <- generateConstructCor(.structural = path_matrix, .gamma = gamma)

  # Combine the covariance matrix with the correlation matrix between the
  # exogenous variables
  vcv_matrix[1:nrow(phi_matrix), 1:ncol(phi_matrix)] <- phi_matrix

  # Compute the indicator correlation matrix
  indicator_cor <- Lambda %*% vcv_matrix %*% t(Lambda) + Theta

  indicator_cor[lower.tri(indicator_cor)] <- t(indicator_cor)[lower.tri(indicator_cor)]

  # Check if semi-positve definite
  if (!matrixcalc::is.positive.semi.definite(indicator_cor)) {
    if(.handle_negative_definite %in% c("drop", "set_NA")) {
      NA
    } else if(.handle_negative_definite == "stop") {
      stop("Indicator correlation matrix is not semi-positive definite.",
           call. = FALSE)
    }
  } else {
    indicator_cor
  }
}
