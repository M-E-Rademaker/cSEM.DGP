#' Internal: Compute the construct correlation matrix
#'
#' Calculate the construct correlation matrix.
#'
#' @param .Gamma A matrix containing the path coefficients from the exogenous on
#'   the endogenous constructs.
#' @param .B A matrix containing the path coefficients from the endogenous on
#'   the endogenous constructs.
#' @param .Phi A symmetric matrix of correlations between exogenous constructs
#'
#' @return A matrix of construct correlations.
#'
#' @keywords internal

generateConstructCor <- function(
  .Gamma = NULL,
  .B     = NULL,
  .Phi   = NULL
  ){

  # Number of endogenous
  k <- nrow(.B)

  # Set up empty matrix of variances of the structural error terms
  Psi <- matrix(0, nrow = nrow(.B), ncol = ncol(.B), dimnames = dimnames(.B))

 # Define gamma, beta and phi for the following calculations
  gamma_temp  <- matrix(0, nrow = 7, ncol = 5)
  beta_temp   <- matrix(0, nrow = 7, ncol = 7)
  phi_temp    <- matrix(0, nrow = 5, ncol = 5)

  beta_temp[1:nrow(.B), 1:ncol(.B)]          <- .B
  gamma_temp[1:nrow(.Gamma), 1:ncol(.Gamma)] <- .Gamma
  phi_temp[1:nrow(.Phi), 1:ncol(.Phi)]       <- .Phi


  Psi[1, 1] <- varzeta1(beta_temp, gamma_temp, phi_temp)

  if(k >= 2){
    Psi[2, 2] <- varzeta2(beta_temp, gamma_temp, phi_temp)
  }
  if(k >= 3){
    Psi[3, 3] <- varzeta3(beta_temp, gamma_temp, phi_temp)
  }
  if(k >= 4){
    Psi[4, 4] <- varzeta4(beta_temp, gamma_temp, phi_temp)
  }
  if(k >= 5){
    Psi[5, 5] <- varzeta5(beta_temp, gamma_temp, phi_temp)
  }
  if(k >= 6){
    Psi[6, 6] <- varzeta6(beta_temp, gamma_temp, phi_temp)
  }
  if(k >= 7){
    Psi[7, 7] <- varzeta7(beta_temp, gamma_temp, phi_temp)
  }

  I            <- diag(nrow(.B))
  Pi           <- solve(I - .B) %*% .Gamma
  VCV_endo_exo <- Pi %*% .Phi
  VCV_endo     <- Pi %*% .Phi %*% t(Pi) + solve(I - .B) %*% Psi %*% t(solve(I - .B))

  VCV          <- cbind(rbind(.Phi, VCV_endo_exo),rbind(t(VCV_endo_exo),VCV_endo))
  return(VCV)
}
