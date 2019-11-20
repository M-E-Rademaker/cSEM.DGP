#' Compute construct correlation matrix
#'
#' Calculate the construct correlation matrix. Currently, there
#' is a limit of eigth constructs.
#'
#' @param .structural A matrix mimicking the structural relationship
#'   between constructs. Up to 8 constructs are supported.
#' @param .gamma A (8 x 8) matrix containing the path coefficients.
#'
#' @return A (8 x 8) matrix of construct correlations.
#'
#' @keywords internal

generateConstructCor <- function(
  .beta = NULL,
  .gamma = NULL,
  .phi = NULL
  ){

  k <- nrow(.beta)

  # Define Variances of the structural error terms
  Psi <- matrix(0, nrow = nrow(.beta), ncol = ncol(.beta),
                dimnames = dimnames(.beta))

 # Define gamma, beta and phi for the following calculations
  gamma_temp                                 <- matrix(0, nrow = 7, ncol = 5)
  gamma_temp[1:nrow(.gamma), 1:ncol(.gamma)] <- .gamma
  beta_temp                                  <- matrix(0, nrow = 7, ncol = 7)
  beta_temp[1:nrow(.beta), 1:ncol(.beta)]    <- .beta
  phi_temp                                   <- matrix(0, nrow = 5, ncol = 5)
  phi_temp[1:nrow(.phi), 1:ncol(.phi)]       <- .phi


  Psi[1,1] <- varzeta1(beta_temp, gamma_temp, phi_temp)
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

  B            <- .beta
  Gamma        <- .gamma
  I            <- diag(nrow(B))
  Pi           <- solve(I - B)%*%Gamma
  VCV_endo_exo <- Pi%*%.phi
  VCV_endo     <- Pi%*%.phi%*%t(Pi) + solve(I - B)%*%Psi%*%t(solve(I - B))

  VCV          <- cbind(rbind(.phi, VCV_endo_exo),rbind(t(VCV_endo_exo),VCV_endo))
  return(VCV)
}
