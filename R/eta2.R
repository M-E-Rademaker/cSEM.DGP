#' Calculating the covariances between the second construct and the other
#' constructs.
#' Note: Since the covariance is a symmetric function and Cov(eta1,eta2)
#' is computed in the file eta1.R, the calculation starts
#' with Cov(eta2,eta3).
#' @noRd
eta2eta3 <- function(gamma){

  gamma[2,1]*gamma[3,1] + gamma[3,2]
}


eta2eta4 <- function(gamma){

  gamma[2,1]*gamma[4,1] + gamma[4,2] + gamma[2,1]*gamma[3,1]*gamma[4,3] + gamma[3,2]*gamma[4,3]
}


eta2eta5 <- function(gamma){

  gamma[2,1]*gamma[5,1] + gamma[5,2] + gamma[2,1]*gamma[3,1]*gamma[5,3] +
  gamma[3,2]*gamma[5,3] + gamma[2,1]*gamma[4,1]*gamma[5,4] + gamma[4,2]*gamma[5,4] +
  gamma[2,1]*gamma[3,1]*gamma[4,3]*gamma[5,4] + gamma[3,2]*gamma[4,3]*gamma[5,4]
}


eta2eta6 <- function(gamma){

  gamma[2,1]*gamma[6,1] + gamma[6,2] + gamma[2,1]*gamma[3,1]*gamma[6,3] +
  gamma[3,2]*gamma[6,3] + gamma[2,1]*gamma[4,1]*gamma[6,4] + gamma[4,2]*gamma[6,4] +
  gamma[2,1]*gamma[3,1]*gamma[4,3]*gamma[6,4] + gamma[3,2]*gamma[4,3]*gamma[6,4] +
  gamma[2,1]*gamma[5,1]*gamma[6,5] + gamma[5,2]*gamma[6,5] +
  gamma[2,1]*gamma[3,1]*gamma[5,3]*gamma[6,5] + gamma[3,2]*gamma[5,3]*gamma[6,5] +
  gamma[2,1]*gamma[4,1]*gamma[5,4]*gamma[6,5] + gamma[4,2]*gamma[5,4]*gamma[6,5] +
  gamma[2,1]*gamma[3,1]*gamma[4,3]*gamma[5,4]*gamma[6,5] +
  gamma[3,2]*gamma[4,3]*gamma[5,4]*gamma[6,5]
}


eta2eta7 <- function(gamma){

  gamma[2,1]*gamma[7,1] + gamma[7,2] + gamma[2,1]*gamma[3,1]*gamma[7,3] +
  gamma[3,2]*gamma[7,3] + gamma[2,1]*gamma[4,1]*gamma[7,4] + gamma[4,2]*gamma[7,4] +
  gamma[2,1]*gamma[3,1]*gamma[4,3]*gamma[7,4] + gamma[3,2]*gamma[4,3]*gamma[7,4] +
  gamma[2,1]*gamma[5,1]*gamma[7,5] + gamma[5,2]*gamma[7,5] +
  gamma[2,1]*gamma[3,1]*gamma[5,3]*gamma[7,5] + gamma[3,2]*gamma[5,3]*gamma[7,5] +
  gamma[2,1]*gamma[4,1]*gamma[5,4]*gamma[7,5] + gamma[4,2]*gamma[5,4]*gamma[7,5] +
  gamma[2,1]*gamma[3,1]*gamma[4,3]*gamma[5,4]*gamma[7,5] +
  gamma[3,2]*gamma[4,3]*gamma[5,4]*gamma[7,5] + gamma[2,1]*gamma[6,1]*gamma[7,6] +
  gamma[6,2]*gamma[7,6] + gamma[2,1]*gamma[3,1]*gamma[6,3]*gamma[7,6] +
  gamma[3,2]*gamma[6,3]*gamma[7,6] + gamma[2,1]*gamma[4,1]*gamma[6,4]*gamma[7,6] +
  gamma[4,2]*gamma[6,4]*gamma[7,6] + gamma[2,1]*gamma[3,1]*gamma[4,3]*gamma[6,4]*gamma[7,6] +
  gamma[3,2]*gamma[4,3]*gamma[6,4]*gamma[7,6] + gamma[2,1]*gamma[5,1]*gamma[6,5]*gamma[7,6] +
  gamma[5,2]*gamma[6,5]*gamma[7,6] + gamma[2,1]*gamma[3,1]*gamma[5,3]*gamma[6,5]*gamma[7,6] +
  gamma[3,2]*gamma[5,3]*gamma[6,5]*gamma[7,6] +
  gamma[2,1]*gamma[4,1]*gamma[5,4]*gamma[6,5]*gamma[7,6] +
  gamma[4,2]*gamma[5,4]*gamma[6,5]*gamma[7,6] +
  gamma[2,1]*gamma[3,1]*gamma[4,3]*gamma[5,4]*gamma[6,5]*gamma[7,6] +
  gamma[3,2]*gamma[4,3]*gamma[5,4]*gamma[6,5]*gamma[7,6]
}


eta2eta8 <- function(gamma){

  gamma[2,1]*gamma[8,1] + gamma[8,2] + gamma[2,1]*gamma[3,1]*gamma[8,3] +
  gamma[3,2]*gamma[8,3] + gamma[2,1]*gamma[4,1]*gamma[8,4] + gamma[4,2]*gamma[8,4] +
  gamma[2,1]*gamma[3,1]*gamma[4,3]*gamma[8,4] + gamma[3,2]*gamma[4,3]*gamma[8,4] +
  gamma[2,1]*gamma[5,1]*gamma[8,5] + gamma[5,2]*gamma[8,5] +
  gamma[2,1]*gamma[3,1]*gamma[5,3]*gamma[8,5] + gamma[3,2]*gamma[5,3]*gamma[8,5] +
  gamma[2,1]*gamma[4,1]*gamma[5,4]*gamma[8,5] + gamma[4,2]*gamma[5,4]*gamma[8,5] +
  gamma[2,1]*gamma[3,1]*gamma[4,3]*gamma[5,4]*gamma[8,5] +
  gamma[3,2]*gamma[4,3]*gamma[5,4]*gamma[8,5] + gamma[2,1]*gamma[6,1]*gamma[8,6] +
  gamma[6,2]*gamma[8,6] + gamma[2,1]*gamma[3,1]*gamma[6,3]*gamma[8,6] +
  gamma[3,2]*gamma[6,3]*gamma[8,6] + gamma[2,1]*gamma[4,1]*gamma[6,4]*gamma[8,6] +
  gamma[4,2]*gamma[6,4]*gamma[8,6] + gamma[2,1]*gamma[3,1]*gamma[4,3]*gamma[6,4]*gamma[8,6] +
  gamma[3,2]*gamma[4,3]*gamma[6,4]*gamma[8,6] + gamma[2,1]*gamma[5,1]*gamma[6,5]*gamma[8,6] +
  gamma[5,2]*gamma[6,5]*gamma[8,6] + gamma[2,1]*gamma[3,1]*gamma[5,3]*gamma[6,5]*gamma[8,6] +
  gamma[3,2]*gamma[5,3]*gamma[6,5]*gamma[8,6] +
  gamma[2,1]*gamma[4,1]*gamma[5,4]*gamma[6,5]*gamma[8,6] +
  gamma[4,2]*gamma[5,4]*gamma[6,5]*gamma[8,6] +
  gamma[2,1]*gamma[3,1]*gamma[4,3]*gamma[5,4]*gamma[6,5]*gamma[8,6] +
  gamma[3,2]*gamma[4,3]*gamma[5,4]*gamma[6,5]*gamma[8,6] + gamma[2,1]*gamma[7,1]*gamma[8,7] +
  gamma[7,2]*gamma[8,7] + gamma[2,1]*gamma[3,1]*gamma[7,3]*gamma[8,7] +
  gamma[3,2]*gamma[7,3]*gamma[8,7] + gamma[2,1]*gamma[4,1]*gamma[7,4]*gamma[8,7] +
  gamma[4,2]*gamma[7,4]*gamma[8,7] + gamma[2,1]*gamma[3,1]*gamma[4,3]*gamma[7,4]*gamma[8,7] +
  gamma[3,2]*gamma[4,3]*gamma[7,4]*gamma[8,7] + gamma[2,1]*gamma[5,1]*gamma[7,5]*gamma[8,7] +
  gamma[5,2]*gamma[7,5]*gamma[8,7] + gamma[2,1]*gamma[3,1]*gamma[5,3]*gamma[7,5]*gamma[8,7] +
  gamma[3,2]*gamma[5,3]*gamma[7,5]*gamma[8,7] +
  gamma[2,1]*gamma[4,1]*gamma[5,4]*gamma[7,5]*gamma[8,7] +
  gamma[4,2]*gamma[5,4]*gamma[7,5]*gamma[8,7] +
  gamma[2,1]*gamma[3,1]*gamma[4,3]*gamma[5,4]*gamma[7,5]*gamma[8,7] +
  gamma[3,2]*gamma[4,3]*gamma[5,4]*gamma[7,5]*gamma[8,7] +
  gamma[2,1]*gamma[6,1]*gamma[7,6]*gamma[8,7] + gamma[6,2]*gamma[7,6]*gamma[8,7] +
  gamma[2,1]*gamma[3,1]*gamma[6,3]*gamma[7,6]*gamma[8,7] +
  gamma[3,2]*gamma[6,3]*gamma[7,6]*gamma[8,7] +
  gamma[2,1]*gamma[4,1]*gamma[6,4]*gamma[7,6]*gamma[8,7] +
  gamma[4,2]*gamma[6,4]*gamma[7,6]*gamma[8,7] +
  gamma[2,1]*gamma[3,1]*gamma[4,3]*gamma[6,4]*gamma[7,6]*gamma[8,7] +
  gamma[3,2]*gamma[4,3]*gamma[6,4]*gamma[7,6]*gamma[8,7] +
  gamma[2,1]*gamma[5,1]*gamma[6,5]*gamma[7,6]*gamma[8,7] +
  gamma[5,2]*gamma[6,5]*gamma[7,6]*gamma[8,7] +
  gamma[2,1]*gamma[3,1]*gamma[5,3]*gamma[6,5]*gamma[7,6]*gamma[8,7] +
  gamma[3,2]*gamma[5,3]*gamma[6,5]*gamma[7,6]*gamma[8,7] +
  gamma[2,1]*gamma[4,1]*gamma[5,4]*gamma[6,5]*gamma[7,6]*gamma[8,7] +
  gamma[4,2]*gamma[5,4]*gamma[6,5]*gamma[7,6]*gamma[8,7] +
  gamma[2,1]*gamma[3,1]*gamma[4,3]*gamma[5,4]*gamma[6,5]*gamma[7,6]*gamma[8,7] +
  gamma[3,2]*gamma[4,3]*gamma[5,4]*gamma[6,5]*gamma[7,6]*gamma[8,7]
}

