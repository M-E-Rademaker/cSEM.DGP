varzeta5 <- function(beta, gamma, phi){
  1 - beta[5,1]^2 - beta[5,2]^2 - beta[5,3]^2 -2*beta[3,1]*beta[4,1]*beta[5,3]*beta[5,4] -
   2*beta[2,1]*beta[3,2]*beta[4,1]*beta[5,3]*beta[5,4] -
   2*beta[2,1]*beta[3,1]*beta[4,2]*beta[5,3]*beta[5,4] -
   2*beta[3,2]*beta[4,2]*beta[5,3]*beta[5,4] -2*beta[4,3]*beta[5,3]*beta[5,4] - beta[5,4]^2 -
   2*beta[3,2]*beta[4,1]*beta[5,3]*beta[5,4]*gamma[1,1]*gamma[2,1] -
   2*beta[3,1]*beta[4,2]*beta[5,3]*beta[5,4]*gamma[1,1]*gamma[2,1] -
   2*beta[3,2]*beta[4,1]*beta[5,3]*beta[5,4]*gamma[1,2]*gamma[2,2] -
   2*beta[3,1]*beta[4,2]*beta[5,3]*beta[5,4]*gamma[1,2]*gamma[2,2] -
   2*beta[3,2]*beta[4,1]*beta[5,3]*beta[5,4]*gamma[1,3]*gamma[2,3] -
   2*beta[3,1]*beta[4,2]*beta[5,3]*beta[5,4]*gamma[1,3]*gamma[2,3] -
   2*beta[4,1]*beta[5,3]*beta[5,4]*gamma[1,1]*gamma[3,1] -
   2*beta[2,1]*beta[4,2]*beta[5,3]*beta[5,4]*gamma[1,1]*gamma[3,1] -
   2*beta[4,2]*beta[5,3]*beta[5,4]*gamma[2,1]*gamma[3,1] -
   2*beta[4,1]*beta[5,3]*beta[5,4]*gamma[1,2]*gamma[3,2] -
   2*beta[2,1]*beta[4,2]*beta[5,3]*beta[5,4]*gamma[1,2]*gamma[3,2] -
   2*beta[4,2]*beta[5,3]*beta[5,4]*gamma[2,2]*gamma[3,2] -
   2*beta[4,1]*beta[5,3]*beta[5,4]*gamma[1,3]*gamma[3,3] -
   2*beta[2,1]*beta[4,2]*beta[5,3]*beta[5,4]*gamma[1,3]*gamma[3,3] -
   2*beta[4,2]*beta[5,3]*beta[5,4]*gamma[2,3]*gamma[3,3] -
   2*beta[3,1]*beta[5,3]*beta[5,4]*gamma[1,1]*gamma[4,1] -
   2*beta[2,1]*beta[3,2]*beta[5,3]*beta[5,4]*gamma[1,1]*gamma[4,1] -
   2*beta[3,2]*beta[5,3]*beta[5,4]*gamma[2,1]*gamma[4,1] -
   2*beta[5,3]*beta[5,4]*gamma[3,1]*gamma[4,1] -
   2*beta[3,1]*beta[5,3]*beta[5,4]*gamma[1,2]*gamma[4,2] -
   2*beta[2,1]*beta[3,2]*beta[5,3]*beta[5,4]*gamma[1,2]*gamma[4,2] -
   2*beta[3,2]*beta[5,3]*beta[5,4]*gamma[2,2]*gamma[4,2] -
   2*beta[5,3]*beta[5,4]*gamma[3,2]*gamma[4,2] -
   2*beta[3,1]*beta[5,3]*beta[5,4]*gamma[1,3]*gamma[4,3] -
   2*beta[2,1]*beta[3,2]*beta[5,3]*beta[5,4]*gamma[1,3]*gamma[4,3] -
   2*beta[3,2]*beta[5,3]*beta[5,4]*gamma[2,3]*gamma[4,3] -
   2*beta[5,3]*beta[5,4]*gamma[3,3]*gamma[4,3] -2*beta[3,1]*beta[5,3]*gamma[1,1]*gamma[5,1] -
   2*beta[2,1]*beta[3,2]*beta[5,3]*gamma[1,1]*gamma[5,1] -
   2*beta[4,1]*beta[5,4]*gamma[1,1]*gamma[5,1] -
   2*beta[2,1]*beta[4,2]*beta[5,4]*gamma[1,1]*gamma[5,1] -
   2*beta[3,1]*beta[4,3]*beta[5,4]*gamma[1,1]*gamma[5,1] -
   2*beta[2,1]*beta[3,2]*beta[4,3]*beta[5,4]*gamma[1,1]*gamma[5,1] -
   2*beta[3,2]*beta[5,3]*gamma[2,1]*gamma[5,1] -2*beta[4,2]*beta[5,4]*gamma[2,1]*gamma[5,1] -
   2*beta[3,2]*beta[4,3]*beta[5,4]*gamma[2,1]*gamma[5,1] -2*beta[5,3]*gamma[3,1]*gamma[5,1] -
   2*beta[4,3]*beta[5,4]*gamma[3,1]*gamma[5,1] -
   2*beta[5,4]*gamma[4,1]*gamma[5,1] - gamma[5,1]^2 -
   2*beta[3,1]*beta[5,3]*gamma[1,2]*gamma[5,2] -
   2*beta[2,1]*beta[3,2]*beta[5,3]*gamma[1,2]*gamma[5,2] -
   2*beta[4,1]*beta[5,4]*gamma[1,2]*gamma[5,2] -
   2*beta[2,1]*beta[4,2]*beta[5,4]*gamma[1,2]*gamma[5,2] -
   2*beta[3,1]*beta[4,3]*beta[5,4]*gamma[1,2]*gamma[5,2] -
   2*beta[2,1]*beta[3,2]*beta[4,3]*beta[5,4]*gamma[1,2]*gamma[5,2] -
   2*beta[3,2]*beta[5,3]*gamma[2,2]*gamma[5,2] -2*beta[4,2]*beta[5,4]*gamma[2,2]*gamma[5,2] -
   2*beta[3,2]*beta[4,3]*beta[5,4]*gamma[2,2]*gamma[5,2] -2*beta[5,3]*gamma[3,2]*gamma[5,2] -
   2*beta[4,3]*beta[5,4]*gamma[3,2]*gamma[5,2] -
   2*beta[5,4]*gamma[4,2]*gamma[5,2] - gamma[5,2]^2 -
   2*beta[3,1]*beta[5,3]*gamma[1,3]*gamma[5,3] -
   2*beta[2,1]*beta[3,2]*beta[5,3]*gamma[1,3]*gamma[5,3] -
   2*beta[4,1]*beta[5,4]*gamma[1,3]*gamma[5,3] -
   2*beta[2,1]*beta[4,2]*beta[5,4]*gamma[1,3]*gamma[5,3] -
   2*beta[3,1]*beta[4,3]*beta[5,4]*gamma[1,3]*gamma[5,3] -
   2*beta[2,1]*beta[3,2]*beta[4,3]*beta[5,4]*gamma[1,3]*gamma[5,3] -
   2*beta[3,2]*beta[5,3]*gamma[2,3]*gamma[5,3] -2*beta[4,2]*beta[5,4]*gamma[2,3]*gamma[5,3] -
   2*beta[3,2]*beta[4,3]*beta[5,4]*gamma[2,3]*gamma[5,3] -2*beta[5,3]*gamma[3,3]*gamma[5,3] -
   2*beta[4,3]*beta[5,4]*gamma[3,3]*gamma[5,3] -
   2*beta[5,4]*gamma[4,3]*gamma[5,3] - gamma[5,3]^2 -
   2*beta[3,2]*beta[4,1]*beta[5,3]*beta[5,4]*gamma[1,2]*gamma[2,1]*phi[1,2] -
   2*beta[3,1]*beta[4,2]*beta[5,3]*beta[5,4]*gamma[1,2]*gamma[2,1]*phi[1,2] -
   2*beta[3,2]*beta[4,1]*beta[5,3]*beta[5,4]*gamma[1,1]*gamma[2,2]*phi[1,2] -
   2*beta[3,1]*beta[4,2]*beta[5,3]*beta[5,4]*gamma[1,1]*gamma[2,2]*phi[1,2] -
   2*beta[4,1]*beta[5,3]*beta[5,4]*gamma[1,2]*gamma[3,1]*phi[1,2] -
   2*beta[2,1]*beta[4,2]*beta[5,3]*beta[5,4]*gamma[1,2]*gamma[3,1]*phi[1,2] -
   2*beta[4,2]*beta[5,3]*beta[5,4]*gamma[2,2]*gamma[3,1]*phi[1,2] -
   2*beta[4,1]*beta[5,3]*beta[5,4]*gamma[1,1]*gamma[3,2]*phi[1,2] -
   2*beta[2,1]*beta[4,2]*beta[5,3]*beta[5,4]*gamma[1,1]*gamma[3,2]*phi[1,2] -
   2*beta[4,2]*beta[5,3]*beta[5,4]*gamma[2,1]*gamma[3,2]*phi[1,2] -
   2*beta[3,1]*beta[5,3]*beta[5,4]*gamma[1,2]*gamma[4,1]*phi[1,2] -
   2*beta[2,1]*beta[3,2]*beta[5,3]*beta[5,4]*gamma[1,2]*gamma[4,1]*phi[1,2] -
   2*beta[3,2]*beta[5,3]*beta[5,4]*gamma[2,2]*gamma[4,1]*phi[1,2] -
   2*beta[5,3]*beta[5,4]*gamma[3,2]*gamma[4,1]*phi[1,2] -
   2*beta[3,1]*beta[5,3]*beta[5,4]*gamma[1,1]*gamma[4,2]*phi[1,2] -
   2*beta[2,1]*beta[3,2]*beta[5,3]*beta[5,4]*gamma[1,1]*gamma[4,2]*phi[1,2] -
   2*beta[3,2]*beta[5,3]*beta[5,4]*gamma[2,1]*gamma[4,2]*phi[1,2] -
   2*beta[5,3]*beta[5,4]*gamma[3,1]*gamma[4,2]*phi[1,2] -
   2*beta[3,1]*beta[5,3]*gamma[1,2]*gamma[5,1]*phi[1,2] -
   2*beta[2,1]*beta[3,2]*beta[5,3]*gamma[1,2]*gamma[5,1]*phi[1,2] -
   2*beta[4,1]*beta[5,4]*gamma[1,2]*gamma[5,1]*phi[1,2] -
   2*beta[2,1]*beta[4,2]*beta[5,4]*gamma[1,2]*gamma[5,1]*phi[1,2] -
   2*beta[3,1]*beta[4,3]*beta[5,4]*gamma[1,2]*gamma[5,1]*phi[1,2] -
   2*beta[2,1]*beta[3,2]*beta[4,3]*beta[5,4]*gamma[1,2]*gamma[5,1]*phi[1,2] -
   2*beta[3,2]*beta[5,3]*gamma[2,2]*gamma[5,1]*phi[1,2] -
   2*beta[4,2]*beta[5,4]*gamma[2,2]*gamma[5,1]*phi[1,2] -
   2*beta[3,2]*beta[4,3]*beta[5,4]*gamma[2,2]*gamma[5,1]*phi[1,2] -
   2*beta[5,3]*gamma[3,2]*gamma[5,1]*phi[1,2] -
   2*beta[4,3]*beta[5,4]*gamma[3,2]*gamma[5,1]*phi[1,2] -
   2*beta[5,4]*gamma[4,2]*gamma[5,1]*phi[1,2] -
   2*beta[3,1]*beta[5,3]*gamma[1,1]*gamma[5,2]*phi[1,2] -
   2*beta[2,1]*beta[3,2]*beta[5,3]*gamma[1,1]*gamma[5,2]*phi[1,2] -
   2*beta[4,1]*beta[5,4]*gamma[1,1]*gamma[5,2]*phi[1,2] -
   2*beta[2,1]*beta[4,2]*beta[5,4]*gamma[1,1]*gamma[5,2]*phi[1,2] -
   2*beta[3,1]*beta[4,3]*beta[5,4]*gamma[1,1]*gamma[5,2]*phi[1,2] -
   2*beta[2,1]*beta[3,2]*beta[4,3]*beta[5,4]*gamma[1,1]*gamma[5,2]*phi[1,2] -
   2*beta[3,2]*beta[5,3]*gamma[2,1]*gamma[5,2]*phi[1,2] -
   2*beta[4,2]*beta[5,4]*gamma[2,1]*gamma[5,2]*phi[1,2] -
   2*beta[3,2]*beta[4,3]*beta[5,4]*gamma[2,1]*gamma[5,2]*phi[1,2] -
   2*beta[5,3]*gamma[3,1]*gamma[5,2]*phi[1,2] -
   2*beta[4,3]*beta[5,4]*gamma[3,1]*gamma[5,2]*phi[1,2] -
   2*beta[5,4]*gamma[4,1]*gamma[5,2]*phi[1,2] -2*gamma[5,1]*gamma[5,2]*phi[1,2] -
   2*beta[3,2]*beta[4,1]*beta[5,3]*beta[5,4]*gamma[1,3]*gamma[2,1]*phi[1,3] -
   2*beta[3,1]*beta[4,2]*beta[5,3]*beta[5,4]*gamma[1,3]*gamma[2,1]*phi[1,3] -
   2*beta[3,2]*beta[4,1]*beta[5,3]*beta[5,4]*gamma[1,1]*gamma[2,3]*phi[1,3] -
   2*beta[3,1]*beta[4,2]*beta[5,3]*beta[5,4]*gamma[1,1]*gamma[2,3]*phi[1,3] -
   2*beta[4,1]*beta[5,3]*beta[5,4]*gamma[1,3]*gamma[3,1]*phi[1,3] -
   2*beta[2,1]*beta[4,2]*beta[5,3]*beta[5,4]*gamma[1,3]*gamma[3,1]*phi[1,3] -
   2*beta[4,2]*beta[5,3]*beta[5,4]*gamma[2,3]*gamma[3,1]*phi[1,3] -
   2*beta[4,1]*beta[5,3]*beta[5,4]*gamma[1,1]*gamma[3,3]*phi[1,3] -
   2*beta[2,1]*beta[4,2]*beta[5,3]*beta[5,4]*gamma[1,1]*gamma[3,3]*phi[1,3] -
   2*beta[4,2]*beta[5,3]*beta[5,4]*gamma[2,1]*gamma[3,3]*phi[1,3] -
   2*beta[3,1]*beta[5,3]*beta[5,4]*gamma[1,3]*gamma[4,1]*phi[1,3] -
   2*beta[2,1]*beta[3,2]*beta[5,3]*beta[5,4]*gamma[1,3]*gamma[4,1]*phi[1,3] -
   2*beta[3,2]*beta[5,3]*beta[5,4]*gamma[2,3]*gamma[4,1]*phi[1,3] -
   2*beta[5,3]*beta[5,4]*gamma[3,3]*gamma[4,1]*phi[1,3] -
   2*beta[3,1]*beta[5,3]*beta[5,4]*gamma[1,1]*gamma[4,3]*phi[1,3] -
   2*beta[2,1]*beta[3,2]*beta[5,3]*beta[5,4]*gamma[1,1]*gamma[4,3]*phi[1,3] -
   2*beta[3,2]*beta[5,3]*beta[5,4]*gamma[2,1]*gamma[4,3]*phi[1,3] -
   2*beta[5,3]*beta[5,4]*gamma[3,1]*gamma[4,3]*phi[1,3] -
   2*beta[3,1]*beta[5,3]*gamma[1,3]*gamma[5,1]*phi[1,3] -
   2*beta[2,1]*beta[3,2]*beta[5,3]*gamma[1,3]*gamma[5,1]*phi[1,3] -
   2*beta[4,1]*beta[5,4]*gamma[1,3]*gamma[5,1]*phi[1,3] -
   2*beta[2,1]*beta[4,2]*beta[5,4]*gamma[1,3]*gamma[5,1]*phi[1,3] -
   2*beta[3,1]*beta[4,3]*beta[5,4]*gamma[1,3]*gamma[5,1]*phi[1,3] -
   2*beta[2,1]*beta[3,2]*beta[4,3]*beta[5,4]*gamma[1,3]*gamma[5,1]*phi[1,3] -
   2*beta[3,2]*beta[5,3]*gamma[2,3]*gamma[5,1]*phi[1,3] -
   2*beta[4,2]*beta[5,4]*gamma[2,3]*gamma[5,1]*phi[1,3] -
   2*beta[3,2]*beta[4,3]*beta[5,4]*gamma[2,3]*gamma[5,1]*phi[1,3] -
   2*beta[5,3]*gamma[3,3]*gamma[5,1]*phi[1,3] -
   2*beta[4,3]*beta[5,4]*gamma[3,3]*gamma[5,1]*phi[1,3] -
   2*beta[5,4]*gamma[4,3]*gamma[5,1]*phi[1,3] -
   2*beta[3,1]*beta[5,3]*gamma[1,1]*gamma[5,3]*phi[1,3] -
   2*beta[2,1]*beta[3,2]*beta[5,3]*gamma[1,1]*gamma[5,3]*phi[1,3] -
   2*beta[4,1]*beta[5,4]*gamma[1,1]*gamma[5,3]*phi[1,3] -
   2*beta[2,1]*beta[4,2]*beta[5,4]*gamma[1,1]*gamma[5,3]*phi[1,3] -
   2*beta[3,1]*beta[4,3]*beta[5,4]*gamma[1,1]*gamma[5,3]*phi[1,3] -
   2*beta[2,1]*beta[3,2]*beta[4,3]*beta[5,4]*gamma[1,1]*gamma[5,3]*phi[1,3] -
   2*beta[3,2]*beta[5,3]*gamma[2,1]*gamma[5,3]*phi[1,3] -
   2*beta[4,2]*beta[5,4]*gamma[2,1]*gamma[5,3]*phi[1,3] -
   2*beta[3,2]*beta[4,3]*beta[5,4]*gamma[2,1]*gamma[5,3]*phi[1,3] -
   2*beta[5,3]*gamma[3,1]*gamma[5,3]*phi[1,3] -
   2*beta[4,3]*beta[5,4]*gamma[3,1]*gamma[5,3]*phi[1,3] -
   2*beta[5,4]*gamma[4,1]*gamma[5,3]*phi[1,3] -2*gamma[5,1]*gamma[5,3]*phi[1,3] -
   2*beta[3,2]*beta[4,1]*beta[5,3]*beta[5,4]*gamma[1,3]*gamma[2,2]*phi[2,3] -
   2*beta[3,1]*beta[4,2]*beta[5,3]*beta[5,4]*gamma[1,3]*gamma[2,2]*phi[2,3] -
   2*beta[3,2]*beta[4,1]*beta[5,3]*beta[5,4]*gamma[1,2]*gamma[2,3]*phi[2,3] -
   2*beta[3,1]*beta[4,2]*beta[5,3]*beta[5,4]*gamma[1,2]*gamma[2,3]*phi[2,3] -
   2*beta[4,1]*beta[5,3]*beta[5,4]*gamma[1,3]*gamma[3,2]*phi[2,3] -
   2*beta[2,1]*beta[4,2]*beta[5,3]*beta[5,4]*gamma[1,3]*gamma[3,2]*phi[2,3] -
   2*beta[4,2]*beta[5,3]*beta[5,4]*gamma[2,3]*gamma[3,2]*phi[2,3] -
   2*beta[4,1]*beta[5,3]*beta[5,4]*gamma[1,2]*gamma[3,3]*phi[2,3] -
   2*beta[2,1]*beta[4,2]*beta[5,3]*beta[5,4]*gamma[1,2]*gamma[3,3]*phi[2,3] -
   2*beta[4,2]*beta[5,3]*beta[5,4]*gamma[2,2]*gamma[3,3]*phi[2,3] -
   2*beta[3,1]*beta[5,3]*beta[5,4]*gamma[1,3]*gamma[4,2]*phi[2,3] -
   2*beta[2,1]*beta[3,2]*beta[5,3]*beta[5,4]*gamma[1,3]*gamma[4,2]*phi[2,3] -
   2*beta[3,2]*beta[5,3]*beta[5,4]*gamma[2,3]*gamma[4,2]*phi[2,3] -
   2*beta[5,3]*beta[5,4]*gamma[3,3]*gamma[4,2]*phi[2,3] -
   2*beta[3,1]*beta[5,3]*beta[5,4]*gamma[1,2]*gamma[4,3]*phi[2,3] -
   2*beta[2,1]*beta[3,2]*beta[5,3]*beta[5,4]*gamma[1,2]*gamma[4,3]*phi[2,3] -
   2*beta[3,2]*beta[5,3]*beta[5,4]*gamma[2,2]*gamma[4,3]*phi[2,3] -
   2*beta[5,3]*beta[5,4]*gamma[3,2]*gamma[4,3]*phi[2,3] -
   2*beta[3,1]*beta[5,3]*gamma[1,3]*gamma[5,2]*phi[2,3] -
   2*beta[2,1]*beta[3,2]*beta[5,3]*gamma[1,3]*gamma[5,2]*phi[2,3] -
   2*beta[4,1]*beta[5,4]*gamma[1,3]*gamma[5,2]*phi[2,3] -
   2*beta[2,1]*beta[4,2]*beta[5,4]*gamma[1,3]*gamma[5,2]*phi[2,3] -
   2*beta[3,1]*beta[4,3]*beta[5,4]*gamma[1,3]*gamma[5,2]*phi[2,3] -
   2*beta[2,1]*beta[3,2]*beta[4,3]*beta[5,4]*gamma[1,3]*gamma[5,2]*phi[2,3] -
   2*beta[3,2]*beta[5,3]*gamma[2,3]*gamma[5,2]*phi[2,3] -
   2*beta[4,2]*beta[5,4]*gamma[2,3]*gamma[5,2]*phi[2,3] -
   2*beta[3,2]*beta[4,3]*beta[5,4]*gamma[2,3]*gamma[5,2]*phi[2,3] -
   2*beta[5,3]*gamma[3,3]*gamma[5,2]*phi[2,3] -
   2*beta[4,3]*beta[5,4]*gamma[3,3]*gamma[5,2]*phi[2,3] -
   2*beta[5,4]*gamma[4,3]*gamma[5,2]*phi[2,3] -
   2*beta[3,1]*beta[5,3]*gamma[1,2]*gamma[5,3]*phi[2,3] -
   2*beta[2,1]*beta[3,2]*beta[5,3]*gamma[1,2]*gamma[5,3]*phi[2,3] -
   2*beta[4,1]*beta[5,4]*gamma[1,2]*gamma[5,3]*phi[2,3] -
   2*beta[2,1]*beta[4,2]*beta[5,4]*gamma[1,2]*gamma[5,3]*phi[2,3] -
   2*beta[3,1]*beta[4,3]*beta[5,4]*gamma[1,2]*gamma[5,3]*phi[2,3] -
   2*beta[2,1]*beta[3,2]*beta[4,3]*beta[5,4]*gamma[1,2]*gamma[5,3]*phi[2,3] -
   2*beta[3,2]*beta[5,3]*gamma[2,2]*gamma[5,3]*phi[2,3] -
   2*beta[4,2]*beta[5,4]*gamma[2,2]*gamma[5,3]*phi[2,3] -
   2*beta[3,2]*beta[4,3]*beta[5,4]*gamma[2,2]*gamma[5,3]*phi[2,3] -
   2*beta[5,3]*gamma[3,2]*gamma[5,3]*phi[2,3] -
   2*beta[4,3]*beta[5,4]*gamma[3,2]*gamma[5,3]*phi[2,3] -
   2*beta[5,4]*gamma[4,2]*gamma[5,3]*phi[2,3] -2*gamma[5,2]*gamma[5,3]*phi[2,3] -
   2*beta[5,1]*(beta[3,1]*beta[5,3] + beta[4,1]*beta[5,4] + beta[3,1]*beta[4,3]*beta[5,4] +
                beta[2,1]*(beta[5,2] + beta[3,2]*beta[5,3] + beta[4,2]*beta[5,4] +
                          beta[3,2]*beta[4,3]*beta[5,4]) + beta[5,2]*gamma[1,1]*gamma[2,1] +
                beta[3,2]*beta[5,3]*gamma[1,1]*gamma[2,1] + beta[4,2]*beta[5,4]*gamma[1,1]*gamma[2,1] +
                beta[3,2]*beta[4,3]*beta[5,4]*gamma[1,1]*gamma[2,1] + beta[5,2]*gamma[1,2]*gamma[2,2] +
                beta[3,2]*beta[5,3]*gamma[1,2]*gamma[2,2] + beta[4,2]*beta[5,4]*gamma[1,2]*gamma[2,2] +
                beta[3,2]*beta[4,3]*beta[5,4]*gamma[1,2]*gamma[2,2] + beta[5,2]*gamma[1,3]*gamma[2,3] +
                beta[3,2]*beta[5,3]*gamma[1,3]*gamma[2,3] + beta[4,2]*beta[5,4]*gamma[1,3]*gamma[2,3] +
                beta[3,2]*beta[4,3]*beta[5,4]*gamma[1,3]*gamma[2,3] + beta[5,3]*gamma[1,1]*gamma[3,1] +
                beta[4,3]*beta[5,4]*gamma[1,1]*gamma[3,1] + beta[5,3]*gamma[1,2]*gamma[3,2] +
                beta[4,3]*beta[5,4]*gamma[1,2]*gamma[3,2] + beta[5,3]*gamma[1,3]*gamma[3,3] +
                beta[4,3]*beta[5,4]*gamma[1,3]*gamma[3,3] + beta[5,4]*gamma[1,1]*gamma[4,1] +
                beta[5,4]*gamma[1,2]*gamma[4,2] + beta[5,4]*gamma[1,3]*gamma[4,3] +
                gamma[1,1]*gamma[5,1] + gamma[1,2]*gamma[5,2] + gamma[1,3]*gamma[5,3] +
                beta[5,2]*gamma[1,2]*gamma[2,1]*phi[1,2] +
                beta[3,2]*beta[5,3]*gamma[1,2]*gamma[2,1]*phi[1,2] +
                beta[4,2]*beta[5,4]*gamma[1,2]*gamma[2,1]*phi[1,2] +
                beta[3,2]*beta[4,3]*beta[5,4]*gamma[1,2]*gamma[2,1]*phi[1,2] +
                beta[5,2]*gamma[1,1]*gamma[2,2]*phi[1,2] +
                beta[3,2]*beta[5,3]*gamma[1,1]*gamma[2,2]*phi[1,2] +
                beta[4,2]*beta[5,4]*gamma[1,1]*gamma[2,2]*phi[1,2] +
                beta[3,2]*beta[4,3]*beta[5,4]*gamma[1,1]*gamma[2,2]*phi[1,2] +
                beta[5,3]*gamma[1,2]*gamma[3,1]*phi[1,2] +
                beta[4,3]*beta[5,4]*gamma[1,2]*gamma[3,1]*phi[1,2] +
                beta[5,3]*gamma[1,1]*gamma[3,2]*phi[1,2] +
                beta[4,3]*beta[5,4]*gamma[1,1]*gamma[3,2]*phi[1,2] +
                beta[5,4]*gamma[1,2]*gamma[4,1]*phi[1,2] + beta[5,4]*gamma[1,1]*gamma[4,2]*phi[1,2] +
                gamma[1,2]*gamma[5,1]*phi[1,2] + gamma[1,1]*gamma[5,2]*phi[1,2] +
                beta[5,2]*gamma[1,3]*gamma[2,1]*phi[1,3] +
                beta[3,2]*beta[5,3]*gamma[1,3]*gamma[2,1]*phi[1,3] +
                beta[4,2]*beta[5,4]*gamma[1,3]*gamma[2,1]*phi[1,3] +
                beta[3,2]*beta[4,3]*beta[5,4]*gamma[1,3]*gamma[2,1]*phi[1,3] +
                beta[5,2]*gamma[1,1]*gamma[2,3]*phi[1,3] +
                beta[3,2]*beta[5,3]*gamma[1,1]*gamma[2,3]*phi[1,3] +
                beta[4,2]*beta[5,4]*gamma[1,1]*gamma[2,3]*phi[1,3] +
                beta[3,2]*beta[4,3]*beta[5,4]*gamma[1,1]*gamma[2,3]*phi[1,3] +
                beta[5,3]*gamma[1,3]*gamma[3,1]*phi[1,3] +
                beta[4,3]*beta[5,4]*gamma[1,3]*gamma[3,1]*phi[1,3] +
                beta[5,3]*gamma[1,1]*gamma[3,3]*phi[1,3] +
                beta[4,3]*beta[5,4]*gamma[1,1]*gamma[3,3]*phi[1,3] +
                beta[5,4]*gamma[1,3]*gamma[4,1]*phi[1,3] + beta[5,4]*gamma[1,1]*gamma[4,3]*phi[1,3] +
                gamma[1,3]*gamma[5,1]*phi[1,3] + gamma[1,1]*gamma[5,3]*phi[1,3] +
                beta[5,2]*gamma[1,3]*gamma[2,2]*phi[2,3] +
                beta[3,2]*beta[5,3]*gamma[1,3]*gamma[2,2]*phi[2,3] +
                beta[4,2]*beta[5,4]*gamma[1,3]*gamma[2,2]*phi[2,3] +
                beta[3,2]*beta[4,3]*beta[5,4]*gamma[1,3]*gamma[2,2]*phi[2,3] +
                beta[5,2]*gamma[1,2]*gamma[2,3]*phi[2,3] +
                beta[3,2]*beta[5,3]*gamma[1,2]*gamma[2,3]*phi[2,3] +
                beta[4,2]*beta[5,4]*gamma[1,2]*gamma[2,3]*phi[2,3] +
                beta[3,2]*beta[4,3]*beta[5,4]*gamma[1,2]*gamma[2,3]*phi[2,3] +
                beta[5,3]*gamma[1,3]*gamma[3,2]*phi[2,3] +
                beta[4,3]*beta[5,4]*gamma[1,3]*gamma[3,2]*phi[2,3] +
                beta[5,3]*gamma[1,2]*gamma[3,3]*phi[2,3] +
                beta[4,3]*beta[5,4]*gamma[1,2]*gamma[3,3]*phi[2,3] +
                beta[5,4]*gamma[1,3]*gamma[4,2]*phi[2,3] + beta[5,4]*gamma[1,2]*gamma[4,3]*phi[2,3] +
                gamma[1,3]*gamma[5,2]*phi[2,3] + gamma[1,2]*gamma[5,3]*phi[2,3]) -
   2*beta[5,2]*(beta[3,2]*beta[5,3] + beta[4,2]*beta[5,4] + beta[3,2]*beta[4,3]*beta[5,4] +
                beta[3,1]*beta[5,3]*gamma[1,1]*gamma[2,1] + beta[4,1]*beta[5,4]*gamma[1,1]*gamma[2,1] +
                beta[3,1]*beta[4,3]*beta[5,4]*gamma[1,1]*gamma[2,1] +
                beta[3,1]*beta[5,3]*gamma[1,2]*gamma[2,2] + beta[4,1]*beta[5,4]*gamma[1,2]*gamma[2,2] +
                beta[3,1]*beta[4,3]*beta[5,4]*gamma[1,2]*gamma[2,2] +
                beta[3,1]*beta[5,3]*gamma[1,3]*gamma[2,3] + beta[4,1]*beta[5,4]*gamma[1,3]*gamma[2,3] +
                beta[3,1]*beta[4,3]*beta[5,4]*gamma[1,3]*gamma[2,3] + beta[5,3]*gamma[2,1]*gamma[3,1] +
                beta[4,3]*beta[5,4]*gamma[2,1]*gamma[3,1] + beta[5,3]*gamma[2,2]*gamma[3,2] +
                beta[4,3]*beta[5,4]*gamma[2,2]*gamma[3,2] + beta[5,3]*gamma[2,3]*gamma[3,3] +
                beta[4,3]*beta[5,4]*gamma[2,3]*gamma[3,3] + beta[5,4]*gamma[2,1]*gamma[4,1] +
                beta[5,4]*gamma[2,2]*gamma[4,2] + beta[5,4]*gamma[2,3]*gamma[4,3] +
                gamma[2,1]*gamma[5,1] + gamma[2,2]*gamma[5,2] + gamma[2,3]*gamma[5,3] +
                beta[3,1]*beta[5,3]*gamma[1,2]*gamma[2,1]*phi[1,2] +
                beta[4,1]*beta[5,4]*gamma[1,2]*gamma[2,1]*phi[1,2] +
                beta[3,1]*beta[4,3]*beta[5,4]*gamma[1,2]*gamma[2,1]*phi[1,2] +
                beta[3,1]*beta[5,3]*gamma[1,1]*gamma[2,2]*phi[1,2] +
                beta[4,1]*beta[5,4]*gamma[1,1]*gamma[2,2]*phi[1,2] +
                beta[3,1]*beta[4,3]*beta[5,4]*gamma[1,1]*gamma[2,2]*phi[1,2] +
                beta[5,3]*gamma[2,2]*gamma[3,1]*phi[1,2] +
                beta[4,3]*beta[5,4]*gamma[2,2]*gamma[3,1]*phi[1,2] +
                beta[5,3]*gamma[2,1]*gamma[3,2]*phi[1,2] +
                beta[4,3]*beta[5,4]*gamma[2,1]*gamma[3,2]*phi[1,2] +
                beta[5,4]*gamma[2,2]*gamma[4,1]*phi[1,2] + beta[5,4]*gamma[2,1]*gamma[4,2]*phi[1,2] +
                gamma[2,2]*gamma[5,1]*phi[1,2] + gamma[2,1]*gamma[5,2]*phi[1,2] +
                beta[3,1]*beta[5,3]*gamma[1,3]*gamma[2,1]*phi[1,3] +
                beta[4,1]*beta[5,4]*gamma[1,3]*gamma[2,1]*phi[1,3] +
                beta[3,1]*beta[4,3]*beta[5,4]*gamma[1,3]*gamma[2,1]*phi[1,3] +
                beta[3,1]*beta[5,3]*gamma[1,1]*gamma[2,3]*phi[1,3] +
                beta[4,1]*beta[5,4]*gamma[1,1]*gamma[2,3]*phi[1,3] +
                beta[3,1]*beta[4,3]*beta[5,4]*gamma[1,1]*gamma[2,3]*phi[1,3] +
                beta[5,3]*gamma[2,3]*gamma[3,1]*phi[1,3] +
                beta[4,3]*beta[5,4]*gamma[2,3]*gamma[3,1]*phi[1,3] +
                beta[5,3]*gamma[2,1]*gamma[3,3]*phi[1,3] +
                beta[4,3]*beta[5,4]*gamma[2,1]*gamma[3,3]*phi[1,3] +
                beta[5,4]*gamma[2,3]*gamma[4,1]*phi[1,3] + beta[5,4]*gamma[2,1]*gamma[4,3]*phi[1,3] +
                gamma[2,3]*gamma[5,1]*phi[1,3] + gamma[2,1]*gamma[5,3]*phi[1,3] +
                beta[3,1]*beta[5,3]*gamma[1,3]*gamma[2,2]*phi[2,3] +
                beta[4,1]*beta[5,4]*gamma[1,3]*gamma[2,2]*phi[2,3] +
                beta[3,1]*beta[4,3]*beta[5,4]*gamma[1,3]*gamma[2,2]*phi[2,3] +
                beta[3,1]*beta[5,3]*gamma[1,2]*gamma[2,3]*phi[2,3] +
                beta[4,1]*beta[5,4]*gamma[1,2]*gamma[2,3]*phi[2,3] +
                beta[3,1]*beta[4,3]*beta[5,4]*gamma[1,2]*gamma[2,3]*phi[2,3] +
                beta[5,3]*gamma[2,3]*gamma[3,2]*phi[2,3] +
                beta[4,3]*beta[5,4]*gamma[2,3]*gamma[3,2]*phi[2,3] +
                beta[5,3]*gamma[2,2]*gamma[3,3]*phi[2,3] +
                beta[4,3]*beta[5,4]*gamma[2,2]*gamma[3,3]*phi[2,3] +
                beta[5,4]*gamma[2,3]*gamma[4,2]*phi[2,3] + beta[5,4]*gamma[2,2]*gamma[4,3]*phi[2,3] +
                gamma[2,3]*gamma[5,2]*phi[2,3] + gamma[2,2]*gamma[5,3]*phi[2,3] +
                beta[2,1]*(beta[3,1]*beta[5,3] + beta[4,1]*beta[5,4] + beta[3,1]*beta[4,3]*beta[5,4] +
                          beta[5,3]*gamma[1,1]*gamma[3,1] + beta[4,3]*beta[5,4]*gamma[1,1]*gamma[3,1] +
                          beta[5,3]*gamma[1,2]*gamma[3,2] + beta[4,3]*beta[5,4]*gamma[1,2]*gamma[3,2] +
                          beta[5,3]*gamma[1,3]*gamma[3,3] + beta[4,3]*beta[5,4]*gamma[1,3]*gamma[3,3] +
                          beta[5,4]*gamma[1,1]*gamma[4,1] + beta[5,4]*gamma[1,2]*gamma[4,2] +
                          beta[5,4]*gamma[1,3]*gamma[4,3] + gamma[1,1]*gamma[5,1] + gamma[1,2]*gamma[5,2] +
                          gamma[1,3]*gamma[5,3] + beta[5,3]*gamma[1,2]*gamma[3,1]*phi[1,2] +
                          beta[4,3]*beta[5,4]*gamma[1,2]*gamma[3,1]*phi[1,2] +
                          beta[5,3]*gamma[1,1]*gamma[3,2]*phi[1,2] +
                          beta[4,3]*beta[5,4]*gamma[1,1]*gamma[3,2]*phi[1,2] +
                          beta[5,4]*gamma[1,2]*gamma[4,1]*phi[1,2] + beta[5,4]*gamma[1,1]*gamma[4,2]*phi[1,2] +
                          gamma[1,2]*gamma[5,1]*phi[1,2] + gamma[1,1]*gamma[5,2]*phi[1,2] +
                          beta[5,3]*gamma[1,3]*gamma[3,1]*phi[1,3] +
                          beta[4,3]*beta[5,4]*gamma[1,3]*gamma[3,1]*phi[1,3] +
                          beta[5,3]*gamma[1,1]*gamma[3,3]*phi[1,3] +
                          beta[4,3]*beta[5,4]*gamma[1,1]*gamma[3,3]*phi[1,3] +
                          beta[5,4]*gamma[1,3]*gamma[4,1]*phi[1,3] + beta[5,4]*gamma[1,1]*gamma[4,3]*phi[1,3] +
                          gamma[1,3]*gamma[5,1]*phi[1,3] + gamma[1,1]*gamma[5,3]*phi[1,3] +
                          beta[5,3]*gamma[1,3]*gamma[3,2]*phi[2,3] +
                          beta[4,3]*beta[5,4]*gamma[1,3]*gamma[3,2]*phi[2,3] +
                          beta[5,3]*gamma[1,2]*gamma[3,3]*phi[2,3] +
                          beta[4,3]*beta[5,4]*gamma[1,2]*gamma[3,3]*phi[2,3] +
                          beta[5,4]*gamma[1,3]*gamma[4,2]*phi[2,3] + beta[5,4]*gamma[1,2]*gamma[4,3]*phi[2,3] +
                          gamma[1,3]*gamma[5,2]*phi[2,3] + gamma[1,2]*gamma[5,3]*phi[2,3]))
}

varzeta6 <- function(beta, gamma, phi){
  1 - beta[6,1]^2 - beta[6,2]^2 - beta[6,3]^2 -2*beta[3,1]*beta[4,1]*beta[6,3]*beta[6,4] -
   2*beta[2,1]*beta[3,2]*beta[4,1]*beta[6,3]*beta[6,4] -
   2*beta[2,1]*beta[3,1]*beta[4,2]*beta[6,3]*beta[6,4] -
   2*beta[3,2]*beta[4,2]*beta[6,3]*beta[6,4] -2*beta[4,3]*beta[6,3]*beta[6,4] - beta[6,4]^2 -
   2*beta[3,1]*beta[5,1]*beta[6,3]*beta[6,5] -
   2*beta[2,1]*beta[3,2]*beta[5,1]*beta[6,3]*beta[6,5] -
   2*beta[2,1]*beta[3,1]*beta[5,2]*beta[6,3]*beta[6,5] -
   2*beta[3,2]*beta[5,2]*beta[6,3]*beta[6,5] -2*beta[5,3]*beta[6,3]*beta[6,5] -
   2*beta[3,1]*beta[4,1]*beta[5,4]*beta[6,3]*beta[6,5] -
   2*beta[2,1]*beta[3,2]*beta[4,1]*beta[5,4]*beta[6,3]*beta[6,5] -
   2*beta[2,1]*beta[3,1]*beta[4,2]*beta[5,4]*beta[6,3]*beta[6,5] -
   2*beta[3,2]*beta[4,2]*beta[5,4]*beta[6,3]*beta[6,5] -
   2*beta[4,3]*beta[5,4]*beta[6,3]*beta[6,5] -2*beta[4,1]*beta[5,1]*beta[6,4]*beta[6,5] -
   2*beta[2,1]*beta[4,2]*beta[5,1]*beta[6,4]*beta[6,5] -
   2*beta[3,1]*beta[4,3]*beta[5,1]*beta[6,4]*beta[6,5] -
   2*beta[2,1]*beta[3,2]*beta[4,3]*beta[5,1]*beta[6,4]*beta[6,5] -
   2*beta[2,1]*beta[4,1]*beta[5,2]*beta[6,4]*beta[6,5] -
   2*beta[4,2]*beta[5,2]*beta[6,4]*beta[6,5] -
   2*beta[2,1]*beta[3,1]*beta[4,3]*beta[5,2]*beta[6,4]*beta[6,5] -
   2*beta[3,2]*beta[4,3]*beta[5,2]*beta[6,4]*beta[6,5] -
   2*beta[3,1]*beta[4,1]*beta[5,3]*beta[6,4]*beta[6,5] -
   2*beta[2,1]*beta[3,2]*beta[4,1]*beta[5,3]*beta[6,4]*beta[6,5] -
   2*beta[2,1]*beta[3,1]*beta[4,2]*beta[5,3]*beta[6,4]*beta[6,5] -
   2*beta[3,2]*beta[4,2]*beta[5,3]*beta[6,4]*beta[6,5] -
   2*beta[4,3]*beta[5,3]*beta[6,4]*beta[6,5] -2*beta[5,4]*beta[6,4]*beta[6,5] - beta[6,5]^2 -
   2*beta[3,2]*beta[4,1]*beta[6,3]*beta[6,4]*gamma[1,1]*gamma[2,1] -
   2*beta[3,1]*beta[4,2]*beta[6,3]*beta[6,4]*gamma[1,1]*gamma[2,1] -
   2*beta[3,2]*beta[5,1]*beta[6,3]*beta[6,5]*gamma[1,1]*gamma[2,1] -
   2*beta[3,1]*beta[5,2]*beta[6,3]*beta[6,5]*gamma[1,1]*gamma[2,1] -
   2*beta[3,2]*beta[4,1]*beta[5,4]*beta[6,3]*beta[6,5]*gamma[1,1]*gamma[2,1] -
   2*beta[3,1]*beta[4,2]*beta[5,4]*beta[6,3]*beta[6,5]*gamma[1,1]*gamma[2,1] -
   2*beta[4,2]*beta[5,1]*beta[6,4]*beta[6,5]*gamma[1,1]*gamma[2,1] -
   2*beta[3,2]*beta[4,3]*beta[5,1]*beta[6,4]*beta[6,5]*gamma[1,1]*gamma[2,1] -
   2*beta[4,1]*beta[5,2]*beta[6,4]*beta[6,5]*gamma[1,1]*gamma[2,1] -
   2*beta[3,1]*beta[4,3]*beta[5,2]*beta[6,4]*beta[6,5]*gamma[1,1]*gamma[2,1] -
   2*beta[3,2]*beta[4,1]*beta[5,3]*beta[6,4]*beta[6,5]*gamma[1,1]*gamma[2,1] -
   2*beta[3,1]*beta[4,2]*beta[5,3]*beta[6,4]*beta[6,5]*gamma[1,1]*gamma[2,1] -
   2*beta[3,2]*beta[4,1]*beta[6,3]*beta[6,4]*gamma[1,2]*gamma[2,2] -
   2*beta[3,1]*beta[4,2]*beta[6,3]*beta[6,4]*gamma[1,2]*gamma[2,2] -
   2*beta[3,2]*beta[5,1]*beta[6,3]*beta[6,5]*gamma[1,2]*gamma[2,2] -
   2*beta[3,1]*beta[5,2]*beta[6,3]*beta[6,5]*gamma[1,2]*gamma[2,2] -
   2*beta[3,2]*beta[4,1]*beta[5,4]*beta[6,3]*beta[6,5]*gamma[1,2]*gamma[2,2] -
   2*beta[3,1]*beta[4,2]*beta[5,4]*beta[6,3]*beta[6,5]*gamma[1,2]*gamma[2,2] -
   2*beta[4,2]*beta[5,1]*beta[6,4]*beta[6,5]*gamma[1,2]*gamma[2,2] -
   2*beta[3,2]*beta[4,3]*beta[5,1]*beta[6,4]*beta[6,5]*gamma[1,2]*gamma[2,2] -
   2*beta[4,1]*beta[5,2]*beta[6,4]*beta[6,5]*gamma[1,2]*gamma[2,2] -
   2*beta[3,1]*beta[4,3]*beta[5,2]*beta[6,4]*beta[6,5]*gamma[1,2]*gamma[2,2] -
   2*beta[3,2]*beta[4,1]*beta[5,3]*beta[6,4]*beta[6,5]*gamma[1,2]*gamma[2,2] -
   2*beta[3,1]*beta[4,2]*beta[5,3]*beta[6,4]*beta[6,5]*gamma[1,2]*gamma[2,2] -
   2*beta[4,1]*beta[6,3]*beta[6,4]*gamma[1,1]*gamma[3,1] -
   2*beta[2,1]*beta[4,2]*beta[6,3]*beta[6,4]*gamma[1,1]*gamma[3,1] -
   2*beta[5,1]*beta[6,3]*beta[6,5]*gamma[1,1]*gamma[3,1] -
   2*beta[2,1]*beta[5,2]*beta[6,3]*beta[6,5]*gamma[1,1]*gamma[3,1] -
   2*beta[4,1]*beta[5,4]*beta[6,3]*beta[6,5]*gamma[1,1]*gamma[3,1] -
   2*beta[2,1]*beta[4,2]*beta[5,4]*beta[6,3]*beta[6,5]*gamma[1,1]*gamma[3,1] -
   2*beta[4,3]*beta[5,1]*beta[6,4]*beta[6,5]*gamma[1,1]*gamma[3,1] -
   2*beta[2,1]*beta[4,3]*beta[5,2]*beta[6,4]*beta[6,5]*gamma[1,1]*gamma[3,1] -
   2*beta[4,1]*beta[5,3]*beta[6,4]*beta[6,5]*gamma[1,1]*gamma[3,1] -
   2*beta[2,1]*beta[4,2]*beta[5,3]*beta[6,4]*beta[6,5]*gamma[1,1]*gamma[3,1] -
   2*beta[4,2]*beta[6,3]*beta[6,4]*gamma[2,1]*gamma[3,1] -
   2*beta[5,2]*beta[6,3]*beta[6,5]*gamma[2,1]*gamma[3,1] -
   2*beta[4,2]*beta[5,4]*beta[6,3]*beta[6,5]*gamma[2,1]*gamma[3,1] -
   2*beta[4,3]*beta[5,2]*beta[6,4]*beta[6,5]*gamma[2,1]*gamma[3,1] -
   2*beta[4,2]*beta[5,3]*beta[6,4]*beta[6,5]*gamma[2,1]*gamma[3,1] -
   2*beta[4,1]*beta[6,3]*beta[6,4]*gamma[1,2]*gamma[3,2] -
   2*beta[2,1]*beta[4,2]*beta[6,3]*beta[6,4]*gamma[1,2]*gamma[3,2] -
   2*beta[5,1]*beta[6,3]*beta[6,5]*gamma[1,2]*gamma[3,2] -
   2*beta[2,1]*beta[5,2]*beta[6,3]*beta[6,5]*gamma[1,2]*gamma[3,2] -
   2*beta[4,1]*beta[5,4]*beta[6,3]*beta[6,5]*gamma[1,2]*gamma[3,2] -
   2*beta[2,1]*beta[4,2]*beta[5,4]*beta[6,3]*beta[6,5]*gamma[1,2]*gamma[3,2] -
   2*beta[4,3]*beta[5,1]*beta[6,4]*beta[6,5]*gamma[1,2]*gamma[3,2] -
   2*beta[2,1]*beta[4,3]*beta[5,2]*beta[6,4]*beta[6,5]*gamma[1,2]*gamma[3,2] -
   2*beta[4,1]*beta[5,3]*beta[6,4]*beta[6,5]*gamma[1,2]*gamma[3,2] -
   2*beta[2,1]*beta[4,2]*beta[5,3]*beta[6,4]*beta[6,5]*gamma[1,2]*gamma[3,2] -
   2*beta[4,2]*beta[6,3]*beta[6,4]*gamma[2,2]*gamma[3,2] -
   2*beta[5,2]*beta[6,3]*beta[6,5]*gamma[2,2]*gamma[3,2] -
   2*beta[4,2]*beta[5,4]*beta[6,3]*beta[6,5]*gamma[2,2]*gamma[3,2] -
   2*beta[4,3]*beta[5,2]*beta[6,4]*beta[6,5]*gamma[2,2]*gamma[3,2] -
   2*beta[4,2]*beta[5,3]*beta[6,4]*beta[6,5]*gamma[2,2]*gamma[3,2] -
   2*beta[3,1]*beta[6,3]*beta[6,4]*gamma[1,1]*gamma[4,1] -
   2*beta[2,1]*beta[3,2]*beta[6,3]*beta[6,4]*gamma[1,1]*gamma[4,1] -
   2*beta[3,1]*beta[5,4]*beta[6,3]*beta[6,5]*gamma[1,1]*gamma[4,1] -
   2*beta[2,1]*beta[3,2]*beta[5,4]*beta[6,3]*beta[6,5]*gamma[1,1]*gamma[4,1] -
   2*beta[5,1]*beta[6,4]*beta[6,5]*gamma[1,1]*gamma[4,1] -
   2*beta[2,1]*beta[5,2]*beta[6,4]*beta[6,5]*gamma[1,1]*gamma[4,1] -
   2*beta[3,1]*beta[5,3]*beta[6,4]*beta[6,5]*gamma[1,1]*gamma[4,1] -
   2*beta[2,1]*beta[3,2]*beta[5,3]*beta[6,4]*beta[6,5]*gamma[1,1]*gamma[4,1] -
   2*beta[3,2]*beta[6,3]*beta[6,4]*gamma[2,1]*gamma[4,1] -
   2*beta[3,2]*beta[5,4]*beta[6,3]*beta[6,5]*gamma[2,1]*gamma[4,1] -
   2*beta[5,2]*beta[6,4]*beta[6,5]*gamma[2,1]*gamma[4,1] -
   2*beta[3,2]*beta[5,3]*beta[6,4]*beta[6,5]*gamma[2,1]*gamma[4,1] -
   2*beta[6,3]*beta[6,4]*gamma[3,1]*gamma[4,1] -
   2*beta[5,4]*beta[6,3]*beta[6,5]*gamma[3,1]*gamma[4,1] -
   2*beta[5,3]*beta[6,4]*beta[6,5]*gamma[3,1]*gamma[4,1] -
   2*beta[3,1]*beta[6,3]*beta[6,4]*gamma[1,2]*gamma[4,2] -
   2*beta[2,1]*beta[3,2]*beta[6,3]*beta[6,4]*gamma[1,2]*gamma[4,2] -
   2*beta[3,1]*beta[5,4]*beta[6,3]*beta[6,5]*gamma[1,2]*gamma[4,2] -
   2*beta[2,1]*beta[3,2]*beta[5,4]*beta[6,3]*beta[6,5]*gamma[1,2]*gamma[4,2] -
   2*beta[5,1]*beta[6,4]*beta[6,5]*gamma[1,2]*gamma[4,2] -
   2*beta[2,1]*beta[5,2]*beta[6,4]*beta[6,5]*gamma[1,2]*gamma[4,2] -
   2*beta[3,1]*beta[5,3]*beta[6,4]*beta[6,5]*gamma[1,2]*gamma[4,2] -
   2*beta[2,1]*beta[3,2]*beta[5,3]*beta[6,4]*beta[6,5]*gamma[1,2]*gamma[4,2] -
   2*beta[3,2]*beta[6,3]*beta[6,4]*gamma[2,2]*gamma[4,2] -
   2*beta[3,2]*beta[5,4]*beta[6,3]*beta[6,5]*gamma[2,2]*gamma[4,2] -
   2*beta[5,2]*beta[6,4]*beta[6,5]*gamma[2,2]*gamma[4,2] -
   2*beta[3,2]*beta[5,3]*beta[6,4]*beta[6,5]*gamma[2,2]*gamma[4,2] -
   2*beta[6,3]*beta[6,4]*gamma[3,2]*gamma[4,2] -
   2*beta[5,4]*beta[6,3]*beta[6,5]*gamma[3,2]*gamma[4,2] -
   2*beta[5,3]*beta[6,4]*beta[6,5]*gamma[3,2]*gamma[4,2] -
   2*beta[3,1]*beta[6,3]*beta[6,5]*gamma[1,1]*gamma[5,1] -
   2*beta[2,1]*beta[3,2]*beta[6,3]*beta[6,5]*gamma[1,1]*gamma[5,1] -
   2*beta[4,1]*beta[6,4]*beta[6,5]*gamma[1,1]*gamma[5,1] -
   2*beta[2,1]*beta[4,2]*beta[6,4]*beta[6,5]*gamma[1,1]*gamma[5,1] -
   2*beta[3,1]*beta[4,3]*beta[6,4]*beta[6,5]*gamma[1,1]*gamma[5,1] -
   2*beta[2,1]*beta[3,2]*beta[4,3]*beta[6,4]*beta[6,5]*gamma[1,1]*gamma[5,1] -
   2*beta[3,2]*beta[6,3]*beta[6,5]*gamma[2,1]*gamma[5,1] -
   2*beta[4,2]*beta[6,4]*beta[6,5]*gamma[2,1]*gamma[5,1] -
   2*beta[3,2]*beta[4,3]*beta[6,4]*beta[6,5]*gamma[2,1]*gamma[5,1] -
   2*beta[6,3]*beta[6,5]*gamma[3,1]*gamma[5,1] -
   2*beta[4,3]*beta[6,4]*beta[6,5]*gamma[3,1]*gamma[5,1] -
   2*beta[6,4]*beta[6,5]*gamma[4,1]*gamma[5,1] -
   2*beta[3,1]*beta[6,3]*beta[6,5]*gamma[1,2]*gamma[5,2] -
   2*beta[2,1]*beta[3,2]*beta[6,3]*beta[6,5]*gamma[1,2]*gamma[5,2] -
   2*beta[4,1]*beta[6,4]*beta[6,5]*gamma[1,2]*gamma[5,2] -
   2*beta[2,1]*beta[4,2]*beta[6,4]*beta[6,5]*gamma[1,2]*gamma[5,2] -
   2*beta[3,1]*beta[4,3]*beta[6,4]*beta[6,5]*gamma[1,2]*gamma[5,2] -
   2*beta[2,1]*beta[3,2]*beta[4,3]*beta[6,4]*beta[6,5]*gamma[1,2]*gamma[5,2] -
   2*beta[3,2]*beta[6,3]*beta[6,5]*gamma[2,2]*gamma[5,2] -
   2*beta[4,2]*beta[6,4]*beta[6,5]*gamma[2,2]*gamma[5,2] -
   2*beta[3,2]*beta[4,3]*beta[6,4]*beta[6,5]*gamma[2,2]*gamma[5,2] -
   2*beta[6,3]*beta[6,5]*gamma[3,2]*gamma[5,2] -
   2*beta[4,3]*beta[6,4]*beta[6,5]*gamma[3,2]*gamma[5,2] -
   2*beta[6,4]*beta[6,5]*gamma[4,2]*gamma[5,2] -2*beta[3,1]*beta[6,3]*gamma[1,1]*gamma[6,1] -
   2*beta[2,1]*beta[3,2]*beta[6,3]*gamma[1,1]*gamma[6,1] -
   2*beta[4,1]*beta[6,4]*gamma[1,1]*gamma[6,1] -
   2*beta[2,1]*beta[4,2]*beta[6,4]*gamma[1,1]*gamma[6,1] -
   2*beta[3,1]*beta[4,3]*beta[6,4]*gamma[1,1]*gamma[6,1] -
   2*beta[2,1]*beta[3,2]*beta[4,3]*beta[6,4]*gamma[1,1]*gamma[6,1] -
   2*beta[5,1]*beta[6,5]*gamma[1,1]*gamma[6,1] -
   2*beta[2,1]*beta[5,2]*beta[6,5]*gamma[1,1]*gamma[6,1] -
   2*beta[3,1]*beta[5,3]*beta[6,5]*gamma[1,1]*gamma[6,1] -
   2*beta[2,1]*beta[3,2]*beta[5,3]*beta[6,5]*gamma[1,1]*gamma[6,1] -
   2*beta[4,1]*beta[5,4]*beta[6,5]*gamma[1,1]*gamma[6,1] -
   2*beta[2,1]*beta[4,2]*beta[5,4]*beta[6,5]*gamma[1,1]*gamma[6,1] -
   2*beta[3,1]*beta[4,3]*beta[5,4]*beta[6,5]*gamma[1,1]*gamma[6,1] -
   2*beta[2,1]*beta[3,2]*beta[4,3]*beta[5,4]*beta[6,5]*gamma[1,1]*gamma[6,1] -
   2*beta[3,2]*beta[6,3]*gamma[2,1]*gamma[6,1] -2*beta[4,2]*beta[6,4]*gamma[2,1]*gamma[6,1] -
   2*beta[3,2]*beta[4,3]*beta[6,4]*gamma[2,1]*gamma[6,1] -
   2*beta[5,2]*beta[6,5]*gamma[2,1]*gamma[6,1] -
   2*beta[3,2]*beta[5,3]*beta[6,5]*gamma[2,1]*gamma[6,1] -
   2*beta[4,2]*beta[5,4]*beta[6,5]*gamma[2,1]*gamma[6,1] -
   2*beta[3,2]*beta[4,3]*beta[5,4]*beta[6,5]*gamma[2,1]*gamma[6,1] -
   2*beta[6,3]*gamma[3,1]*gamma[6,1] -2*beta[4,3]*beta[6,4]*gamma[3,1]*gamma[6,1] -
   2*beta[5,3]*beta[6,5]*gamma[3,1]*gamma[6,1] -
   2*beta[4,3]*beta[5,4]*beta[6,5]*gamma[3,1]*gamma[6,1] -2*beta[6,4]*gamma[4,1]*gamma[6,1] -
   2*beta[5,4]*beta[6,5]*gamma[4,1]*gamma[6,1] -
   2*beta[6,5]*gamma[5,1]*gamma[6,1] - gamma[6,1]^2 -
   2*beta[3,1]*beta[6,3]*gamma[1,2]*gamma[6,2] -
   2*beta[2,1]*beta[3,2]*beta[6,3]*gamma[1,2]*gamma[6,2] -
   2*beta[4,1]*beta[6,4]*gamma[1,2]*gamma[6,2] -
   2*beta[2,1]*beta[4,2]*beta[6,4]*gamma[1,2]*gamma[6,2] -
   2*beta[3,1]*beta[4,3]*beta[6,4]*gamma[1,2]*gamma[6,2] -
   2*beta[2,1]*beta[3,2]*beta[4,3]*beta[6,4]*gamma[1,2]*gamma[6,2] -
   2*beta[5,1]*beta[6,5]*gamma[1,2]*gamma[6,2] -
   2*beta[2,1]*beta[5,2]*beta[6,5]*gamma[1,2]*gamma[6,2] -
   2*beta[3,1]*beta[5,3]*beta[6,5]*gamma[1,2]*gamma[6,2] -
   2*beta[2,1]*beta[3,2]*beta[5,3]*beta[6,5]*gamma[1,2]*gamma[6,2] -
   2*beta[4,1]*beta[5,4]*beta[6,5]*gamma[1,2]*gamma[6,2] -
   2*beta[2,1]*beta[4,2]*beta[5,4]*beta[6,5]*gamma[1,2]*gamma[6,2] -
   2*beta[3,1]*beta[4,3]*beta[5,4]*beta[6,5]*gamma[1,2]*gamma[6,2] -
   2*beta[2,1]*beta[3,2]*beta[4,3]*beta[5,4]*beta[6,5]*gamma[1,2]*gamma[6,2] -
   2*beta[3,2]*beta[6,3]*gamma[2,2]*gamma[6,2] -2*beta[4,2]*beta[6,4]*gamma[2,2]*gamma[6,2] -
   2*beta[3,2]*beta[4,3]*beta[6,4]*gamma[2,2]*gamma[6,2] -
   2*beta[5,2]*beta[6,5]*gamma[2,2]*gamma[6,2] -
   2*beta[3,2]*beta[5,3]*beta[6,5]*gamma[2,2]*gamma[6,2] -
   2*beta[4,2]*beta[5,4]*beta[6,5]*gamma[2,2]*gamma[6,2] -
   2*beta[3,2]*beta[4,3]*beta[5,4]*beta[6,5]*gamma[2,2]*gamma[6,2] -
   2*beta[6,3]*gamma[3,2]*gamma[6,2] -2*beta[4,3]*beta[6,4]*gamma[3,2]*gamma[6,2] -
   2*beta[5,3]*beta[6,5]*gamma[3,2]*gamma[6,2] -
   2*beta[4,3]*beta[5,4]*beta[6,5]*gamma[3,2]*gamma[6,2] -2*beta[6,4]*gamma[4,2]*gamma[6,2] -
   2*beta[5,4]*beta[6,5]*gamma[4,2]*gamma[6,2] -
   2*beta[6,5]*gamma[5,2]*gamma[6,2] - gamma[6,2]^2 -
   2*beta[3,2]*beta[4,1]*beta[6,3]*beta[6,4]*gamma[1,2]*gamma[2,1]*phi[1,2] -
   2*beta[3,1]*beta[4,2]*beta[6,3]*beta[6,4]*gamma[1,2]*gamma[2,1]*phi[1,2] -
   2*beta[3,2]*beta[5,1]*beta[6,3]*beta[6,5]*gamma[1,2]*gamma[2,1]*phi[1,2] -
   2*beta[3,1]*beta[5,2]*beta[6,3]*beta[6,5]*gamma[1,2]*gamma[2,1]*phi[1,2] -
   2*beta[3,2]*beta[4,1]*beta[5,4]*beta[6,3]*beta[6,5]*gamma[1,2]*gamma[2,1]*phi[1,2] -
   2*beta[3,1]*beta[4,2]*beta[5,4]*beta[6,3]*beta[6,5]*gamma[1,2]*gamma[2,1]*phi[1,2] -
   2*beta[4,2]*beta[5,1]*beta[6,4]*beta[6,5]*gamma[1,2]*gamma[2,1]*phi[1,2] -
   2*beta[3,2]*beta[4,3]*beta[5,1]*beta[6,4]*beta[6,5]*gamma[1,2]*gamma[2,1]*phi[1,2] -
   2*beta[4,1]*beta[5,2]*beta[6,4]*beta[6,5]*gamma[1,2]*gamma[2,1]*phi[1,2] -
   2*beta[3,1]*beta[4,3]*beta[5,2]*beta[6,4]*beta[6,5]*gamma[1,2]*gamma[2,1]*phi[1,2] -
   2*beta[3,2]*beta[4,1]*beta[5,3]*beta[6,4]*beta[6,5]*gamma[1,2]*gamma[2,1]*phi[1,2] -
   2*beta[3,1]*beta[4,2]*beta[5,3]*beta[6,4]*beta[6,5]*gamma[1,2]*gamma[2,1]*phi[1,2] -
   2*beta[3,2]*beta[4,1]*beta[6,3]*beta[6,4]*gamma[1,1]*gamma[2,2]*phi[1,2] -
   2*beta[3,1]*beta[4,2]*beta[6,3]*beta[6,4]*gamma[1,1]*gamma[2,2]*phi[1,2] -
   2*beta[3,2]*beta[5,1]*beta[6,3]*beta[6,5]*gamma[1,1]*gamma[2,2]*phi[1,2] -
   2*beta[3,1]*beta[5,2]*beta[6,3]*beta[6,5]*gamma[1,1]*gamma[2,2]*phi[1,2] -
   2*beta[3,2]*beta[4,1]*beta[5,4]*beta[6,3]*beta[6,5]*gamma[1,1]*gamma[2,2]*phi[1,2] -
   2*beta[3,1]*beta[4,2]*beta[5,4]*beta[6,3]*beta[6,5]*gamma[1,1]*gamma[2,2]*phi[1,2] -
   2*beta[4,2]*beta[5,1]*beta[6,4]*beta[6,5]*gamma[1,1]*gamma[2,2]*phi[1,2] -
   2*beta[3,2]*beta[4,3]*beta[5,1]*beta[6,4]*beta[6,5]*gamma[1,1]*gamma[2,2]*phi[1,2] -
   2*beta[4,1]*beta[5,2]*beta[6,4]*beta[6,5]*gamma[1,1]*gamma[2,2]*phi[1,2] -
   2*beta[3,1]*beta[4,3]*beta[5,2]*beta[6,4]*beta[6,5]*gamma[1,1]*gamma[2,2]*phi[1,2] -
   2*beta[3,2]*beta[4,1]*beta[5,3]*beta[6,4]*beta[6,5]*gamma[1,1]*gamma[2,2]*phi[1,2] -
   2*beta[3,1]*beta[4,2]*beta[5,3]*beta[6,4]*beta[6,5]*gamma[1,1]*gamma[2,2]*phi[1,2] -
   2*beta[4,1]*beta[6,3]*beta[6,4]*gamma[1,2]*gamma[3,1]*phi[1,2] -
   2*beta[2,1]*beta[4,2]*beta[6,3]*beta[6,4]*gamma[1,2]*gamma[3,1]*phi[1,2] -
   2*beta[5,1]*beta[6,3]*beta[6,5]*gamma[1,2]*gamma[3,1]*phi[1,2] -
   2*beta[2,1]*beta[5,2]*beta[6,3]*beta[6,5]*gamma[1,2]*gamma[3,1]*phi[1,2] -
   2*beta[4,1]*beta[5,4]*beta[6,3]*beta[6,5]*gamma[1,2]*gamma[3,1]*phi[1,2] -
   2*beta[2,1]*beta[4,2]*beta[5,4]*beta[6,3]*beta[6,5]*gamma[1,2]*gamma[3,1]*phi[1,2] -
   2*beta[4,3]*beta[5,1]*beta[6,4]*beta[6,5]*gamma[1,2]*gamma[3,1]*phi[1,2] -
   2*beta[2,1]*beta[4,3]*beta[5,2]*beta[6,4]*beta[6,5]*gamma[1,2]*gamma[3,1]*phi[1,2] -
   2*beta[4,1]*beta[5,3]*beta[6,4]*beta[6,5]*gamma[1,2]*gamma[3,1]*phi[1,2] -
   2*beta[2,1]*beta[4,2]*beta[5,3]*beta[6,4]*beta[6,5]*gamma[1,2]*gamma[3,1]*phi[1,2] -
   2*beta[4,2]*beta[6,3]*beta[6,4]*gamma[2,2]*gamma[3,1]*phi[1,2] -
   2*beta[5,2]*beta[6,3]*beta[6,5]*gamma[2,2]*gamma[3,1]*phi[1,2] -
   2*beta[4,2]*beta[5,4]*beta[6,3]*beta[6,5]*gamma[2,2]*gamma[3,1]*phi[1,2] -
   2*beta[4,3]*beta[5,2]*beta[6,4]*beta[6,5]*gamma[2,2]*gamma[3,1]*phi[1,2] -
   2*beta[4,2]*beta[5,3]*beta[6,4]*beta[6,5]*gamma[2,2]*gamma[3,1]*phi[1,2] -
   2*beta[4,1]*beta[6,3]*beta[6,4]*gamma[1,1]*gamma[3,2]*phi[1,2] -
   2*beta[2,1]*beta[4,2]*beta[6,3]*beta[6,4]*gamma[1,1]*gamma[3,2]*phi[1,2] -
   2*beta[5,1]*beta[6,3]*beta[6,5]*gamma[1,1]*gamma[3,2]*phi[1,2] -
   2*beta[2,1]*beta[5,2]*beta[6,3]*beta[6,5]*gamma[1,1]*gamma[3,2]*phi[1,2] -
   2*beta[4,1]*beta[5,4]*beta[6,3]*beta[6,5]*gamma[1,1]*gamma[3,2]*phi[1,2] -
   2*beta[2,1]*beta[4,2]*beta[5,4]*beta[6,3]*beta[6,5]*gamma[1,1]*gamma[3,2]*phi[1,2] -
   2*beta[4,3]*beta[5,1]*beta[6,4]*beta[6,5]*gamma[1,1]*gamma[3,2]*phi[1,2] -
   2*beta[2,1]*beta[4,3]*beta[5,2]*beta[6,4]*beta[6,5]*gamma[1,1]*gamma[3,2]*phi[1,2] -
   2*beta[4,1]*beta[5,3]*beta[6,4]*beta[6,5]*gamma[1,1]*gamma[3,2]*phi[1,2] -
   2*beta[2,1]*beta[4,2]*beta[5,3]*beta[6,4]*beta[6,5]*gamma[1,1]*gamma[3,2]*phi[1,2] -
   2*beta[4,2]*beta[6,3]*beta[6,4]*gamma[2,1]*gamma[3,2]*phi[1,2] -
   2*beta[5,2]*beta[6,3]*beta[6,5]*gamma[2,1]*gamma[3,2]*phi[1,2] -
   2*beta[4,2]*beta[5,4]*beta[6,3]*beta[6,5]*gamma[2,1]*gamma[3,2]*phi[1,2] -
   2*beta[4,3]*beta[5,2]*beta[6,4]*beta[6,5]*gamma[2,1]*gamma[3,2]*phi[1,2] -
   2*beta[4,2]*beta[5,3]*beta[6,4]*beta[6,5]*gamma[2,1]*gamma[3,2]*phi[1,2] -
   2*beta[3,1]*beta[6,3]*beta[6,4]*gamma[1,2]*gamma[4,1]*phi[1,2] -
   2*beta[2,1]*beta[3,2]*beta[6,3]*beta[6,4]*gamma[1,2]*gamma[4,1]*phi[1,2] -
   2*beta[3,1]*beta[5,4]*beta[6,3]*beta[6,5]*gamma[1,2]*gamma[4,1]*phi[1,2] -
   2*beta[2,1]*beta[3,2]*beta[5,4]*beta[6,3]*beta[6,5]*gamma[1,2]*gamma[4,1]*phi[1,2] -
   2*beta[5,1]*beta[6,4]*beta[6,5]*gamma[1,2]*gamma[4,1]*phi[1,2] -
   2*beta[2,1]*beta[5,2]*beta[6,4]*beta[6,5]*gamma[1,2]*gamma[4,1]*phi[1,2] -
   2*beta[3,1]*beta[5,3]*beta[6,4]*beta[6,5]*gamma[1,2]*gamma[4,1]*phi[1,2] -
   2*beta[2,1]*beta[3,2]*beta[5,3]*beta[6,4]*beta[6,5]*gamma[1,2]*gamma[4,1]*phi[1,2] -
   2*beta[3,2]*beta[6,3]*beta[6,4]*gamma[2,2]*gamma[4,1]*phi[1,2] -
   2*beta[3,2]*beta[5,4]*beta[6,3]*beta[6,5]*gamma[2,2]*gamma[4,1]*phi[1,2] -
   2*beta[5,2]*beta[6,4]*beta[6,5]*gamma[2,2]*gamma[4,1]*phi[1,2] -
   2*beta[3,2]*beta[5,3]*beta[6,4]*beta[6,5]*gamma[2,2]*gamma[4,1]*phi[1,2] -
   2*beta[6,3]*beta[6,4]*gamma[3,2]*gamma[4,1]*phi[1,2] -
   2*beta[5,4]*beta[6,3]*beta[6,5]*gamma[3,2]*gamma[4,1]*phi[1,2] -
   2*beta[5,3]*beta[6,4]*beta[6,5]*gamma[3,2]*gamma[4,1]*phi[1,2] -
   2*beta[3,1]*beta[6,3]*beta[6,4]*gamma[1,1]*gamma[4,2]*phi[1,2] -
   2*beta[2,1]*beta[3,2]*beta[6,3]*beta[6,4]*gamma[1,1]*gamma[4,2]*phi[1,2] -
   2*beta[3,1]*beta[5,4]*beta[6,3]*beta[6,5]*gamma[1,1]*gamma[4,2]*phi[1,2] -
   2*beta[2,1]*beta[3,2]*beta[5,4]*beta[6,3]*beta[6,5]*gamma[1,1]*gamma[4,2]*phi[1,2] -
   2*beta[5,1]*beta[6,4]*beta[6,5]*gamma[1,1]*gamma[4,2]*phi[1,2] -
   2*beta[2,1]*beta[5,2]*beta[6,4]*beta[6,5]*gamma[1,1]*gamma[4,2]*phi[1,2] -
   2*beta[3,1]*beta[5,3]*beta[6,4]*beta[6,5]*gamma[1,1]*gamma[4,2]*phi[1,2] -
   2*beta[2,1]*beta[3,2]*beta[5,3]*beta[6,4]*beta[6,5]*gamma[1,1]*gamma[4,2]*phi[1,2] -
   2*beta[3,2]*beta[6,3]*beta[6,4]*gamma[2,1]*gamma[4,2]*phi[1,2] -
   2*beta[3,2]*beta[5,4]*beta[6,3]*beta[6,5]*gamma[2,1]*gamma[4,2]*phi[1,2] -
   2*beta[5,2]*beta[6,4]*beta[6,5]*gamma[2,1]*gamma[4,2]*phi[1,2] -
   2*beta[3,2]*beta[5,3]*beta[6,4]*beta[6,5]*gamma[2,1]*gamma[4,2]*phi[1,2] -
   2*beta[6,3]*beta[6,4]*gamma[3,1]*gamma[4,2]*phi[1,2] -
   2*beta[5,4]*beta[6,3]*beta[6,5]*gamma[3,1]*gamma[4,2]*phi[1,2] -
   2*beta[5,3]*beta[6,4]*beta[6,5]*gamma[3,1]*gamma[4,2]*phi[1,2] -
   2*beta[3,1]*beta[6,3]*beta[6,5]*gamma[1,2]*gamma[5,1]*phi[1,2] -
   2*beta[2,1]*beta[3,2]*beta[6,3]*beta[6,5]*gamma[1,2]*gamma[5,1]*phi[1,2] -
   2*beta[4,1]*beta[6,4]*beta[6,5]*gamma[1,2]*gamma[5,1]*phi[1,2] -
   2*beta[2,1]*beta[4,2]*beta[6,4]*beta[6,5]*gamma[1,2]*gamma[5,1]*phi[1,2] -
   2*beta[3,1]*beta[4,3]*beta[6,4]*beta[6,5]*gamma[1,2]*gamma[5,1]*phi[1,2] -
   2*beta[2,1]*beta[3,2]*beta[4,3]*beta[6,4]*beta[6,5]*gamma[1,2]*gamma[5,1]*phi[1,2] -
   2*beta[3,2]*beta[6,3]*beta[6,5]*gamma[2,2]*gamma[5,1]*phi[1,2] -
   2*beta[4,2]*beta[6,4]*beta[6,5]*gamma[2,2]*gamma[5,1]*phi[1,2] -
   2*beta[3,2]*beta[4,3]*beta[6,4]*beta[6,5]*gamma[2,2]*gamma[5,1]*phi[1,2] -
   2*beta[6,3]*beta[6,5]*gamma[3,2]*gamma[5,1]*phi[1,2] -
   2*beta[4,3]*beta[6,4]*beta[6,5]*gamma[3,2]*gamma[5,1]*phi[1,2] -
   2*beta[6,4]*beta[6,5]*gamma[4,2]*gamma[5,1]*phi[1,2] -
   2*beta[3,1]*beta[6,3]*beta[6,5]*gamma[1,1]*gamma[5,2]*phi[1,2] -
   2*beta[2,1]*beta[3,2]*beta[6,3]*beta[6,5]*gamma[1,1]*gamma[5,2]*phi[1,2] -
   2*beta[4,1]*beta[6,4]*beta[6,5]*gamma[1,1]*gamma[5,2]*phi[1,2] -
   2*beta[2,1]*beta[4,2]*beta[6,4]*beta[6,5]*gamma[1,1]*gamma[5,2]*phi[1,2] -
   2*beta[3,1]*beta[4,3]*beta[6,4]*beta[6,5]*gamma[1,1]*gamma[5,2]*phi[1,2] -
   2*beta[2,1]*beta[3,2]*beta[4,3]*beta[6,4]*beta[6,5]*gamma[1,1]*gamma[5,2]*phi[1,2] -
   2*beta[3,2]*beta[6,3]*beta[6,5]*gamma[2,1]*gamma[5,2]*phi[1,2] -
   2*beta[4,2]*beta[6,4]*beta[6,5]*gamma[2,1]*gamma[5,2]*phi[1,2] -
   2*beta[3,2]*beta[4,3]*beta[6,4]*beta[6,5]*gamma[2,1]*gamma[5,2]*phi[1,2] -
   2*beta[6,3]*beta[6,5]*gamma[3,1]*gamma[5,2]*phi[1,2] -
   2*beta[4,3]*beta[6,4]*beta[6,5]*gamma[3,1]*gamma[5,2]*phi[1,2] -
   2*beta[6,4]*beta[6,5]*gamma[4,1]*gamma[5,2]*phi[1,2] -
   2*beta[3,1]*beta[6,3]*gamma[1,2]*gamma[6,1]*phi[1,2] -
   2*beta[2,1]*beta[3,2]*beta[6,3]*gamma[1,2]*gamma[6,1]*phi[1,2] -
   2*beta[4,1]*beta[6,4]*gamma[1,2]*gamma[6,1]*phi[1,2] -
   2*beta[2,1]*beta[4,2]*beta[6,4]*gamma[1,2]*gamma[6,1]*phi[1,2] -
   2*beta[3,1]*beta[4,3]*beta[6,4]*gamma[1,2]*gamma[6,1]*phi[1,2] -
   2*beta[2,1]*beta[3,2]*beta[4,3]*beta[6,4]*gamma[1,2]*gamma[6,1]*phi[1,2] -
   2*beta[5,1]*beta[6,5]*gamma[1,2]*gamma[6,1]*phi[1,2] -
   2*beta[2,1]*beta[5,2]*beta[6,5]*gamma[1,2]*gamma[6,1]*phi[1,2] -
   2*beta[3,1]*beta[5,3]*beta[6,5]*gamma[1,2]*gamma[6,1]*phi[1,2] -
   2*beta[2,1]*beta[3,2]*beta[5,3]*beta[6,5]*gamma[1,2]*gamma[6,1]*phi[1,2] -
   2*beta[4,1]*beta[5,4]*beta[6,5]*gamma[1,2]*gamma[6,1]*phi[1,2] -
   2*beta[2,1]*beta[4,2]*beta[5,4]*beta[6,5]*gamma[1,2]*gamma[6,1]*phi[1,2] -
   2*beta[3,1]*beta[4,3]*beta[5,4]*beta[6,5]*gamma[1,2]*gamma[6,1]*phi[1,2] -
   2*beta[2,1]*beta[3,2]*beta[4,3]*beta[5,4]*beta[6,5]*gamma[1,2]*gamma[6,1]*phi[1,2] -
   2*beta[3,2]*beta[6,3]*gamma[2,2]*gamma[6,1]*phi[1,2] -
   2*beta[4,2]*beta[6,4]*gamma[2,2]*gamma[6,1]*phi[1,2] -
   2*beta[3,2]*beta[4,3]*beta[6,4]*gamma[2,2]*gamma[6,1]*phi[1,2] -
   2*beta[5,2]*beta[6,5]*gamma[2,2]*gamma[6,1]*phi[1,2] -
   2*beta[3,2]*beta[5,3]*beta[6,5]*gamma[2,2]*gamma[6,1]*phi[1,2] -
   2*beta[4,2]*beta[5,4]*beta[6,5]*gamma[2,2]*gamma[6,1]*phi[1,2] -
   2*beta[3,2]*beta[4,3]*beta[5,4]*beta[6,5]*gamma[2,2]*gamma[6,1]*phi[1,2] -
   2*beta[6,3]*gamma[3,2]*gamma[6,1]*phi[1,2] -
   2*beta[4,3]*beta[6,4]*gamma[3,2]*gamma[6,1]*phi[1,2] -
   2*beta[5,3]*beta[6,5]*gamma[3,2]*gamma[6,1]*phi[1,2] -
   2*beta[4,3]*beta[5,4]*beta[6,5]*gamma[3,2]*gamma[6,1]*phi[1,2] -
   2*beta[6,4]*gamma[4,2]*gamma[6,1]*phi[1,2] -
   2*beta[5,4]*beta[6,5]*gamma[4,2]*gamma[6,1]*phi[1,2] -
   2*beta[6,5]*gamma[5,2]*gamma[6,1]*phi[1,2] -
   2*beta[3,1]*beta[6,3]*gamma[1,1]*gamma[6,2]*phi[1,2] -
   2*beta[2,1]*beta[3,2]*beta[6,3]*gamma[1,1]*gamma[6,2]*phi[1,2] -
   2*beta[4,1]*beta[6,4]*gamma[1,1]*gamma[6,2]*phi[1,2] -
   2*beta[2,1]*beta[4,2]*beta[6,4]*gamma[1,1]*gamma[6,2]*phi[1,2] -
   2*beta[3,1]*beta[4,3]*beta[6,4]*gamma[1,1]*gamma[6,2]*phi[1,2] -
   2*beta[2,1]*beta[3,2]*beta[4,3]*beta[6,4]*gamma[1,1]*gamma[6,2]*phi[1,2] -
   2*beta[5,1]*beta[6,5]*gamma[1,1]*gamma[6,2]*phi[1,2] -
   2*beta[2,1]*beta[5,2]*beta[6,5]*gamma[1,1]*gamma[6,2]*phi[1,2] -
   2*beta[3,1]*beta[5,3]*beta[6,5]*gamma[1,1]*gamma[6,2]*phi[1,2] -
   2*beta[2,1]*beta[3,2]*beta[5,3]*beta[6,5]*gamma[1,1]*gamma[6,2]*phi[1,2] -
   2*beta[4,1]*beta[5,4]*beta[6,5]*gamma[1,1]*gamma[6,2]*phi[1,2] -
   2*beta[2,1]*beta[4,2]*beta[5,4]*beta[6,5]*gamma[1,1]*gamma[6,2]*phi[1,2] -
   2*beta[3,1]*beta[4,3]*beta[5,4]*beta[6,5]*gamma[1,1]*gamma[6,2]*phi[1,2] -
   2*beta[2,1]*beta[3,2]*beta[4,3]*beta[5,4]*beta[6,5]*gamma[1,1]*gamma[6,2]*phi[1,2] -
   2*beta[3,2]*beta[6,3]*gamma[2,1]*gamma[6,2]*phi[1,2] -
   2*beta[4,2]*beta[6,4]*gamma[2,1]*gamma[6,2]*phi[1,2] -
   2*beta[3,2]*beta[4,3]*beta[6,4]*gamma[2,1]*gamma[6,2]*phi[1,2] -
   2*beta[5,2]*beta[6,5]*gamma[2,1]*gamma[6,2]*phi[1,2] -
   2*beta[3,2]*beta[5,3]*beta[6,5]*gamma[2,1]*gamma[6,2]*phi[1,2] -
   2*beta[4,2]*beta[5,4]*beta[6,5]*gamma[2,1]*gamma[6,2]*phi[1,2] -
   2*beta[3,2]*beta[4,3]*beta[5,4]*beta[6,5]*gamma[2,1]*gamma[6,2]*phi[1,2] -
   2*beta[6,3]*gamma[3,1]*gamma[6,2]*phi[1,2] -
   2*beta[4,3]*beta[6,4]*gamma[3,1]*gamma[6,2]*phi[1,2] -
   2*beta[5,3]*beta[6,5]*gamma[3,1]*gamma[6,2]*phi[1,2] -
   2*beta[4,3]*beta[5,4]*beta[6,5]*gamma[3,1]*gamma[6,2]*phi[1,2] -
   2*beta[6,4]*gamma[4,1]*gamma[6,2]*phi[1,2] -
   2*beta[5,4]*beta[6,5]*gamma[4,1]*gamma[6,2]*phi[1,2] -
   2*beta[6,5]*gamma[5,1]*gamma[6,2]*phi[1,2] -2*gamma[6,1]*gamma[6,2]*phi[1,2] -
   2*beta[6,1]*(beta[4,1]*beta[6,4] + beta[5,1]*beta[6,5] + beta[4,1]*beta[5,4]*beta[6,5] +
                beta[3,1]*(beta[6,3] + beta[5,3]*beta[6,5] +
                          beta[4,3]*(beta[6,4] + beta[5,4]*beta[6,5])) +
                beta[2,1]*(beta[6,2] + beta[4,2]*beta[6,4] + beta[5,2]*beta[6,5] +
                          beta[4,2]*beta[5,4]*beta[6,5] +
                          beta[3,2]*(beta[6,3] + beta[4,3]*beta[6,4] + beta[5,3]*beta[6,5] +
                                    beta[4,3]*beta[5,4]*beta[6,5])) + beta[6,2]*gamma[1,1]*gamma[2,1] +
                beta[3,2]*beta[6,3]*gamma[1,1]*gamma[2,1] + beta[4,2]*beta[6,4]*gamma[1,1]*gamma[2,1] +
                beta[3,2]*beta[4,3]*beta[6,4]*gamma[1,1]*gamma[2,1] +
                beta[5,2]*beta[6,5]*gamma[1,1]*gamma[2,1] +
                beta[3,2]*beta[5,3]*beta[6,5]*gamma[1,1]*gamma[2,1] +
                beta[4,2]*beta[5,4]*beta[6,5]*gamma[1,1]*gamma[2,1] +
                beta[3,2]*beta[4,3]*beta[5,4]*beta[6,5]*gamma[1,1]*gamma[2,1] +
                beta[6,2]*gamma[1,2]*gamma[2,2] + beta[3,2]*beta[6,3]*gamma[1,2]*gamma[2,2] +
                beta[4,2]*beta[6,4]*gamma[1,2]*gamma[2,2] +
                beta[3,2]*beta[4,3]*beta[6,4]*gamma[1,2]*gamma[2,2] +
                beta[5,2]*beta[6,5]*gamma[1,2]*gamma[2,2] +
                beta[3,2]*beta[5,3]*beta[6,5]*gamma[1,2]*gamma[2,2] +
                beta[4,2]*beta[5,4]*beta[6,5]*gamma[1,2]*gamma[2,2] +
                beta[3,2]*beta[4,3]*beta[5,4]*beta[6,5]*gamma[1,2]*gamma[2,2] +
                beta[6,3]*gamma[1,1]*gamma[3,1] + beta[4,3]*beta[6,4]*gamma[1,1]*gamma[3,1] +
                beta[5,3]*beta[6,5]*gamma[1,1]*gamma[3,1] +
                beta[4,3]*beta[5,4]*beta[6,5]*gamma[1,1]*gamma[3,1] + beta[6,3]*gamma[1,2]*gamma[3,2] +
                beta[4,3]*beta[6,4]*gamma[1,2]*gamma[3,2] + beta[5,3]*beta[6,5]*gamma[1,2]*gamma[3,2] +
                beta[4,3]*beta[5,4]*beta[6,5]*gamma[1,2]*gamma[3,2] + beta[6,4]*gamma[1,1]*gamma[4,1] +
                beta[5,4]*beta[6,5]*gamma[1,1]*gamma[4,1] + beta[6,4]*gamma[1,2]*gamma[4,2] +
                beta[5,4]*beta[6,5]*gamma[1,2]*gamma[4,2] + beta[6,5]*gamma[1,1]*gamma[5,1] +
                beta[6,5]*gamma[1,2]*gamma[5,2] + gamma[1,1]*gamma[6,1] + gamma[1,2]*gamma[6,2] +
                beta[6,2]*gamma[1,2]*gamma[2,1]*phi[1,2] +
                beta[3,2]*beta[6,3]*gamma[1,2]*gamma[2,1]*phi[1,2] +
                beta[4,2]*beta[6,4]*gamma[1,2]*gamma[2,1]*phi[1,2] +
                beta[3,2]*beta[4,3]*beta[6,4]*gamma[1,2]*gamma[2,1]*phi[1,2] +
                beta[5,2]*beta[6,5]*gamma[1,2]*gamma[2,1]*phi[1,2] +
                beta[3,2]*beta[5,3]*beta[6,5]*gamma[1,2]*gamma[2,1]*phi[1,2] +
                beta[4,2]*beta[5,4]*beta[6,5]*gamma[1,2]*gamma[2,1]*phi[1,2] +
                beta[3,2]*beta[4,3]*beta[5,4]*beta[6,5]*gamma[1,2]*gamma[2,1]*phi[1,2] +
                beta[6,2]*gamma[1,1]*gamma[2,2]*phi[1,2] +
                beta[3,2]*beta[6,3]*gamma[1,1]*gamma[2,2]*phi[1,2] +
                beta[4,2]*beta[6,4]*gamma[1,1]*gamma[2,2]*phi[1,2] +
                beta[3,2]*beta[4,3]*beta[6,4]*gamma[1,1]*gamma[2,2]*phi[1,2] +
                beta[5,2]*beta[6,5]*gamma[1,1]*gamma[2,2]*phi[1,2] +
                beta[3,2]*beta[5,3]*beta[6,5]*gamma[1,1]*gamma[2,2]*phi[1,2] +
                beta[4,2]*beta[5,4]*beta[6,5]*gamma[1,1]*gamma[2,2]*phi[1,2] +
                beta[3,2]*beta[4,3]*beta[5,4]*beta[6,5]*gamma[1,1]*gamma[2,2]*phi[1,2] +
                beta[6,3]*gamma[1,2]*gamma[3,1]*phi[1,2] +
                beta[4,3]*beta[6,4]*gamma[1,2]*gamma[3,1]*phi[1,2] +
                beta[5,3]*beta[6,5]*gamma[1,2]*gamma[3,1]*phi[1,2] +
                beta[4,3]*beta[5,4]*beta[6,5]*gamma[1,2]*gamma[3,1]*phi[1,2] +
                beta[6,3]*gamma[1,1]*gamma[3,2]*phi[1,2] +
                beta[4,3]*beta[6,4]*gamma[1,1]*gamma[3,2]*phi[1,2] +
                beta[5,3]*beta[6,5]*gamma[1,1]*gamma[3,2]*phi[1,2] +
                beta[4,3]*beta[5,4]*beta[6,5]*gamma[1,1]*gamma[3,2]*phi[1,2] +
                beta[6,4]*gamma[1,2]*gamma[4,1]*phi[1,2] +
                beta[5,4]*beta[6,5]*gamma[1,2]*gamma[4,1]*phi[1,2] +
                beta[6,4]*gamma[1,1]*gamma[4,2]*phi[1,2] +
                beta[5,4]*beta[6,5]*gamma[1,1]*gamma[4,2]*phi[1,2] +
                beta[6,5]*gamma[1,2]*gamma[5,1]*phi[1,2] + beta[6,5]*gamma[1,1]*gamma[5,2]*phi[1,2] +
                gamma[1,2]*gamma[6,1]*phi[1,2] + gamma[1,1]*gamma[6,2]*phi[1,2]) -
   2*beta[6,2]*(beta[4,2]*beta[6,4] + beta[5,2]*beta[6,5] + beta[4,2]*beta[5,4]*beta[6,5] +
                beta[3,2]*(beta[6,3] + beta[4,3]*beta[6,4] + beta[5,3]*beta[6,5] +
                          beta[4,3]*beta[5,4]*beta[6,5]) + beta[3,1]*beta[6,3]*gamma[1,1]*gamma[2,1] +
                beta[4,1]*beta[6,4]*gamma[1,1]*gamma[2,1] +
                beta[3,1]*beta[4,3]*beta[6,4]*gamma[1,1]*gamma[2,1] +
                beta[5,1]*beta[6,5]*gamma[1,1]*gamma[2,1] +
                beta[3,1]*beta[5,3]*beta[6,5]*gamma[1,1]*gamma[2,1] +
                beta[4,1]*beta[5,4]*beta[6,5]*gamma[1,1]*gamma[2,1] +
                beta[3,1]*beta[4,3]*beta[5,4]*beta[6,5]*gamma[1,1]*gamma[2,1] +
                beta[3,1]*beta[6,3]*gamma[1,2]*gamma[2,2] + beta[4,1]*beta[6,4]*gamma[1,2]*gamma[2,2] +
                beta[3,1]*beta[4,3]*beta[6,4]*gamma[1,2]*gamma[2,2] +
                beta[5,1]*beta[6,5]*gamma[1,2]*gamma[2,2] +
                beta[3,1]*beta[5,3]*beta[6,5]*gamma[1,2]*gamma[2,2] +
                beta[4,1]*beta[5,4]*beta[6,5]*gamma[1,2]*gamma[2,2] +
                beta[3,1]*beta[4,3]*beta[5,4]*beta[6,5]*gamma[1,2]*gamma[2,2] +
                beta[6,3]*gamma[2,1]*gamma[3,1] + beta[4,3]*beta[6,4]*gamma[2,1]*gamma[3,1] +
                beta[5,3]*beta[6,5]*gamma[2,1]*gamma[3,1] +
                beta[4,3]*beta[5,4]*beta[6,5]*gamma[2,1]*gamma[3,1] + beta[6,3]*gamma[2,2]*gamma[3,2] +
                beta[4,3]*beta[6,4]*gamma[2,2]*gamma[3,2] + beta[5,3]*beta[6,5]*gamma[2,2]*gamma[3,2] +
                beta[4,3]*beta[5,4]*beta[6,5]*gamma[2,2]*gamma[3,2] + beta[6,4]*gamma[2,1]*gamma[4,1] +
                beta[5,4]*beta[6,5]*gamma[2,1]*gamma[4,1] + beta[6,4]*gamma[2,2]*gamma[4,2] +
                beta[5,4]*beta[6,5]*gamma[2,2]*gamma[4,2] + beta[6,5]*gamma[2,1]*gamma[5,1] +
                beta[6,5]*gamma[2,2]*gamma[5,2] + gamma[2,1]*gamma[6,1] + gamma[2,2]*gamma[6,2] +
                beta[3,1]*beta[6,3]*gamma[1,2]*gamma[2,1]*phi[1,2] +
                beta[4,1]*beta[6,4]*gamma[1,2]*gamma[2,1]*phi[1,2] +
                beta[3,1]*beta[4,3]*beta[6,4]*gamma[1,2]*gamma[2,1]*phi[1,2] +
                beta[5,1]*beta[6,5]*gamma[1,2]*gamma[2,1]*phi[1,2] +
                beta[3,1]*beta[5,3]*beta[6,5]*gamma[1,2]*gamma[2,1]*phi[1,2] +
                beta[4,1]*beta[5,4]*beta[6,5]*gamma[1,2]*gamma[2,1]*phi[1,2] +
                beta[3,1]*beta[4,3]*beta[5,4]*beta[6,5]*gamma[1,2]*gamma[2,1]*phi[1,2] +
                beta[3,1]*beta[6,3]*gamma[1,1]*gamma[2,2]*phi[1,2] +
                beta[4,1]*beta[6,4]*gamma[1,1]*gamma[2,2]*phi[1,2] +
                beta[3,1]*beta[4,3]*beta[6,4]*gamma[1,1]*gamma[2,2]*phi[1,2] +
                beta[5,1]*beta[6,5]*gamma[1,1]*gamma[2,2]*phi[1,2] +
                beta[3,1]*beta[5,3]*beta[6,5]*gamma[1,1]*gamma[2,2]*phi[1,2] +
                beta[4,1]*beta[5,4]*beta[6,5]*gamma[1,1]*gamma[2,2]*phi[1,2] +
                beta[3,1]*beta[4,3]*beta[5,4]*beta[6,5]*gamma[1,1]*gamma[2,2]*phi[1,2] +
                beta[6,3]*gamma[2,2]*gamma[3,1]*phi[1,2] +
                beta[4,3]*beta[6,4]*gamma[2,2]*gamma[3,1]*phi[1,2] +
                beta[5,3]*beta[6,5]*gamma[2,2]*gamma[3,1]*phi[1,2] +
                beta[4,3]*beta[5,4]*beta[6,5]*gamma[2,2]*gamma[3,1]*phi[1,2] +
                beta[6,3]*gamma[2,1]*gamma[3,2]*phi[1,2] +
                beta[4,3]*beta[6,4]*gamma[2,1]*gamma[3,2]*phi[1,2] +
                beta[5,3]*beta[6,5]*gamma[2,1]*gamma[3,2]*phi[1,2] +
                beta[4,3]*beta[5,4]*beta[6,5]*gamma[2,1]*gamma[3,2]*phi[1,2] +
                beta[6,4]*gamma[2,2]*gamma[4,1]*phi[1,2] +
                beta[5,4]*beta[6,5]*gamma[2,2]*gamma[4,1]*phi[1,2] +
                beta[6,4]*gamma[2,1]*gamma[4,2]*phi[1,2] +
                beta[5,4]*beta[6,5]*gamma[2,1]*gamma[4,2]*phi[1,2] +
                beta[6,5]*gamma[2,2]*gamma[5,1]*phi[1,2] + beta[6,5]*gamma[2,1]*gamma[5,2]*phi[1,2] +
                gamma[2,2]*gamma[6,1]*phi[1,2] + gamma[2,1]*gamma[6,2]*phi[1,2] +
                beta[2,1]*(beta[4,1]*beta[6,4] + beta[5,1]*beta[6,5] + beta[4,1]*beta[5,4]*beta[6,5] +
                          beta[3,1]*(beta[6,3] + beta[4,3]*beta[6,4] + beta[5,3]*beta[6,5] +
                                    beta[4,3]*beta[5,4]*beta[6,5]) + beta[6,3]*gamma[1,1]*gamma[3,1] +
                          beta[4,3]*beta[6,4]*gamma[1,1]*gamma[3,1] + beta[5,3]*beta[6,5]*gamma[1,1]*gamma[3,1] +
                          beta[4,3]*beta[5,4]*beta[6,5]*gamma[1,1]*gamma[3,1] +
                          beta[6,3]*gamma[1,2]*gamma[3,2] + beta[4,3]*beta[6,4]*gamma[1,2]*gamma[3,2] +
                          beta[5,3]*beta[6,5]*gamma[1,2]*gamma[3,2] +
                          beta[4,3]*beta[5,4]*beta[6,5]*gamma[1,2]*gamma[3,2] + beta[6,4]*gamma[1,1]*gamma[4,1] +
                          beta[5,4]*beta[6,5]*gamma[1,1]*gamma[4,1] + beta[6,4]*gamma[1,2]*gamma[4,2] +
                          beta[5,4]*beta[6,5]*gamma[1,2]*gamma[4,2] + beta[6,5]*gamma[1,1]*gamma[5,1] +
                          beta[6,5]*gamma[1,2]*gamma[5,2] + gamma[1,1]*gamma[6,1] + gamma[1,2]*gamma[6,2] +
                          beta[6,3]*gamma[1,2]*gamma[3,1]*phi[1,2] +
                          beta[4,3]*beta[6,4]*gamma[1,2]*gamma[3,1]*phi[1,2] +
                          beta[5,3]*beta[6,5]*gamma[1,2]*gamma[3,1]*phi[1,2] +
                          beta[4,3]*beta[5,4]*beta[6,5]*gamma[1,2]*gamma[3,1]*phi[1,2] +
                          beta[6,3]*gamma[1,1]*gamma[3,2]*phi[1,2] +
                          beta[4,3]*beta[6,4]*gamma[1,1]*gamma[3,2]*phi[1,2] +
                          beta[5,3]*beta[6,5]*gamma[1,1]*gamma[3,2]*phi[1,2] +
                          beta[4,3]*beta[5,4]*beta[6,5]*gamma[1,1]*gamma[3,2]*phi[1,2] +
                          beta[6,4]*gamma[1,2]*gamma[4,1]*phi[1,2] +
                          beta[5,4]*beta[6,5]*gamma[1,2]*gamma[4,1]*phi[1,2] +
                          beta[6,4]*gamma[1,1]*gamma[4,2]*phi[1,2] +
                          beta[5,4]*beta[6,5]*gamma[1,1]*gamma[4,2]*phi[1,2] +
                          beta[6,5]*gamma[1,2]*gamma[5,1]*phi[1,2] + beta[6,5]*gamma[1,1]*gamma[5,2]*phi[1,2] +
                          gamma[1,2]*gamma[6,1]*phi[1,2] + gamma[1,1]*gamma[6,2]*phi[1,2]))
}


varzeta7 <- function(beta, gamma, phi){
  1 - beta[7,1]^2 - beta[7,2]^2 - beta[7,3]^2 -2*beta[3,1]*beta[4,1]*beta[7,3]*beta[7,4] -
   2*beta[2,1]*beta[3,2]*beta[4,1]*beta[7,3]*beta[7,4] -
   2*beta[2,1]*beta[3,1]*beta[4,2]*beta[7,3]*beta[7,4] -
   2*beta[3,2]*beta[4,2]*beta[7,3]*beta[7,4] -2*beta[4,3]*beta[7,3]*beta[7,4] - beta[7,4]^2 -
   2*beta[3,1]*beta[5,1]*beta[7,3]*beta[7,5] -
   2*beta[2,1]*beta[3,2]*beta[5,1]*beta[7,3]*beta[7,5] -
   2*beta[2,1]*beta[3,1]*beta[5,2]*beta[7,3]*beta[7,5] -
   2*beta[3,2]*beta[5,2]*beta[7,3]*beta[7,5] -2*beta[5,3]*beta[7,3]*beta[7,5] -
   2*beta[3,1]*beta[4,1]*beta[5,4]*beta[7,3]*beta[7,5] -
   2*beta[2,1]*beta[3,2]*beta[4,1]*beta[5,4]*beta[7,3]*beta[7,5] -
   2*beta[2,1]*beta[3,1]*beta[4,2]*beta[5,4]*beta[7,3]*beta[7,5] -
   2*beta[3,2]*beta[4,2]*beta[5,4]*beta[7,3]*beta[7,5] -
   2*beta[4,3]*beta[5,4]*beta[7,3]*beta[7,5] -2*beta[4,1]*beta[5,1]*beta[7,4]*beta[7,5] -
   2*beta[2,1]*beta[4,2]*beta[5,1]*beta[7,4]*beta[7,5] -
   2*beta[3,1]*beta[4,3]*beta[5,1]*beta[7,4]*beta[7,5] -
   2*beta[2,1]*beta[3,2]*beta[4,3]*beta[5,1]*beta[7,4]*beta[7,5] -
   2*beta[2,1]*beta[4,1]*beta[5,2]*beta[7,4]*beta[7,5] -
   2*beta[4,2]*beta[5,2]*beta[7,4]*beta[7,5] -
   2*beta[2,1]*beta[3,1]*beta[4,3]*beta[5,2]*beta[7,4]*beta[7,5] -
   2*beta[3,2]*beta[4,3]*beta[5,2]*beta[7,4]*beta[7,5] -
   2*beta[3,1]*beta[4,1]*beta[5,3]*beta[7,4]*beta[7,5] -
   2*beta[2,1]*beta[3,2]*beta[4,1]*beta[5,3]*beta[7,4]*beta[7,5] -
   2*beta[2,1]*beta[3,1]*beta[4,2]*beta[5,3]*beta[7,4]*beta[7,5] -
   2*beta[3,2]*beta[4,2]*beta[5,3]*beta[7,4]*beta[7,5] -
   2*beta[4,3]*beta[5,3]*beta[7,4]*beta[7,5] -2*beta[5,4]*beta[7,4]*beta[7,5] - beta[7,5]^2 -
   2*beta[3,1]*beta[6,1]*beta[7,3]*beta[7,6] -
   2*beta[2,1]*beta[3,2]*beta[6,1]*beta[7,3]*beta[7,6] -
   2*beta[2,1]*beta[3,1]*beta[6,2]*beta[7,3]*beta[7,6] -
   2*beta[3,2]*beta[6,2]*beta[7,3]*beta[7,6] -2*beta[6,3]*beta[7,3]*beta[7,6] -
   2*beta[3,1]*beta[4,1]*beta[6,4]*beta[7,3]*beta[7,6] -
   2*beta[2,1]*beta[3,2]*beta[4,1]*beta[6,4]*beta[7,3]*beta[7,6] -
   2*beta[2,1]*beta[3,1]*beta[4,2]*beta[6,4]*beta[7,3]*beta[7,6] -
   2*beta[3,2]*beta[4,2]*beta[6,4]*beta[7,3]*beta[7,6] -
   2*beta[4,3]*beta[6,4]*beta[7,3]*beta[7,6] -
   2*beta[3,1]*beta[5,1]*beta[6,5]*beta[7,3]*beta[7,6] -
   2*beta[2,1]*beta[3,2]*beta[5,1]*beta[6,5]*beta[7,3]*beta[7,6] -
   2*beta[2,1]*beta[3,1]*beta[5,2]*beta[6,5]*beta[7,3]*beta[7,6] -
   2*beta[3,2]*beta[5,2]*beta[6,5]*beta[7,3]*beta[7,6] -
   2*beta[5,3]*beta[6,5]*beta[7,3]*beta[7,6] -
   2*beta[3,1]*beta[4,1]*beta[5,4]*beta[6,5]*beta[7,3]*beta[7,6] -
   2*beta[2,1]*beta[3,2]*beta[4,1]*beta[5,4]*beta[6,5]*beta[7,3]*beta[7,6] -
   2*beta[2,1]*beta[3,1]*beta[4,2]*beta[5,4]*beta[6,5]*beta[7,3]*beta[7,6] -
   2*beta[3,2]*beta[4,2]*beta[5,4]*beta[6,5]*beta[7,3]*beta[7,6] -
   2*beta[4,3]*beta[5,4]*beta[6,5]*beta[7,3]*beta[7,6] -
   2*beta[4,1]*beta[6,1]*beta[7,4]*beta[7,6] -
   2*beta[2,1]*beta[4,2]*beta[6,1]*beta[7,4]*beta[7,6] -
   2*beta[3,1]*beta[4,3]*beta[6,1]*beta[7,4]*beta[7,6] -
   2*beta[2,1]*beta[3,2]*beta[4,3]*beta[6,1]*beta[7,4]*beta[7,6] -
   2*beta[2,1]*beta[4,1]*beta[6,2]*beta[7,4]*beta[7,6] -
   2*beta[4,2]*beta[6,2]*beta[7,4]*beta[7,6] -
   2*beta[2,1]*beta[3,1]*beta[4,3]*beta[6,2]*beta[7,4]*beta[7,6] -
   2*beta[3,2]*beta[4,3]*beta[6,2]*beta[7,4]*beta[7,6] -
   2*beta[3,1]*beta[4,1]*beta[6,3]*beta[7,4]*beta[7,6] -
   2*beta[2,1]*beta[3,2]*beta[4,1]*beta[6,3]*beta[7,4]*beta[7,6] -
   2*beta[2,1]*beta[3,1]*beta[4,2]*beta[6,3]*beta[7,4]*beta[7,6] -
   2*beta[3,2]*beta[4,2]*beta[6,3]*beta[7,4]*beta[7,6] -
   2*beta[4,3]*beta[6,3]*beta[7,4]*beta[7,6] -2*beta[6,4]*beta[7,4]*beta[7,6] -
   2*beta[4,1]*beta[5,1]*beta[6,5]*beta[7,4]*beta[7,6] -
   2*beta[2,1]*beta[4,2]*beta[5,1]*beta[6,5]*beta[7,4]*beta[7,6] -
   2*beta[3,1]*beta[4,3]*beta[5,1]*beta[6,5]*beta[7,4]*beta[7,6] -
   2*beta[2,1]*beta[3,2]*beta[4,3]*beta[5,1]*beta[6,5]*beta[7,4]*beta[7,6] -
   2*beta[2,1]*beta[4,1]*beta[5,2]*beta[6,5]*beta[7,4]*beta[7,6] -
   2*beta[4,2]*beta[5,2]*beta[6,5]*beta[7,4]*beta[7,6] -
   2*beta[2,1]*beta[3,1]*beta[4,3]*beta[5,2]*beta[6,5]*beta[7,4]*beta[7,6] -
   2*beta[3,2]*beta[4,3]*beta[5,2]*beta[6,5]*beta[7,4]*beta[7,6] -
   2*beta[3,1]*beta[4,1]*beta[5,3]*beta[6,5]*beta[7,4]*beta[7,6] -
   2*beta[2,1]*beta[3,2]*beta[4,1]*beta[5,3]*beta[6,5]*beta[7,4]*beta[7,6] -
   2*beta[2,1]*beta[3,1]*beta[4,2]*beta[5,3]*beta[6,5]*beta[7,4]*beta[7,6] -
   2*beta[3,2]*beta[4,2]*beta[5,3]*beta[6,5]*beta[7,4]*beta[7,6] -
   2*beta[4,3]*beta[5,3]*beta[6,5]*beta[7,4]*beta[7,6] -
   2*beta[5,4]*beta[6,5]*beta[7,4]*beta[7,6] -2*beta[5,1]*beta[6,1]*beta[7,5]*beta[7,6] -
   2*beta[2,1]*beta[5,2]*beta[6,1]*beta[7,5]*beta[7,6] -
   2*beta[3,1]*beta[5,3]*beta[6,1]*beta[7,5]*beta[7,6] -
   2*beta[2,1]*beta[3,2]*beta[5,3]*beta[6,1]*beta[7,5]*beta[7,6] -
   2*beta[4,1]*beta[5,4]*beta[6,1]*beta[7,5]*beta[7,6] -
   2*beta[2,1]*beta[4,2]*beta[5,4]*beta[6,1]*beta[7,5]*beta[7,6] -
   2*beta[3,1]*beta[4,3]*beta[5,4]*beta[6,1]*beta[7,5]*beta[7,6] -
   2*beta[2,1]*beta[3,2]*beta[4,3]*beta[5,4]*beta[6,1]*beta[7,5]*beta[7,6] -
   2*beta[2,1]*beta[5,1]*beta[6,2]*beta[7,5]*beta[7,6] -
   2*beta[5,2]*beta[6,2]*beta[7,5]*beta[7,6] -
   2*beta[2,1]*beta[3,1]*beta[5,3]*beta[6,2]*beta[7,5]*beta[7,6] -
   2*beta[3,2]*beta[5,3]*beta[6,2]*beta[7,5]*beta[7,6] -
   2*beta[2,1]*beta[4,1]*beta[5,4]*beta[6,2]*beta[7,5]*beta[7,6] -
   2*beta[4,2]*beta[5,4]*beta[6,2]*beta[7,5]*beta[7,6] -
   2*beta[2,1]*beta[3,1]*beta[4,3]*beta[5,4]*beta[6,2]*beta[7,5]*beta[7,6] -
   2*beta[3,2]*beta[4,3]*beta[5,4]*beta[6,2]*beta[7,5]*beta[7,6] -
   2*beta[3,1]*beta[5,1]*beta[6,3]*beta[7,5]*beta[7,6] -
   2*beta[2,1]*beta[3,2]*beta[5,1]*beta[6,3]*beta[7,5]*beta[7,6] -
   2*beta[2,1]*beta[3,1]*beta[5,2]*beta[6,3]*beta[7,5]*beta[7,6] -
   2*beta[3,2]*beta[5,2]*beta[6,3]*beta[7,5]*beta[7,6] -
   2*beta[5,3]*beta[6,3]*beta[7,5]*beta[7,6] -
   2*beta[3,1]*beta[4,1]*beta[5,4]*beta[6,3]*beta[7,5]*beta[7,6] -
   2*beta[2,1]*beta[3,2]*beta[4,1]*beta[5,4]*beta[6,3]*beta[7,5]*beta[7,6] -
   2*beta[2,1]*beta[3,1]*beta[4,2]*beta[5,4]*beta[6,3]*beta[7,5]*beta[7,6] -
   2*beta[3,2]*beta[4,2]*beta[5,4]*beta[6,3]*beta[7,5]*beta[7,6] -
   2*beta[4,3]*beta[5,4]*beta[6,3]*beta[7,5]*beta[7,6] -
   2*beta[4,1]*beta[5,1]*beta[6,4]*beta[7,5]*beta[7,6] -
   2*beta[2,1]*beta[4,2]*beta[5,1]*beta[6,4]*beta[7,5]*beta[7,6] -
   2*beta[3,1]*beta[4,3]*beta[5,1]*beta[6,4]*beta[7,5]*beta[7,6] -
   2*beta[2,1]*beta[3,2]*beta[4,3]*beta[5,1]*beta[6,4]*beta[7,5]*beta[7,6] -
   2*beta[2,1]*beta[4,1]*beta[5,2]*beta[6,4]*beta[7,5]*beta[7,6] -
   2*beta[4,2]*beta[5,2]*beta[6,4]*beta[7,5]*beta[7,6] -
   2*beta[2,1]*beta[3,1]*beta[4,3]*beta[5,2]*beta[6,4]*beta[7,5]*beta[7,6] -
   2*beta[3,2]*beta[4,3]*beta[5,2]*beta[6,4]*beta[7,5]*beta[7,6] -
   2*beta[3,1]*beta[4,1]*beta[5,3]*beta[6,4]*beta[7,5]*beta[7,6] -
   2*beta[2,1]*beta[3,2]*beta[4,1]*beta[5,3]*beta[6,4]*beta[7,5]*beta[7,6] -
   2*beta[2,1]*beta[3,1]*beta[4,2]*beta[5,3]*beta[6,4]*beta[7,5]*beta[7,6] -
   2*beta[3,2]*beta[4,2]*beta[5,3]*beta[6,4]*beta[7,5]*beta[7,6] -
   2*beta[4,3]*beta[5,3]*beta[6,4]*beta[7,5]*beta[7,6] -
   2*beta[5,4]*beta[6,4]*beta[7,5]*beta[7,6] -2*beta[6,5]*beta[7,5]*beta[7,6] - beta[7,6]^2 -
   2*beta[3,2]*beta[4,1]*beta[7,3]*beta[7,4]*gamma[1,1]*gamma[2,1] -
   2*beta[3,1]*beta[4,2]*beta[7,3]*beta[7,4]*gamma[1,1]*gamma[2,1] -
   2*beta[3,2]*beta[5,1]*beta[7,3]*beta[7,5]*gamma[1,1]*gamma[2,1] -
   2*beta[3,1]*beta[5,2]*beta[7,3]*beta[7,5]*gamma[1,1]*gamma[2,1] -
   2*beta[3,2]*beta[4,1]*beta[5,4]*beta[7,3]*beta[7,5]*gamma[1,1]*gamma[2,1] -
   2*beta[3,1]*beta[4,2]*beta[5,4]*beta[7,3]*beta[7,5]*gamma[1,1]*gamma[2,1] -
   2*beta[4,2]*beta[5,1]*beta[7,4]*beta[7,5]*gamma[1,1]*gamma[2,1] -
   2*beta[3,2]*beta[4,3]*beta[5,1]*beta[7,4]*beta[7,5]*gamma[1,1]*gamma[2,1] -
   2*beta[4,1]*beta[5,2]*beta[7,4]*beta[7,5]*gamma[1,1]*gamma[2,1] -
   2*beta[3,1]*beta[4,3]*beta[5,2]*beta[7,4]*beta[7,5]*gamma[1,1]*gamma[2,1] -
   2*beta[3,2]*beta[4,1]*beta[5,3]*beta[7,4]*beta[7,5]*gamma[1,1]*gamma[2,1] -
   2*beta[3,1]*beta[4,2]*beta[5,3]*beta[7,4]*beta[7,5]*gamma[1,1]*gamma[2,1] -
   2*beta[3,2]*beta[6,1]*beta[7,3]*beta[7,6]*gamma[1,1]*gamma[2,1] -
   2*beta[3,1]*beta[6,2]*beta[7,3]*beta[7,6]*gamma[1,1]*gamma[2,1] -
   2*beta[3,2]*beta[4,1]*beta[6,4]*beta[7,3]*beta[7,6]*gamma[1,1]*gamma[2,1] -
   2*beta[3,1]*beta[4,2]*beta[6,4]*beta[7,3]*beta[7,6]*gamma[1,1]*gamma[2,1] -
   2*beta[3,2]*beta[5,1]*beta[6,5]*beta[7,3]*beta[7,6]*gamma[1,1]*gamma[2,1] -
   2*beta[3,1]*beta[5,2]*beta[6,5]*beta[7,3]*beta[7,6]*gamma[1,1]*gamma[2,1] -
   2*beta[3,2]*beta[4,1]*beta[5,4]*beta[6,5]*beta[7,3]*beta[7,6]*gamma[1,1]*gamma[2,1] -
   2*beta[3,1]*beta[4,2]*beta[5,4]*beta[6,5]*beta[7,3]*beta[7,6]*gamma[1,1]*gamma[2,1] -
   2*beta[4,2]*beta[6,1]*beta[7,4]*beta[7,6]*gamma[1,1]*gamma[2,1] -
   2*beta[3,2]*beta[4,3]*beta[6,1]*beta[7,4]*beta[7,6]*gamma[1,1]*gamma[2,1] -
   2*beta[4,1]*beta[6,2]*beta[7,4]*beta[7,6]*gamma[1,1]*gamma[2,1] -
   2*beta[3,1]*beta[4,3]*beta[6,2]*beta[7,4]*beta[7,6]*gamma[1,1]*gamma[2,1] -
   2*beta[3,2]*beta[4,1]*beta[6,3]*beta[7,4]*beta[7,6]*gamma[1,1]*gamma[2,1] -
   2*beta[3,1]*beta[4,2]*beta[6,3]*beta[7,4]*beta[7,6]*gamma[1,1]*gamma[2,1] -
   2*beta[4,2]*beta[5,1]*beta[6,5]*beta[7,4]*beta[7,6]*gamma[1,1]*gamma[2,1] -
   2*beta[3,2]*beta[4,3]*beta[5,1]*beta[6,5]*beta[7,4]*beta[7,6]*gamma[1,1]*gamma[2,1] -
   2*beta[4,1]*beta[5,2]*beta[6,5]*beta[7,4]*beta[7,6]*gamma[1,1]*gamma[2,1] -
   2*beta[3,1]*beta[4,3]*beta[5,2]*beta[6,5]*beta[7,4]*beta[7,6]*gamma[1,1]*gamma[2,1] -
   2*beta[3,2]*beta[4,1]*beta[5,3]*beta[6,5]*beta[7,4]*beta[7,6]*gamma[1,1]*gamma[2,1] -
   2*beta[3,1]*beta[4,2]*beta[5,3]*beta[6,5]*beta[7,4]*beta[7,6]*gamma[1,1]*gamma[2,1] -
   2*beta[5,2]*beta[6,1]*beta[7,5]*beta[7,6]*gamma[1,1]*gamma[2,1] -
   2*beta[3,2]*beta[5,3]*beta[6,1]*beta[7,5]*beta[7,6]*gamma[1,1]*gamma[2,1] -
   2*beta[4,2]*beta[5,4]*beta[6,1]*beta[7,5]*beta[7,6]*gamma[1,1]*gamma[2,1] -
   2*beta[3,2]*beta[4,3]*beta[5,4]*beta[6,1]*beta[7,5]*beta[7,6]*gamma[1,1]*gamma[2,1] -
   2*beta[5,1]*beta[6,2]*beta[7,5]*beta[7,6]*gamma[1,1]*gamma[2,1] -
   2*beta[3,1]*beta[5,3]*beta[6,2]*beta[7,5]*beta[7,6]*gamma[1,1]*gamma[2,1] -
   2*beta[4,1]*beta[5,4]*beta[6,2]*beta[7,5]*beta[7,6]*gamma[1,1]*gamma[2,1] -
   2*beta[3,1]*beta[4,3]*beta[5,4]*beta[6,2]*beta[7,5]*beta[7,6]*gamma[1,1]*gamma[2,1] -
   2*beta[3,2]*beta[5,1]*beta[6,3]*beta[7,5]*beta[7,6]*gamma[1,1]*gamma[2,1] -
   2*beta[3,1]*beta[5,2]*beta[6,3]*beta[7,5]*beta[7,6]*gamma[1,1]*gamma[2,1] -
   2*beta[3,2]*beta[4,1]*beta[5,4]*beta[6,3]*beta[7,5]*beta[7,6]*gamma[1,1]*gamma[2,1] -
   2*beta[3,1]*beta[4,2]*beta[5,4]*beta[6,3]*beta[7,5]*beta[7,6]*gamma[1,1]*gamma[2,1] -
   2*beta[4,2]*beta[5,1]*beta[6,4]*beta[7,5]*beta[7,6]*gamma[1,1]*gamma[2,1] -
   2*beta[3,2]*beta[4,3]*beta[5,1]*beta[6,4]*beta[7,5]*beta[7,6]*gamma[1,1]*gamma[2,1] -
   2*beta[4,1]*beta[5,2]*beta[6,4]*beta[7,5]*beta[7,6]*gamma[1,1]*gamma[2,1] -
   2*beta[3,1]*beta[4,3]*beta[5,2]*beta[6,4]*beta[7,5]*beta[7,6]*gamma[1,1]*gamma[2,1] -
   2*beta[3,2]*beta[4,1]*beta[5,3]*beta[6,4]*beta[7,5]*beta[7,6]*gamma[1,1]*gamma[2,1] -
   2*beta[3,1]*beta[4,2]*beta[5,3]*beta[6,4]*beta[7,5]*beta[7,6]*gamma[1,1]*gamma[2,1] -
   2*beta[4,1]*beta[7,3]*beta[7,4]*gamma[1,1]*gamma[3,1] -
   2*beta[2,1]*beta[4,2]*beta[7,3]*beta[7,4]*gamma[1,1]*gamma[3,1] -
   2*beta[5,1]*beta[7,3]*beta[7,5]*gamma[1,1]*gamma[3,1] -
   2*beta[2,1]*beta[5,2]*beta[7,3]*beta[7,5]*gamma[1,1]*gamma[3,1] -
   2*beta[4,1]*beta[5,4]*beta[7,3]*beta[7,5]*gamma[1,1]*gamma[3,1] -
   2*beta[2,1]*beta[4,2]*beta[5,4]*beta[7,3]*beta[7,5]*gamma[1,1]*gamma[3,1] -
   2*beta[4,3]*beta[5,1]*beta[7,4]*beta[7,5]*gamma[1,1]*gamma[3,1] -
   2*beta[2,1]*beta[4,3]*beta[5,2]*beta[7,4]*beta[7,5]*gamma[1,1]*gamma[3,1] -
   2*beta[4,1]*beta[5,3]*beta[7,4]*beta[7,5]*gamma[1,1]*gamma[3,1] -
   2*beta[2,1]*beta[4,2]*beta[5,3]*beta[7,4]*beta[7,5]*gamma[1,1]*gamma[3,1] -
   2*beta[6,1]*beta[7,3]*beta[7,6]*gamma[1,1]*gamma[3,1] -
   2*beta[2,1]*beta[6,2]*beta[7,3]*beta[7,6]*gamma[1,1]*gamma[3,1] -
   2*beta[4,1]*beta[6,4]*beta[7,3]*beta[7,6]*gamma[1,1]*gamma[3,1] -
   2*beta[2,1]*beta[4,2]*beta[6,4]*beta[7,3]*beta[7,6]*gamma[1,1]*gamma[3,1] -
   2*beta[5,1]*beta[6,5]*beta[7,3]*beta[7,6]*gamma[1,1]*gamma[3,1] -
   2*beta[2,1]*beta[5,2]*beta[6,5]*beta[7,3]*beta[7,6]*gamma[1,1]*gamma[3,1] -
   2*beta[4,1]*beta[5,4]*beta[6,5]*beta[7,3]*beta[7,6]*gamma[1,1]*gamma[3,1] -
   2*beta[2,1]*beta[4,2]*beta[5,4]*beta[6,5]*beta[7,3]*beta[7,6]*gamma[1,1]*gamma[3,1] -
   2*beta[4,3]*beta[6,1]*beta[7,4]*beta[7,6]*gamma[1,1]*gamma[3,1] -
   2*beta[2,1]*beta[4,3]*beta[6,2]*beta[7,4]*beta[7,6]*gamma[1,1]*gamma[3,1] -
   2*beta[4,1]*beta[6,3]*beta[7,4]*beta[7,6]*gamma[1,1]*gamma[3,1] -
   2*beta[2,1]*beta[4,2]*beta[6,3]*beta[7,4]*beta[7,6]*gamma[1,1]*gamma[3,1] -
   2*beta[4,3]*beta[5,1]*beta[6,5]*beta[7,4]*beta[7,6]*gamma[1,1]*gamma[3,1] -
   2*beta[2,1]*beta[4,3]*beta[5,2]*beta[6,5]*beta[7,4]*beta[7,6]*gamma[1,1]*gamma[3,1] -
   2*beta[4,1]*beta[5,3]*beta[6,5]*beta[7,4]*beta[7,6]*gamma[1,1]*gamma[3,1] -
   2*beta[2,1]*beta[4,2]*beta[5,3]*beta[6,5]*beta[7,4]*beta[7,6]*gamma[1,1]*gamma[3,1] -
   2*beta[5,3]*beta[6,1]*beta[7,5]*beta[7,6]*gamma[1,1]*gamma[3,1] -
   2*beta[4,3]*beta[5,4]*beta[6,1]*beta[7,5]*beta[7,6]*gamma[1,1]*gamma[3,1] -
   2*beta[2,1]*beta[5,3]*beta[6,2]*beta[7,5]*beta[7,6]*gamma[1,1]*gamma[3,1] -
   2*beta[2,1]*beta[4,3]*beta[5,4]*beta[6,2]*beta[7,5]*beta[7,6]*gamma[1,1]*gamma[3,1] -
   2*beta[5,1]*beta[6,3]*beta[7,5]*beta[7,6]*gamma[1,1]*gamma[3,1] -
   2*beta[2,1]*beta[5,2]*beta[6,3]*beta[7,5]*beta[7,6]*gamma[1,1]*gamma[3,1] -
   2*beta[4,1]*beta[5,4]*beta[6,3]*beta[7,5]*beta[7,6]*gamma[1,1]*gamma[3,1] -
   2*beta[2,1]*beta[4,2]*beta[5,4]*beta[6,3]*beta[7,5]*beta[7,6]*gamma[1,1]*gamma[3,1] -
   2*beta[4,3]*beta[5,1]*beta[6,4]*beta[7,5]*beta[7,6]*gamma[1,1]*gamma[3,1] -
   2*beta[2,1]*beta[4,3]*beta[5,2]*beta[6,4]*beta[7,5]*beta[7,6]*gamma[1,1]*gamma[3,1] -
   2*beta[4,1]*beta[5,3]*beta[6,4]*beta[7,5]*beta[7,6]*gamma[1,1]*gamma[3,1] -
   2*beta[2,1]*beta[4,2]*beta[5,3]*beta[6,4]*beta[7,5]*beta[7,6]*gamma[1,1]*gamma[3,1] -
   2*beta[4,2]*beta[7,3]*beta[7,4]*gamma[2,1]*gamma[3,1] -
   2*beta[5,2]*beta[7,3]*beta[7,5]*gamma[2,1]*gamma[3,1] -
   2*beta[4,2]*beta[5,4]*beta[7,3]*beta[7,5]*gamma[2,1]*gamma[3,1] -
   2*beta[4,3]*beta[5,2]*beta[7,4]*beta[7,5]*gamma[2,1]*gamma[3,1] -
   2*beta[4,2]*beta[5,3]*beta[7,4]*beta[7,5]*gamma[2,1]*gamma[3,1] -
   2*beta[6,2]*beta[7,3]*beta[7,6]*gamma[2,1]*gamma[3,1] -
   2*beta[4,2]*beta[6,4]*beta[7,3]*beta[7,6]*gamma[2,1]*gamma[3,1] -
   2*beta[5,2]*beta[6,5]*beta[7,3]*beta[7,6]*gamma[2,1]*gamma[3,1] -
   2*beta[4,2]*beta[5,4]*beta[6,5]*beta[7,3]*beta[7,6]*gamma[2,1]*gamma[3,1] -
   2*beta[4,3]*beta[6,2]*beta[7,4]*beta[7,6]*gamma[2,1]*gamma[3,1] -
   2*beta[4,2]*beta[6,3]*beta[7,4]*beta[7,6]*gamma[2,1]*gamma[3,1] -
   2*beta[4,3]*beta[5,2]*beta[6,5]*beta[7,4]*beta[7,6]*gamma[2,1]*gamma[3,1] -
   2*beta[4,2]*beta[5,3]*beta[6,5]*beta[7,4]*beta[7,6]*gamma[2,1]*gamma[3,1] -
   2*beta[5,3]*beta[6,2]*beta[7,5]*beta[7,6]*gamma[2,1]*gamma[3,1] -
   2*beta[4,3]*beta[5,4]*beta[6,2]*beta[7,5]*beta[7,6]*gamma[2,1]*gamma[3,1] -
   2*beta[5,2]*beta[6,3]*beta[7,5]*beta[7,6]*gamma[2,1]*gamma[3,1] -
   2*beta[4,2]*beta[5,4]*beta[6,3]*beta[7,5]*beta[7,6]*gamma[2,1]*gamma[3,1] -
   2*beta[4,3]*beta[5,2]*beta[6,4]*beta[7,5]*beta[7,6]*gamma[2,1]*gamma[3,1] -
   2*beta[4,2]*beta[5,3]*beta[6,4]*beta[7,5]*beta[7,6]*gamma[2,1]*gamma[3,1] -
   2*beta[3,1]*beta[7,3]*beta[7,4]*gamma[1,1]*gamma[4,1] -
   2*beta[2,1]*beta[3,2]*beta[7,3]*beta[7,4]*gamma[1,1]*gamma[4,1] -
   2*beta[3,1]*beta[5,4]*beta[7,3]*beta[7,5]*gamma[1,1]*gamma[4,1] -
   2*beta[2,1]*beta[3,2]*beta[5,4]*beta[7,3]*beta[7,5]*gamma[1,1]*gamma[4,1] -
   2*beta[5,1]*beta[7,4]*beta[7,5]*gamma[1,1]*gamma[4,1] -
   2*beta[2,1]*beta[5,2]*beta[7,4]*beta[7,5]*gamma[1,1]*gamma[4,1] -
   2*beta[3,1]*beta[5,3]*beta[7,4]*beta[7,5]*gamma[1,1]*gamma[4,1] -
   2*beta[2,1]*beta[3,2]*beta[5,3]*beta[7,4]*beta[7,5]*gamma[1,1]*gamma[4,1] -
   2*beta[3,1]*beta[6,4]*beta[7,3]*beta[7,6]*gamma[1,1]*gamma[4,1] -
   2*beta[2,1]*beta[3,2]*beta[6,4]*beta[7,3]*beta[7,6]*gamma[1,1]*gamma[4,1] -
   2*beta[3,1]*beta[5,4]*beta[6,5]*beta[7,3]*beta[7,6]*gamma[1,1]*gamma[4,1] -
   2*beta[2,1]*beta[3,2]*beta[5,4]*beta[6,5]*beta[7,3]*beta[7,6]*gamma[1,1]*gamma[4,1] -
   2*beta[6,1]*beta[7,4]*beta[7,6]*gamma[1,1]*gamma[4,1] -
   2*beta[2,1]*beta[6,2]*beta[7,4]*beta[7,6]*gamma[1,1]*gamma[4,1] -
   2*beta[3,1]*beta[6,3]*beta[7,4]*beta[7,6]*gamma[1,1]*gamma[4,1] -
   2*beta[2,1]*beta[3,2]*beta[6,3]*beta[7,4]*beta[7,6]*gamma[1,1]*gamma[4,1] -
   2*beta[5,1]*beta[6,5]*beta[7,4]*beta[7,6]*gamma[1,1]*gamma[4,1] -
   2*beta[2,1]*beta[5,2]*beta[6,5]*beta[7,4]*beta[7,6]*gamma[1,1]*gamma[4,1] -
   2*beta[3,1]*beta[5,3]*beta[6,5]*beta[7,4]*beta[7,6]*gamma[1,1]*gamma[4,1] -
   2*beta[2,1]*beta[3,2]*beta[5,3]*beta[6,5]*beta[7,4]*beta[7,6]*gamma[1,1]*gamma[4,1] -
   2*beta[5,4]*beta[6,1]*beta[7,5]*beta[7,6]*gamma[1,1]*gamma[4,1] -
   2*beta[2,1]*beta[5,4]*beta[6,2]*beta[7,5]*beta[7,6]*gamma[1,1]*gamma[4,1] -
   2*beta[3,1]*beta[5,4]*beta[6,3]*beta[7,5]*beta[7,6]*gamma[1,1]*gamma[4,1] -
   2*beta[2,1]*beta[3,2]*beta[5,4]*beta[6,3]*beta[7,5]*beta[7,6]*gamma[1,1]*gamma[4,1] -
   2*beta[5,1]*beta[6,4]*beta[7,5]*beta[7,6]*gamma[1,1]*gamma[4,1] -
   2*beta[2,1]*beta[5,2]*beta[6,4]*beta[7,5]*beta[7,6]*gamma[1,1]*gamma[4,1] -
   2*beta[3,1]*beta[5,3]*beta[6,4]*beta[7,5]*beta[7,6]*gamma[1,1]*gamma[4,1] -
   2*beta[2,1]*beta[3,2]*beta[5,3]*beta[6,4]*beta[7,5]*beta[7,6]*gamma[1,1]*gamma[4,1] -
   2*beta[3,2]*beta[7,3]*beta[7,4]*gamma[2,1]*gamma[4,1] -
   2*beta[3,2]*beta[5,4]*beta[7,3]*beta[7,5]*gamma[2,1]*gamma[4,1] -
   2*beta[5,2]*beta[7,4]*beta[7,5]*gamma[2,1]*gamma[4,1] -
   2*beta[3,2]*beta[5,3]*beta[7,4]*beta[7,5]*gamma[2,1]*gamma[4,1] -
   2*beta[3,2]*beta[6,4]*beta[7,3]*beta[7,6]*gamma[2,1]*gamma[4,1] -
   2*beta[3,2]*beta[5,4]*beta[6,5]*beta[7,3]*beta[7,6]*gamma[2,1]*gamma[4,1] -
   2*beta[6,2]*beta[7,4]*beta[7,6]*gamma[2,1]*gamma[4,1] -
   2*beta[3,2]*beta[6,3]*beta[7,4]*beta[7,6]*gamma[2,1]*gamma[4,1] -
   2*beta[5,2]*beta[6,5]*beta[7,4]*beta[7,6]*gamma[2,1]*gamma[4,1] -
   2*beta[3,2]*beta[5,3]*beta[6,5]*beta[7,4]*beta[7,6]*gamma[2,1]*gamma[4,1] -
   2*beta[5,4]*beta[6,2]*beta[7,5]*beta[7,6]*gamma[2,1]*gamma[4,1] -
   2*beta[3,2]*beta[5,4]*beta[6,3]*beta[7,5]*beta[7,6]*gamma[2,1]*gamma[4,1] -
   2*beta[5,2]*beta[6,4]*beta[7,5]*beta[7,6]*gamma[2,1]*gamma[4,1] -
   2*beta[3,2]*beta[5,3]*beta[6,4]*beta[7,5]*beta[7,6]*gamma[2,1]*gamma[4,1] -
   2*beta[7,3]*beta[7,4]*gamma[3,1]*gamma[4,1] -
   2*beta[5,4]*beta[7,3]*beta[7,5]*gamma[3,1]*gamma[4,1] -
   2*beta[5,3]*beta[7,4]*beta[7,5]*gamma[3,1]*gamma[4,1] -
   2*beta[6,4]*beta[7,3]*beta[7,6]*gamma[3,1]*gamma[4,1] -
   2*beta[5,4]*beta[6,5]*beta[7,3]*beta[7,6]*gamma[3,1]*gamma[4,1] -
   2*beta[6,3]*beta[7,4]*beta[7,6]*gamma[3,1]*gamma[4,1] -
   2*beta[5,3]*beta[6,5]*beta[7,4]*beta[7,6]*gamma[3,1]*gamma[4,1] -
   2*beta[5,4]*beta[6,3]*beta[7,5]*beta[7,6]*gamma[3,1]*gamma[4,1] -
   2*beta[5,3]*beta[6,4]*beta[7,5]*beta[7,6]*gamma[3,1]*gamma[4,1] -
   2*beta[3,1]*beta[7,3]*beta[7,5]*gamma[1,1]*gamma[5,1] -
   2*beta[2,1]*beta[3,2]*beta[7,3]*beta[7,5]*gamma[1,1]*gamma[5,1] -
   2*beta[4,1]*beta[7,4]*beta[7,5]*gamma[1,1]*gamma[5,1] -
   2*beta[2,1]*beta[4,2]*beta[7,4]*beta[7,5]*gamma[1,1]*gamma[5,1] -
   2*beta[3,1]*beta[4,3]*beta[7,4]*beta[7,5]*gamma[1,1]*gamma[5,1] -
   2*beta[2,1]*beta[3,2]*beta[4,3]*beta[7,4]*beta[7,5]*gamma[1,1]*gamma[5,1] -
   2*beta[3,1]*beta[6,5]*beta[7,3]*beta[7,6]*gamma[1,1]*gamma[5,1] -
   2*beta[2,1]*beta[3,2]*beta[6,5]*beta[7,3]*beta[7,6]*gamma[1,1]*gamma[5,1] -
   2*beta[4,1]*beta[6,5]*beta[7,4]*beta[7,6]*gamma[1,1]*gamma[5,1] -
   2*beta[2,1]*beta[4,2]*beta[6,5]*beta[7,4]*beta[7,6]*gamma[1,1]*gamma[5,1] -
   2*beta[3,1]*beta[4,3]*beta[6,5]*beta[7,4]*beta[7,6]*gamma[1,1]*gamma[5,1] -
   2*beta[2,1]*beta[3,2]*beta[4,3]*beta[6,5]*beta[7,4]*beta[7,6]*gamma[1,1]*gamma[5,1] -
   2*beta[6,1]*beta[7,5]*beta[7,6]*gamma[1,1]*gamma[5,1] -
   2*beta[2,1]*beta[6,2]*beta[7,5]*beta[7,6]*gamma[1,1]*gamma[5,1] -
   2*beta[3,1]*beta[6,3]*beta[7,5]*beta[7,6]*gamma[1,1]*gamma[5,1] -
   2*beta[2,1]*beta[3,2]*beta[6,3]*beta[7,5]*beta[7,6]*gamma[1,1]*gamma[5,1] -
   2*beta[4,1]*beta[6,4]*beta[7,5]*beta[7,6]*gamma[1,1]*gamma[5,1] -
   2*beta[2,1]*beta[4,2]*beta[6,4]*beta[7,5]*beta[7,6]*gamma[1,1]*gamma[5,1] -
   2*beta[3,1]*beta[4,3]*beta[6,4]*beta[7,5]*beta[7,6]*gamma[1,1]*gamma[5,1] -
   2*beta[2,1]*beta[3,2]*beta[4,3]*beta[6,4]*beta[7,5]*beta[7,6]*gamma[1,1]*gamma[5,1] -
   2*beta[3,2]*beta[7,3]*beta[7,5]*gamma[2,1]*gamma[5,1] -
   2*beta[4,2]*beta[7,4]*beta[7,5]*gamma[2,1]*gamma[5,1] -
   2*beta[3,2]*beta[4,3]*beta[7,4]*beta[7,5]*gamma[2,1]*gamma[5,1] -
   2*beta[3,2]*beta[6,5]*beta[7,3]*beta[7,6]*gamma[2,1]*gamma[5,1] -
   2*beta[4,2]*beta[6,5]*beta[7,4]*beta[7,6]*gamma[2,1]*gamma[5,1] -
   2*beta[3,2]*beta[4,3]*beta[6,5]*beta[7,4]*beta[7,6]*gamma[2,1]*gamma[5,1] -
   2*beta[6,2]*beta[7,5]*beta[7,6]*gamma[2,1]*gamma[5,1] -
   2*beta[3,2]*beta[6,3]*beta[7,5]*beta[7,6]*gamma[2,1]*gamma[5,1] -
   2*beta[4,2]*beta[6,4]*beta[7,5]*beta[7,6]*gamma[2,1]*gamma[5,1] -
   2*beta[3,2]*beta[4,3]*beta[6,4]*beta[7,5]*beta[7,6]*gamma[2,1]*gamma[5,1] -
   2*beta[7,3]*beta[7,5]*gamma[3,1]*gamma[5,1] -
   2*beta[4,3]*beta[7,4]*beta[7,5]*gamma[3,1]*gamma[5,1] -
   2*beta[6,5]*beta[7,3]*beta[7,6]*gamma[3,1]*gamma[5,1] -
   2*beta[4,3]*beta[6,5]*beta[7,4]*beta[7,6]*gamma[3,1]*gamma[5,1] -
   2*beta[6,3]*beta[7,5]*beta[7,6]*gamma[3,1]*gamma[5,1] -
   2*beta[4,3]*beta[6,4]*beta[7,5]*beta[7,6]*gamma[3,1]*gamma[5,1] -
   2*beta[7,4]*beta[7,5]*gamma[4,1]*gamma[5,1] -
   2*beta[6,5]*beta[7,4]*beta[7,6]*gamma[4,1]*gamma[5,1] -
   2*beta[6,4]*beta[7,5]*beta[7,6]*gamma[4,1]*gamma[5,1] -
   2*beta[3,1]*beta[7,3]*beta[7,6]*gamma[1,1]*gamma[6,1] -
   2*beta[2,1]*beta[3,2]*beta[7,3]*beta[7,6]*gamma[1,1]*gamma[6,1] -
   2*beta[4,1]*beta[7,4]*beta[7,6]*gamma[1,1]*gamma[6,1] -
   2*beta[2,1]*beta[4,2]*beta[7,4]*beta[7,6]*gamma[1,1]*gamma[6,1] -
   2*beta[3,1]*beta[4,3]*beta[7,4]*beta[7,6]*gamma[1,1]*gamma[6,1] -
   2*beta[2,1]*beta[3,2]*beta[4,3]*beta[7,4]*beta[7,6]*gamma[1,1]*gamma[6,1] -
   2*beta[5,1]*beta[7,5]*beta[7,6]*gamma[1,1]*gamma[6,1] -
   2*beta[2,1]*beta[5,2]*beta[7,5]*beta[7,6]*gamma[1,1]*gamma[6,1] -
   2*beta[3,1]*beta[5,3]*beta[7,5]*beta[7,6]*gamma[1,1]*gamma[6,1] -
   2*beta[2,1]*beta[3,2]*beta[5,3]*beta[7,5]*beta[7,6]*gamma[1,1]*gamma[6,1] -
   2*beta[4,1]*beta[5,4]*beta[7,5]*beta[7,6]*gamma[1,1]*gamma[6,1] -
   2*beta[2,1]*beta[4,2]*beta[5,4]*beta[7,5]*beta[7,6]*gamma[1,1]*gamma[6,1] -
   2*beta[3,1]*beta[4,3]*beta[5,4]*beta[7,5]*beta[7,6]*gamma[1,1]*gamma[6,1] -
   2*beta[2,1]*beta[3,2]*beta[4,3]*beta[5,4]*beta[7,5]*beta[7,6]*gamma[1,1]*gamma[6,1] -
   2*beta[3,2]*beta[7,3]*beta[7,6]*gamma[2,1]*gamma[6,1] -
   2*beta[4,2]*beta[7,4]*beta[7,6]*gamma[2,1]*gamma[6,1] -
   2*beta[3,2]*beta[4,3]*beta[7,4]*beta[7,6]*gamma[2,1]*gamma[6,1] -
   2*beta[5,2]*beta[7,5]*beta[7,6]*gamma[2,1]*gamma[6,1] -
   2*beta[3,2]*beta[5,3]*beta[7,5]*beta[7,6]*gamma[2,1]*gamma[6,1] -
   2*beta[4,2]*beta[5,4]*beta[7,5]*beta[7,6]*gamma[2,1]*gamma[6,1] -
   2*beta[3,2]*beta[4,3]*beta[5,4]*beta[7,5]*beta[7,6]*gamma[2,1]*gamma[6,1] -
   2*beta[7,3]*beta[7,6]*gamma[3,1]*gamma[6,1] -
   2*beta[4,3]*beta[7,4]*beta[7,6]*gamma[3,1]*gamma[6,1] -
   2*beta[5,3]*beta[7,5]*beta[7,6]*gamma[3,1]*gamma[6,1] -
   2*beta[4,3]*beta[5,4]*beta[7,5]*beta[7,6]*gamma[3,1]*gamma[6,1] -
   2*beta[7,4]*beta[7,6]*gamma[4,1]*gamma[6,1] -
   2*beta[5,4]*beta[7,5]*beta[7,6]*gamma[4,1]*gamma[6,1] -
   2*beta[7,5]*beta[7,6]*gamma[5,1]*gamma[6,1] -2*beta[3,1]*beta[7,3]*gamma[1,1]*gamma[7,1] -
   2*beta[2,1]*beta[3,2]*beta[7,3]*gamma[1,1]*gamma[7,1] -
   2*beta[4,1]*beta[7,4]*gamma[1,1]*gamma[7,1] -
   2*beta[2,1]*beta[4,2]*beta[7,4]*gamma[1,1]*gamma[7,1] -
   2*beta[3,1]*beta[4,3]*beta[7,4]*gamma[1,1]*gamma[7,1] -
   2*beta[2,1]*beta[3,2]*beta[4,3]*beta[7,4]*gamma[1,1]*gamma[7,1] -
   2*beta[5,1]*beta[7,5]*gamma[1,1]*gamma[7,1] -
   2*beta[2,1]*beta[5,2]*beta[7,5]*gamma[1,1]*gamma[7,1] -
   2*beta[3,1]*beta[5,3]*beta[7,5]*gamma[1,1]*gamma[7,1] -
   2*beta[2,1]*beta[3,2]*beta[5,3]*beta[7,5]*gamma[1,1]*gamma[7,1] -
   2*beta[4,1]*beta[5,4]*beta[7,5]*gamma[1,1]*gamma[7,1] -
   2*beta[2,1]*beta[4,2]*beta[5,4]*beta[7,5]*gamma[1,1]*gamma[7,1] -
   2*beta[3,1]*beta[4,3]*beta[5,4]*beta[7,5]*gamma[1,1]*gamma[7,1] -
   2*beta[2,1]*beta[3,2]*beta[4,3]*beta[5,4]*beta[7,5]*gamma[1,1]*gamma[7,1] -
   2*beta[6,1]*beta[7,6]*gamma[1,1]*gamma[7,1] -
   2*beta[2,1]*beta[6,2]*beta[7,6]*gamma[1,1]*gamma[7,1] -
   2*beta[3,1]*beta[6,3]*beta[7,6]*gamma[1,1]*gamma[7,1] -
   2*beta[2,1]*beta[3,2]*beta[6,3]*beta[7,6]*gamma[1,1]*gamma[7,1] -
   2*beta[4,1]*beta[6,4]*beta[7,6]*gamma[1,1]*gamma[7,1] -
   2*beta[2,1]*beta[4,2]*beta[6,4]*beta[7,6]*gamma[1,1]*gamma[7,1] -
   2*beta[3,1]*beta[4,3]*beta[6,4]*beta[7,6]*gamma[1,1]*gamma[7,1] -
   2*beta[2,1]*beta[3,2]*beta[4,3]*beta[6,4]*beta[7,6]*gamma[1,1]*gamma[7,1] -
   2*beta[5,1]*beta[6,5]*beta[7,6]*gamma[1,1]*gamma[7,1] -
   2*beta[2,1]*beta[5,2]*beta[6,5]*beta[7,6]*gamma[1,1]*gamma[7,1] -
   2*beta[3,1]*beta[5,3]*beta[6,5]*beta[7,6]*gamma[1,1]*gamma[7,1] -
   2*beta[2,1]*beta[3,2]*beta[5,3]*beta[6,5]*beta[7,6]*gamma[1,1]*gamma[7,1] -
   2*beta[4,1]*beta[5,4]*beta[6,5]*beta[7,6]*gamma[1,1]*gamma[7,1] -
   2*beta[2,1]*beta[4,2]*beta[5,4]*beta[6,5]*beta[7,6]*gamma[1,1]*gamma[7,1] -
   2*beta[3,1]*beta[4,3]*beta[5,4]*beta[6,5]*beta[7,6]*gamma[1,1]*gamma[7,1] -
   2*beta[2,1]*beta[3,2]*beta[4,3]*beta[5,4]*beta[6,5]*beta[7,6]*gamma[1,1]*gamma[7,1] -
   2*beta[3,2]*beta[7,3]*gamma[2,1]*gamma[7,1] -2*beta[4,2]*beta[7,4]*gamma[2,1]*gamma[7,1] -
   2*beta[3,2]*beta[4,3]*beta[7,4]*gamma[2,1]*gamma[7,1] -
   2*beta[5,2]*beta[7,5]*gamma[2,1]*gamma[7,1] -
   2*beta[3,2]*beta[5,3]*beta[7,5]*gamma[2,1]*gamma[7,1] -
   2*beta[4,2]*beta[5,4]*beta[7,5]*gamma[2,1]*gamma[7,1] -
   2*beta[3,2]*beta[4,3]*beta[5,4]*beta[7,5]*gamma[2,1]*gamma[7,1] -
   2*beta[6,2]*beta[7,6]*gamma[2,1]*gamma[7,1] -
   2*beta[3,2]*beta[6,3]*beta[7,6]*gamma[2,1]*gamma[7,1] -
   2*beta[4,2]*beta[6,4]*beta[7,6]*gamma[2,1]*gamma[7,1] -
   2*beta[3,2]*beta[4,3]*beta[6,4]*beta[7,6]*gamma[2,1]*gamma[7,1] -
   2*beta[5,2]*beta[6,5]*beta[7,6]*gamma[2,1]*gamma[7,1] -
   2*beta[3,2]*beta[5,3]*beta[6,5]*beta[7,6]*gamma[2,1]*gamma[7,1] -
   2*beta[4,2]*beta[5,4]*beta[6,5]*beta[7,6]*gamma[2,1]*gamma[7,1] -
   2*beta[3,2]*beta[4,3]*beta[5,4]*beta[6,5]*beta[7,6]*gamma[2,1]*gamma[7,1] -
   2*beta[7,3]*gamma[3,1]*gamma[7,1] -2*beta[4,3]*beta[7,4]*gamma[3,1]*gamma[7,1] -
   2*beta[5,3]*beta[7,5]*gamma[3,1]*gamma[7,1] -
   2*beta[4,3]*beta[5,4]*beta[7,5]*gamma[3,1]*gamma[7,1] -
   2*beta[6,3]*beta[7,6]*gamma[3,1]*gamma[7,1] -
   2*beta[4,3]*beta[6,4]*beta[7,6]*gamma[3,1]*gamma[7,1] -
   2*beta[5,3]*beta[6,5]*beta[7,6]*gamma[3,1]*gamma[7,1] -
   2*beta[4,3]*beta[5,4]*beta[6,5]*beta[7,6]*gamma[3,1]*gamma[7,1] -
   2*beta[7,4]*gamma[4,1]*gamma[7,1] -2*beta[5,4]*beta[7,5]*gamma[4,1]*gamma[7,1] -
   2*beta[6,4]*beta[7,6]*gamma[4,1]*gamma[7,1] -
   2*beta[5,4]*beta[6,5]*beta[7,6]*gamma[4,1]*gamma[7,1] -2*beta[7,5]*gamma[5,1]*gamma[7,1] -
   2*beta[6,5]*beta[7,6]*gamma[5,1]*gamma[7,1] -
   2*beta[7,6]*gamma[6,1]*gamma[7,1] - gamma[7,1]^2 -
   2*beta[7,1]*(beta[3,1]*beta[7,3] + beta[4,1]*beta[7,4] + beta[3,1]*beta[4,3]*beta[7,4] +
                beta[5,1]*beta[7,5] + beta[3,1]*beta[5,3]*beta[7,5] + beta[4,1]*beta[5,4]*beta[7,5] +
                beta[3,1]*beta[4,3]*beta[5,4]*beta[7,5] + beta[6,1]*beta[7,6] +
                beta[3,1]*beta[6,3]*beta[7,6] + beta[4,1]*beta[6,4]*beta[7,6] +
                beta[3,1]*beta[4,3]*beta[6,4]*beta[7,6] + beta[5,1]*beta[6,5]*beta[7,6] +
                beta[3,1]*beta[5,3]*beta[6,5]*beta[7,6] + beta[4,1]*beta[5,4]*beta[6,5]*beta[7,6] +
                beta[3,1]*beta[4,3]*beta[5,4]*beta[6,5]*beta[7,6] +
                beta[2,1]*(beta[7,2] + beta[4,2]*beta[7,4] + beta[5,2]*beta[7,5] +
                          beta[4,2]*beta[5,4]*beta[7,5] + beta[6,2]*beta[7,6] + beta[4,2]*beta[6,4]*beta[7,6] +
                          beta[5,2]*beta[6,5]*beta[7,6] + beta[4,2]*beta[5,4]*beta[6,5]*beta[7,6] +
                          beta[3,2]*(beta[7,3] + beta[5,3]*beta[7,5] + beta[6,3]*beta[7,6] +
                                    beta[5,3]*beta[6,5]*beta[7,6] +
                                    beta[4,3]*(beta[7,4] + beta[5,4]*beta[7,5] + beta[6,4]*beta[7,6] +
                                              beta[5,4]*beta[6,5]*beta[7,6]))) + beta[7,2]*gamma[1,1]*gamma[2,1] +
                beta[3,2]*beta[7,3]*gamma[1,1]*gamma[2,1] + beta[4,2]*beta[7,4]*gamma[1,1]*gamma[2,1] +
                beta[3,2]*beta[4,3]*beta[7,4]*gamma[1,1]*gamma[2,1] +
                beta[5,2]*beta[7,5]*gamma[1,1]*gamma[2,1] +
                beta[3,2]*beta[5,3]*beta[7,5]*gamma[1,1]*gamma[2,1] +
                beta[4,2]*beta[5,4]*beta[7,5]*gamma[1,1]*gamma[2,1] +
                beta[3,2]*beta[4,3]*beta[5,4]*beta[7,5]*gamma[1,1]*gamma[2,1] +
                beta[6,2]*beta[7,6]*gamma[1,1]*gamma[2,1] +
                beta[3,2]*beta[6,3]*beta[7,6]*gamma[1,1]*gamma[2,1] +
                beta[4,2]*beta[6,4]*beta[7,6]*gamma[1,1]*gamma[2,1] +
                beta[3,2]*beta[4,3]*beta[6,4]*beta[7,6]*gamma[1,1]*gamma[2,1] +
                beta[5,2]*beta[6,5]*beta[7,6]*gamma[1,1]*gamma[2,1] +
                beta[3,2]*beta[5,3]*beta[6,5]*beta[7,6]*gamma[1,1]*gamma[2,1] +
                beta[4,2]*beta[5,4]*beta[6,5]*beta[7,6]*gamma[1,1]*gamma[2,1] +
                beta[3,2]*beta[4,3]*beta[5,4]*beta[6,5]*beta[7,6]*gamma[1,1]*gamma[2,1] +
                beta[7,3]*gamma[1,1]*gamma[3,1] + beta[4,3]*beta[7,4]*gamma[1,1]*gamma[3,1] +
                beta[5,3]*beta[7,5]*gamma[1,1]*gamma[3,1] +
                beta[4,3]*beta[5,4]*beta[7,5]*gamma[1,1]*gamma[3,1] +
                beta[6,3]*beta[7,6]*gamma[1,1]*gamma[3,1] +
                beta[4,3]*beta[6,4]*beta[7,6]*gamma[1,1]*gamma[3,1] +
                beta[5,3]*beta[6,5]*beta[7,6]*gamma[1,1]*gamma[3,1] +
                beta[4,3]*beta[5,4]*beta[6,5]*beta[7,6]*gamma[1,1]*gamma[3,1] +
                beta[7,4]*gamma[1,1]*gamma[4,1] + beta[5,4]*beta[7,5]*gamma[1,1]*gamma[4,1] +
                beta[6,4]*beta[7,6]*gamma[1,1]*gamma[4,1] +
                beta[5,4]*beta[6,5]*beta[7,6]*gamma[1,1]*gamma[4,1] + beta[7,5]*gamma[1,1]*gamma[5,1] +
                beta[6,5]*beta[7,6]*gamma[1,1]*gamma[5,1] + beta[7,6]*gamma[1,1]*gamma[6,1] +
                gamma[1,1]*gamma[7,1]) -
   2*beta[7,2]*(beta[4,2]*beta[7,4] + beta[5,2]*beta[7,5] + beta[4,2]*beta[5,4]*beta[7,5] +
                beta[6,2]*beta[7,6] + beta[4,2]*beta[6,4]*beta[7,6] + beta[5,2]*beta[6,5]*beta[7,6] +
                beta[4,2]*beta[5,4]*beta[6,5]*beta[7,6] +
                beta[3,2]*(beta[7,3] + beta[5,3]*beta[7,5] + beta[6,3]*beta[7,6] +
                          beta[5,3]*beta[6,5]*beta[7,6] +
                          beta[4,3]*(beta[7,4] + beta[5,4]*beta[7,5] + beta[6,4]*beta[7,6] +
                                    beta[5,4]*beta[6,5]*beta[7,6])) + beta[3,1]*beta[7,3]*gamma[1,1]*gamma[2,1] +
                beta[4,1]*beta[7,4]*gamma[1,1]*gamma[2,1] +
                beta[3,1]*beta[4,3]*beta[7,4]*gamma[1,1]*gamma[2,1] +
                beta[5,1]*beta[7,5]*gamma[1,1]*gamma[2,1] +
                beta[3,1]*beta[5,3]*beta[7,5]*gamma[1,1]*gamma[2,1] +
                beta[4,1]*beta[5,4]*beta[7,5]*gamma[1,1]*gamma[2,1] +
                beta[3,1]*beta[4,3]*beta[5,4]*beta[7,5]*gamma[1,1]*gamma[2,1] +
                beta[6,1]*beta[7,6]*gamma[1,1]*gamma[2,1] +
                beta[3,1]*beta[6,3]*beta[7,6]*gamma[1,1]*gamma[2,1] +
                beta[4,1]*beta[6,4]*beta[7,6]*gamma[1,1]*gamma[2,1] +
                beta[3,1]*beta[4,3]*beta[6,4]*beta[7,6]*gamma[1,1]*gamma[2,1] +
                beta[5,1]*beta[6,5]*beta[7,6]*gamma[1,1]*gamma[2,1] +
                beta[3,1]*beta[5,3]*beta[6,5]*beta[7,6]*gamma[1,1]*gamma[2,1] +
                beta[4,1]*beta[5,4]*beta[6,5]*beta[7,6]*gamma[1,1]*gamma[2,1] +
                beta[3,1]*beta[4,3]*beta[5,4]*beta[6,5]*beta[7,6]*gamma[1,1]*gamma[2,1] +
                beta[7,3]*gamma[2,1]*gamma[3,1] + beta[4,3]*beta[7,4]*gamma[2,1]*gamma[3,1] +
                beta[5,3]*beta[7,5]*gamma[2,1]*gamma[3,1] +
                beta[4,3]*beta[5,4]*beta[7,5]*gamma[2,1]*gamma[3,1] +
                beta[6,3]*beta[7,6]*gamma[2,1]*gamma[3,1] +
                beta[4,3]*beta[6,4]*beta[7,6]*gamma[2,1]*gamma[3,1] +
                beta[5,3]*beta[6,5]*beta[7,6]*gamma[2,1]*gamma[3,1] +
                beta[4,3]*beta[5,4]*beta[6,5]*beta[7,6]*gamma[2,1]*gamma[3,1] +
                beta[7,4]*gamma[2,1]*gamma[4,1] + beta[5,4]*beta[7,5]*gamma[2,1]*gamma[4,1] +
                beta[6,4]*beta[7,6]*gamma[2,1]*gamma[4,1] +
                beta[5,4]*beta[6,5]*beta[7,6]*gamma[2,1]*gamma[4,1] + beta[7,5]*gamma[2,1]*gamma[5,1] +
                beta[6,5]*beta[7,6]*gamma[2,1]*gamma[5,1] + beta[7,6]*gamma[2,1]*gamma[6,1] +
                gamma[2,1]*gamma[7,1] +
                beta[2,1]*(beta[5,1]*beta[7,5] + beta[6,1]*beta[7,6] + beta[5,1]*beta[6,5]*beta[7,6] +
                          beta[4,1]*(beta[7,4] + beta[6,4]*beta[7,6] +
                                    beta[5,4]*(beta[7,5] + beta[6,5]*beta[7,6])) +
                          beta[3,1]*(beta[7,3] + beta[5,3]*beta[7,5] + beta[6,3]*beta[7,6] +
                                    beta[5,3]*beta[6,5]*beta[7,6] +
                                    beta[4,3]*(beta[7,4] + beta[5,4]*beta[7,5] + beta[6,4]*beta[7,6] +
                                              beta[5,4]*beta[6,5]*beta[7,6])) + beta[7,3]*gamma[1,1]*gamma[3,1] +
                          beta[4,3]*beta[7,4]*gamma[1,1]*gamma[3,1] + beta[5,3]*beta[7,5]*gamma[1,1]*gamma[3,1] +
                          beta[4,3]*beta[5,4]*beta[7,5]*gamma[1,1]*gamma[3,1] +
                          beta[6,3]*beta[7,6]*gamma[1,1]*gamma[3,1] +
                          beta[4,3]*beta[6,4]*beta[7,6]*gamma[1,1]*gamma[3,1] +
                          beta[5,3]*beta[6,5]*beta[7,6]*gamma[1,1]*gamma[3,1] +
                          beta[4,3]*beta[5,4]*beta[6,5]*beta[7,6]*gamma[1,1]*gamma[3,1] +
                          beta[7,4]*gamma[1,1]*gamma[4,1] + beta[5,4]*beta[7,5]*gamma[1,1]*gamma[4,1] +
                          beta[6,4]*beta[7,6]*gamma[1,1]*gamma[4,1] +
                          beta[5,4]*beta[6,5]*beta[7,6]*gamma[1,1]*gamma[4,1] + beta[7,5]*gamma[1,1]*gamma[5,1] +
                          beta[6,5]*beta[7,6]*gamma[1,1]*gamma[5,1] + beta[7,6]*gamma[1,1]*gamma[6,1] +
                          gamma[1,1]*gamma[7,1]))
}
