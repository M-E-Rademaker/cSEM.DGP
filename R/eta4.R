eta4eta5 <- function(gamma){

   gamma[4,1]*gamma[5,1] + gamma[2,1]*gamma[4,2]*gamma[5,1] +
   gamma[3,1]*gamma[4,3]*gamma[5,1] + gamma[2,1]*gamma[3,2]*gamma[4,3]*gamma[5,1] +
   gamma[2,1]*gamma[4,1]*gamma[5,2] + gamma[4,2]*gamma[5,2] +
   gamma[2,1]*gamma[3,1]*gamma[4,3]*gamma[5,2] + gamma[3,2]*gamma[4,3]*gamma[5,2] +
   gamma[3,1]*gamma[4,1]*gamma[5,3] + gamma[2,1]*gamma[3,2]*gamma[4,1]*gamma[5,3] +
   gamma[2,1]*gamma[3,1]*gamma[4,2]*gamma[5,3] + gamma[3,2]*gamma[4,2]*gamma[5,3] +
   gamma[4,3]*gamma[5,3] + gamma[5,4]
}


eta4eta6 <- function(gamma){

    gamma[4,1]*gamma[6,1] + gamma[2,1]*gamma[4,2]*gamma[6,1] +
    gamma[3,1]*gamma[4,3]*gamma[6,1] + gamma[2,1]*gamma[3,2]*gamma[4,3]*gamma[6,1] +
    gamma[2,1]*gamma[4,1]*gamma[6,2] + gamma[4,2]*gamma[6,2] +
    gamma[2,1]*gamma[3,1]*gamma[4,3]*gamma[6,2] + gamma[3,2]*gamma[4,3]*gamma[6,2] +
    gamma[3,1]*gamma[4,1]*gamma[6,3] + gamma[2,1]*gamma[3,2]*gamma[4,1]*gamma[6,3] +
    gamma[2,1]*gamma[3,1]*gamma[4,2]*gamma[6,3] + gamma[3,2]*gamma[4,2]*gamma[6,3] +
    gamma[4,3]*gamma[6,3] + gamma[6,4] + gamma[4,1]*gamma[5,1]*gamma[6,5] +
    gamma[2,1]*gamma[4,2]*gamma[5,1]*gamma[6,5] + gamma[3,1]*gamma[4,3]*gamma[5,1]*gamma[6,5] +
    gamma[2,1]*gamma[3,2]*gamma[4,3]*gamma[5,1]*gamma[6,5] +
    gamma[2,1]*gamma[4,1]*gamma[5,2]*gamma[6,5] + gamma[4,2]*gamma[5,2]*gamma[6,5] +
    gamma[2,1]*gamma[3,1]*gamma[4,3]*gamma[5,2]*gamma[6,5] +
    gamma[3,2]*gamma[4,3]*gamma[5,2]*gamma[6,5] + gamma[3,1]*gamma[4,1]*gamma[5,3]*gamma[6,5] +
    gamma[2,1]*gamma[3,2]*gamma[4,1]*gamma[5,3]*gamma[6,5] +
    gamma[2,1]*gamma[3,1]*gamma[4,2]*gamma[5,3]*gamma[6,5] +
    gamma[3,2]*gamma[4,2]*gamma[5,3]*gamma[6,5] + gamma[4,3]*gamma[5,3]*gamma[6,5] +
    gamma[5,4]*gamma[6,5]
}


eta4eta7 <- function(gamma){

    gamma[4,1]*gamma[7,1] + gamma[2,1]*gamma[4,2]*gamma[7,1] +
    gamma[3,1]*gamma[4,3]*gamma[7,1] + gamma[2,1]*gamma[3,2]*gamma[4,3]*gamma[7,1] +
    gamma[2,1]*gamma[4,1]*gamma[7,2] + gamma[4,2]*gamma[7,2] +
    gamma[2,1]*gamma[3,1]*gamma[4,3]*gamma[7,2] + gamma[3,2]*gamma[4,3]*gamma[7,2] +
    gamma[3,1]*gamma[4,1]*gamma[7,3] + gamma[2,1]*gamma[3,2]*gamma[4,1]*gamma[7,3] +
    gamma[2,1]*gamma[3,1]*gamma[4,2]*gamma[7,3] + gamma[3,2]*gamma[4,2]*gamma[7,3] +
    gamma[4,3]*gamma[7,3] + gamma[7,4] + gamma[4,1]*gamma[5,1]*gamma[7,5] +
    gamma[2,1]*gamma[4,2]*gamma[5,1]*gamma[7,5] + gamma[3,1]*gamma[4,3]*gamma[5,1]*gamma[7,5] +
    gamma[2,1]*gamma[3,2]*gamma[4,3]*gamma[5,1]*gamma[7,5] +
    gamma[2,1]*gamma[4,1]*gamma[5,2]*gamma[7,5] + gamma[4,2]*gamma[5,2]*gamma[7,5] +
    gamma[2,1]*gamma[3,1]*gamma[4,3]*gamma[5,2]*gamma[7,5] +
    gamma[3,2]*gamma[4,3]*gamma[5,2]*gamma[7,5] + gamma[3,1]*gamma[4,1]*gamma[5,3]*gamma[7,5] +
    gamma[2,1]*gamma[3,2]*gamma[4,1]*gamma[5,3]*gamma[7,5] +
    gamma[2,1]*gamma[3,1]*gamma[4,2]*gamma[5,3]*gamma[7,5] +
    gamma[3,2]*gamma[4,2]*gamma[5,3]*gamma[7,5] + gamma[4,3]*gamma[5,3]*gamma[7,5] +
    gamma[5,4]*gamma[7,5] + gamma[4,1]*gamma[6,1]*gamma[7,6] +
    gamma[2,1]*gamma[4,2]*gamma[6,1]*gamma[7,6] + gamma[3,1]*gamma[4,3]*gamma[6,1]*gamma[7,6] +
    gamma[2,1]*gamma[3,2]*gamma[4,3]*gamma[6,1]*gamma[7,6] +
    gamma[2,1]*gamma[4,1]*gamma[6,2]*gamma[7,6] + gamma[4,2]*gamma[6,2]*gamma[7,6] +
    gamma[2,1]*gamma[3,1]*gamma[4,3]*gamma[6,2]*gamma[7,6] +
    gamma[3,2]*gamma[4,3]*gamma[6,2]*gamma[7,6] + gamma[3,1]*gamma[4,1]*gamma[6,3]*gamma[7,6] +
    gamma[2,1]*gamma[3,2]*gamma[4,1]*gamma[6,3]*gamma[7,6] +
    gamma[2,1]*gamma[3,1]*gamma[4,2]*gamma[6,3]*gamma[7,6] +
    gamma[3,2]*gamma[4,2]*gamma[6,3]*gamma[7,6] + gamma[4,3]*gamma[6,3]*gamma[7,6] +
    gamma[6,4]*gamma[7,6] + gamma[4,1]*gamma[5,1]*gamma[6,5]*gamma[7,6] +
    gamma[2,1]*gamma[4,2]*gamma[5,1]*gamma[6,5]*gamma[7,6] +
    gamma[3,1]*gamma[4,3]*gamma[5,1]*gamma[6,5]*gamma[7,6] +
    gamma[2,1]*gamma[3,2]*gamma[4,3]*gamma[5,1]*gamma[6,5]*gamma[7,6] +
    gamma[2,1]*gamma[4,1]*gamma[5,2]*gamma[6,5]*gamma[7,6] +
    gamma[4,2]*gamma[5,2]*gamma[6,5]*gamma[7,6] +
    gamma[2,1]*gamma[3,1]*gamma[4,3]*gamma[5,2]*gamma[6,5]*gamma[7,6] +
    gamma[3,2]*gamma[4,3]*gamma[5,2]*gamma[6,5]*gamma[7,6] +
    gamma[3,1]*gamma[4,1]*gamma[5,3]*gamma[6,5]*gamma[7,6] +
    gamma[2,1]*gamma[3,2]*gamma[4,1]*gamma[5,3]*gamma[6,5]*gamma[7,6] +
    gamma[2,1]*gamma[3,1]*gamma[4,2]*gamma[5,3]*gamma[6,5]*gamma[7,6] +
    gamma[3,2]*gamma[4,2]*gamma[5,3]*gamma[6,5]*gamma[7,6] +
    gamma[4,3]*gamma[5,3]*gamma[6,5]*gamma[7,6] + gamma[5,4]*gamma[6,5]*gamma[7,6]
}


eta4eta8 <- function(gamma){

   gamma[4,1]*gamma[8,1] + gamma[2,1]*gamma[4,2]*gamma[8,1] +
   gamma[3,1]*gamma[4,3]*gamma[8,1] + gamma[2,1]*gamma[3,2]*gamma[4,3]*gamma[8,1] +
   gamma[2,1]*gamma[4,1]*gamma[8,2] + gamma[4,2]*gamma[8,2] +
   gamma[2,1]*gamma[3,1]*gamma[4,3]*gamma[8,2] + gamma[3,2]*gamma[4,3]*gamma[8,2] +
   gamma[3,1]*gamma[4,1]*gamma[8,3] + gamma[2,1]*gamma[3,2]*gamma[4,1]*gamma[8,3] +
   gamma[2,1]*gamma[3,1]*gamma[4,2]*gamma[8,3] + gamma[3,2]*gamma[4,2]*gamma[8,3] +
   gamma[4,3]*gamma[8,3] + gamma[8,4] + gamma[4,1]*gamma[5,1]*gamma[8,5] +
   gamma[2,1]*gamma[4,2]*gamma[5,1]*gamma[8,5] + gamma[3,1]*gamma[4,3]*gamma[5,1]*gamma[8,5] +
   gamma[2,1]*gamma[3,2]*gamma[4,3]*gamma[5,1]*gamma[8,5] +
   gamma[2,1]*gamma[4,1]*gamma[5,2]*gamma[8,5] + gamma[4,2]*gamma[5,2]*gamma[8,5] +
   gamma[2,1]*gamma[3,1]*gamma[4,3]*gamma[5,2]*gamma[8,5] +
   gamma[3,2]*gamma[4,3]*gamma[5,2]*gamma[8,5] + gamma[3,1]*gamma[4,1]*gamma[5,3]*gamma[8,5] +
   gamma[2,1]*gamma[3,2]*gamma[4,1]*gamma[5,3]*gamma[8,5] +
   gamma[2,1]*gamma[3,1]*gamma[4,2]*gamma[5,3]*gamma[8,5] +
   gamma[3,2]*gamma[4,2]*gamma[5,3]*gamma[8,5] + gamma[4,3]*gamma[5,3]*gamma[8,5] +
   gamma[5,4]*gamma[8,5] + gamma[4,1]*gamma[6,1]*gamma[8,6] +
   gamma[2,1]*gamma[4,2]*gamma[6,1]*gamma[8,6] + gamma[3,1]*gamma[4,3]*gamma[6,1]*gamma[8,6] +
   gamma[2,1]*gamma[3,2]*gamma[4,3]*gamma[6,1]*gamma[8,6] +
   gamma[2,1]*gamma[4,1]*gamma[6,2]*gamma[8,6] + gamma[4,2]*gamma[6,2]*gamma[8,6] +
   gamma[2,1]*gamma[3,1]*gamma[4,3]*gamma[6,2]*gamma[8,6] +
   gamma[3,2]*gamma[4,3]*gamma[6,2]*gamma[8,6] + gamma[3,1]*gamma[4,1]*gamma[6,3]*gamma[8,6] +
   gamma[2,1]*gamma[3,2]*gamma[4,1]*gamma[6,3]*gamma[8,6] +
   gamma[2,1]*gamma[3,1]*gamma[4,2]*gamma[6,3]*gamma[8,6] +
   gamma[3,2]*gamma[4,2]*gamma[6,3]*gamma[8,6] + gamma[4,3]*gamma[6,3]*gamma[8,6] +
   gamma[6,4]*gamma[8,6] + gamma[4,1]*gamma[5,1]*gamma[6,5]*gamma[8,6] +
   gamma[2,1]*gamma[4,2]*gamma[5,1]*gamma[6,5]*gamma[8,6] +
   gamma[3,1]*gamma[4,3]*gamma[5,1]*gamma[6,5]*gamma[8,6] +
   gamma[2,1]*gamma[3,2]*gamma[4,3]*gamma[5,1]*gamma[6,5]*gamma[8,6] +
   gamma[2,1]*gamma[4,1]*gamma[5,2]*gamma[6,5]*gamma[8,6] +
   gamma[4,2]*gamma[5,2]*gamma[6,5]*gamma[8,6] +
   gamma[2,1]*gamma[3,1]*gamma[4,3]*gamma[5,2]*gamma[6,5]*gamma[8,6] +
   gamma[3,2]*gamma[4,3]*gamma[5,2]*gamma[6,5]*gamma[8,6] +
   gamma[3,1]*gamma[4,1]*gamma[5,3]*gamma[6,5]*gamma[8,6] +
   gamma[2,1]*gamma[3,2]*gamma[4,1]*gamma[5,3]*gamma[6,5]*gamma[8,6] +
   gamma[2,1]*gamma[3,1]*gamma[4,2]*gamma[5,3]*gamma[6,5]*gamma[8,6] +
   gamma[3,2]*gamma[4,2]*gamma[5,3]*gamma[6,5]*gamma[8,6] +
   gamma[4,3]*gamma[5,3]*gamma[6,5]*gamma[8,6] + gamma[5,4]*gamma[6,5]*gamma[8,6] +
   gamma[4,1]*gamma[7,1]*gamma[8,7] + gamma[2,1]*gamma[4,2]*gamma[7,1]*gamma[8,7] +
   gamma[3,1]*gamma[4,3]*gamma[7,1]*gamma[8,7] +
   gamma[2,1]*gamma[3,2]*gamma[4,3]*gamma[7,1]*gamma[8,7] +
   gamma[2,1]*gamma[4,1]*gamma[7,2]*gamma[8,7] + gamma[4,2]*gamma[7,2]*gamma[8,7] +
   gamma[2,1]*gamma[3,1]*gamma[4,3]*gamma[7,2]*gamma[8,7] +
   gamma[3,2]*gamma[4,3]*gamma[7,2]*gamma[8,7] + gamma[3,1]*gamma[4,1]*gamma[7,3]*gamma[8,7] +
   gamma[2,1]*gamma[3,2]*gamma[4,1]*gamma[7,3]*gamma[8,7] +
   gamma[2,1]*gamma[3,1]*gamma[4,2]*gamma[7,3]*gamma[8,7] +
   gamma[3,2]*gamma[4,2]*gamma[7,3]*gamma[8,7] + gamma[4,3]*gamma[7,3]*gamma[8,7] +
   gamma[7,4]*gamma[8,7] + gamma[4,1]*gamma[5,1]*gamma[7,5]*gamma[8,7] +
   gamma[2,1]*gamma[4,2]*gamma[5,1]*gamma[7,5]*gamma[8,7] +
   gamma[3,1]*gamma[4,3]*gamma[5,1]*gamma[7,5]*gamma[8,7] +
   gamma[2,1]*gamma[3,2]*gamma[4,3]*gamma[5,1]*gamma[7,5]*gamma[8,7] +
   gamma[2,1]*gamma[4,1]*gamma[5,2]*gamma[7,5]*gamma[8,7] +
   gamma[4,2]*gamma[5,2]*gamma[7,5]*gamma[8,7] +
   gamma[2,1]*gamma[3,1]*gamma[4,3]*gamma[5,2]*gamma[7,5]*gamma[8,7] +
   gamma[3,2]*gamma[4,3]*gamma[5,2]*gamma[7,5]*gamma[8,7] +
   gamma[3,1]*gamma[4,1]*gamma[5,3]*gamma[7,5]*gamma[8,7] +
   gamma[2,1]*gamma[3,2]*gamma[4,1]*gamma[5,3]*gamma[7,5]*gamma[8,7] +
   gamma[2,1]*gamma[3,1]*gamma[4,2]*gamma[5,3]*gamma[7,5]*gamma[8,7] +
   gamma[3,2]*gamma[4,2]*gamma[5,3]*gamma[7,5]*gamma[8,7] +
   gamma[4,3]*gamma[5,3]*gamma[7,5]*gamma[8,7] + gamma[5,4]*gamma[7,5]*gamma[8,7] +
   gamma[4,1]*gamma[6,1]*gamma[7,6]*gamma[8,7] +
   gamma[2,1]*gamma[4,2]*gamma[6,1]*gamma[7,6]*gamma[8,7] +
   gamma[3,1]*gamma[4,3]*gamma[6,1]*gamma[7,6]*gamma[8,7] +
   gamma[2,1]*gamma[3,2]*gamma[4,3]*gamma[6,1]*gamma[7,6]*gamma[8,7] +
   gamma[2,1]*gamma[4,1]*gamma[6,2]*gamma[7,6]*gamma[8,7] +
   gamma[4,2]*gamma[6,2]*gamma[7,6]*gamma[8,7] +
   gamma[2,1]*gamma[3,1]*gamma[4,3]*gamma[6,2]*gamma[7,6]*gamma[8,7] +
   gamma[3,2]*gamma[4,3]*gamma[6,2]*gamma[7,6]*gamma[8,7] +
   gamma[3,1]*gamma[4,1]*gamma[6,3]*gamma[7,6]*gamma[8,7] +
   gamma[2,1]*gamma[3,2]*gamma[4,1]*gamma[6,3]*gamma[7,6]*gamma[8,7] +
   gamma[2,1]*gamma[3,1]*gamma[4,2]*gamma[6,3]*gamma[7,6]*gamma[8,7] +
   gamma[3,2]*gamma[4,2]*gamma[6,3]*gamma[7,6]*gamma[8,7] +
   gamma[4,3]*gamma[6,3]*gamma[7,6]*gamma[8,7] + gamma[6,4]*gamma[7,6]*gamma[8,7] +
   gamma[4,1]*gamma[5,1]*gamma[6,5]*gamma[7,6]*gamma[8,7] +
   gamma[2,1]*gamma[4,2]*gamma[5,1]*gamma[6,5]*gamma[7,6]*gamma[8,7] +
   gamma[3,1]*gamma[4,3]*gamma[5,1]*gamma[6,5]*gamma[7,6]*gamma[8,7] +
   gamma[2,1]*gamma[3,2]*gamma[4,3]*gamma[5,1]*gamma[6,5]*gamma[7,6]*gamma[8,7] +
   gamma[2,1]*gamma[4,1]*gamma[5,2]*gamma[6,5]*gamma[7,6]*gamma[8,7] +
   gamma[4,2]*gamma[5,2]*gamma[6,5]*gamma[7,6]*gamma[8,7] +
   gamma[2,1]*gamma[3,1]*gamma[4,3]*gamma[5,2]*gamma[6,5]*gamma[7,6]*gamma[8,7] +
   gamma[3,2]*gamma[4,3]*gamma[5,2]*gamma[6,5]*gamma[7,6]*gamma[8,7] +
   gamma[3,1]*gamma[4,1]*gamma[5,3]*gamma[6,5]*gamma[7,6]*gamma[8,7] +
   gamma[2,1]*gamma[3,2]*gamma[4,1]*gamma[5,3]*gamma[6,5]*gamma[7,6]*gamma[8,7] +
   gamma[2,1]*gamma[3,1]*gamma[4,2]*gamma[5,3]*gamma[6,5]*gamma[7,6]*gamma[8,7] +
   gamma[3,2]*gamma[4,2]*gamma[5,3]*gamma[6,5]*gamma[7,6]*gamma[8,7] +
   gamma[4,3]*gamma[5,3]*gamma[6,5]*gamma[7,6]*gamma[8,7] +
   gamma[5,4]*gamma[6,5]*gamma[7,6]*gamma[8,7]
}


eta4eta9 <- function(gamma){

    gamma[4,1]*gamma[9,1] + gamma[2,1]*gamma[4,2]*gamma[9,1] +
    gamma[3,1]*gamma[4,3]*gamma[9,1] + gamma[2,1]*gamma[3,2]*gamma[4,3]*gamma[9,1] +
    gamma[2,1]*gamma[4,1]*gamma[9,2] + gamma[4,2]*gamma[9,2] +
    gamma[2,1]*gamma[3,1]*gamma[4,3]*gamma[9,2] + gamma[3,2]*gamma[4,3]*gamma[9,2] +
    gamma[3,1]*gamma[4,1]*gamma[9,3] + gamma[2,1]*gamma[3,2]*gamma[4,1]*gamma[9,3] +
    gamma[2,1]*gamma[3,1]*gamma[4,2]*gamma[9,3] + gamma[3,2]*gamma[4,2]*gamma[9,3] +
    gamma[4,3]*gamma[9,3] + gamma[9,4] + gamma[4,1]*gamma[5,1]*gamma[9,5] +
    gamma[2,1]*gamma[4,2]*gamma[5,1]*gamma[9,5] + gamma[3,1]*gamma[4,3]*gamma[5,1]*gamma[9,5] +
    gamma[2,1]*gamma[3,2]*gamma[4,3]*gamma[5,1]*gamma[9,5] +
    gamma[2,1]*gamma[4,1]*gamma[5,2]*gamma[9,5] + gamma[4,2]*gamma[5,2]*gamma[9,5] +
    gamma[2,1]*gamma[3,1]*gamma[4,3]*gamma[5,2]*gamma[9,5] +
    gamma[3,2]*gamma[4,3]*gamma[5,2]*gamma[9,5] + gamma[3,1]*gamma[4,1]*gamma[5,3]*gamma[9,5] +
    gamma[2,1]*gamma[3,2]*gamma[4,1]*gamma[5,3]*gamma[9,5] +
    gamma[2,1]*gamma[3,1]*gamma[4,2]*gamma[5,3]*gamma[9,5] +
    gamma[3,2]*gamma[4,2]*gamma[5,3]*gamma[9,5] + gamma[4,3]*gamma[5,3]*gamma[9,5] +
    gamma[5,4]*gamma[9,5] + gamma[4,1]*gamma[6,1]*gamma[9,6] +
    gamma[2,1]*gamma[4,2]*gamma[6,1]*gamma[9,6] + gamma[3,1]*gamma[4,3]*gamma[6,1]*gamma[9,6] +
    gamma[2,1]*gamma[3,2]*gamma[4,3]*gamma[6,1]*gamma[9,6] +
    gamma[2,1]*gamma[4,1]*gamma[6,2]*gamma[9,6] + gamma[4,2]*gamma[6,2]*gamma[9,6] +
    gamma[2,1]*gamma[3,1]*gamma[4,3]*gamma[6,2]*gamma[9,6] +
    gamma[3,2]*gamma[4,3]*gamma[6,2]*gamma[9,6] + gamma[3,1]*gamma[4,1]*gamma[6,3]*gamma[9,6] +
    gamma[2,1]*gamma[3,2]*gamma[4,1]*gamma[6,3]*gamma[9,6] +
    gamma[2,1]*gamma[3,1]*gamma[4,2]*gamma[6,3]*gamma[9,6] +
    gamma[3,2]*gamma[4,2]*gamma[6,3]*gamma[9,6] + gamma[4,3]*gamma[6,3]*gamma[9,6] +
    gamma[6,4]*gamma[9,6] + gamma[4,1]*gamma[5,1]*gamma[6,5]*gamma[9,6] +
    gamma[2,1]*gamma[4,2]*gamma[5,1]*gamma[6,5]*gamma[9,6] +
    gamma[3,1]*gamma[4,3]*gamma[5,1]*gamma[6,5]*gamma[9,6] +
    gamma[2,1]*gamma[3,2]*gamma[4,3]*gamma[5,1]*gamma[6,5]*gamma[9,6] +
    gamma[2,1]*gamma[4,1]*gamma[5,2]*gamma[6,5]*gamma[9,6] +
    gamma[4,2]*gamma[5,2]*gamma[6,5]*gamma[9,6] +
    gamma[2,1]*gamma[3,1]*gamma[4,3]*gamma[5,2]*gamma[6,5]*gamma[9,6] +
    gamma[3,2]*gamma[4,3]*gamma[5,2]*gamma[6,5]*gamma[9,6] +
    gamma[3,1]*gamma[4,1]*gamma[5,3]*gamma[6,5]*gamma[9,6] +
    gamma[2,1]*gamma[3,2]*gamma[4,1]*gamma[5,3]*gamma[6,5]*gamma[9,6] +
    gamma[2,1]*gamma[3,1]*gamma[4,2]*gamma[5,3]*gamma[6,5]*gamma[9,6] +
    gamma[3,2]*gamma[4,2]*gamma[5,3]*gamma[6,5]*gamma[9,6] +
    gamma[4,3]*gamma[5,3]*gamma[6,5]*gamma[9,6] + gamma[5,4]*gamma[6,5]*gamma[9,6] +
    gamma[4,1]*gamma[7,1]*gamma[9,7] + gamma[2,1]*gamma[4,2]*gamma[7,1]*gamma[9,7] +
    gamma[3,1]*gamma[4,3]*gamma[7,1]*gamma[9,7] +
    gamma[2,1]*gamma[3,2]*gamma[4,3]*gamma[7,1]*gamma[9,7] +
    gamma[2,1]*gamma[4,1]*gamma[7,2]*gamma[9,7] + gamma[4,2]*gamma[7,2]*gamma[9,7] +
    gamma[2,1]*gamma[3,1]*gamma[4,3]*gamma[7,2]*gamma[9,7] +
    gamma[3,2]*gamma[4,3]*gamma[7,2]*gamma[9,7] + gamma[3,1]*gamma[4,1]*gamma[7,3]*gamma[9,7] +
    gamma[2,1]*gamma[3,2]*gamma[4,1]*gamma[7,3]*gamma[9,7] +
    gamma[2,1]*gamma[3,1]*gamma[4,2]*gamma[7,3]*gamma[9,7] +
    gamma[3,2]*gamma[4,2]*gamma[7,3]*gamma[9,7] + gamma[4,3]*gamma[7,3]*gamma[9,7] +
    gamma[7,4]*gamma[9,7] + gamma[4,1]*gamma[5,1]*gamma[7,5]*gamma[9,7] +
    gamma[2,1]*gamma[4,2]*gamma[5,1]*gamma[7,5]*gamma[9,7] +
    gamma[3,1]*gamma[4,3]*gamma[5,1]*gamma[7,5]*gamma[9,7] +
    gamma[2,1]*gamma[3,2]*gamma[4,3]*gamma[5,1]*gamma[7,5]*gamma[9,7] +
    gamma[2,1]*gamma[4,1]*gamma[5,2]*gamma[7,5]*gamma[9,7] +
    gamma[4,2]*gamma[5,2]*gamma[7,5]*gamma[9,7] +
    gamma[2,1]*gamma[3,1]*gamma[4,3]*gamma[5,2]*gamma[7,5]*gamma[9,7] +
    gamma[3,2]*gamma[4,3]*gamma[5,2]*gamma[7,5]*gamma[9,7] +
    gamma[3,1]*gamma[4,1]*gamma[5,3]*gamma[7,5]*gamma[9,7] +
    gamma[2,1]*gamma[3,2]*gamma[4,1]*gamma[5,3]*gamma[7,5]*gamma[9,7] +
    gamma[2,1]*gamma[3,1]*gamma[4,2]*gamma[5,3]*gamma[7,5]*gamma[9,7] +
    gamma[3,2]*gamma[4,2]*gamma[5,3]*gamma[7,5]*gamma[9,7] +
    gamma[4,3]*gamma[5,3]*gamma[7,5]*gamma[9,7] + gamma[5,4]*gamma[7,5]*gamma[9,7] +
    gamma[4,1]*gamma[6,1]*gamma[7,6]*gamma[9,7] +
    gamma[2,1]*gamma[4,2]*gamma[6,1]*gamma[7,6]*gamma[9,7] +
    gamma[3,1]*gamma[4,3]*gamma[6,1]*gamma[7,6]*gamma[9,7] +
    gamma[2,1]*gamma[3,2]*gamma[4,3]*gamma[6,1]*gamma[7,6]*gamma[9,7] +
    gamma[2,1]*gamma[4,1]*gamma[6,2]*gamma[7,6]*gamma[9,7] +
    gamma[4,2]*gamma[6,2]*gamma[7,6]*gamma[9,7] +
    gamma[2,1]*gamma[3,1]*gamma[4,3]*gamma[6,2]*gamma[7,6]*gamma[9,7] +
    gamma[3,2]*gamma[4,3]*gamma[6,2]*gamma[7,6]*gamma[9,7] +
    gamma[3,1]*gamma[4,1]*gamma[6,3]*gamma[7,6]*gamma[9,7] +
    gamma[2,1]*gamma[3,2]*gamma[4,1]*gamma[6,3]*gamma[7,6]*gamma[9,7] +
    gamma[2,1]*gamma[3,1]*gamma[4,2]*gamma[6,3]*gamma[7,6]*gamma[9,7] +
    gamma[3,2]*gamma[4,2]*gamma[6,3]*gamma[7,6]*gamma[9,7] +
    gamma[4,3]*gamma[6,3]*gamma[7,6]*gamma[9,7] + gamma[6,4]*gamma[7,6]*gamma[9,7] +
    gamma[4,1]*gamma[5,1]*gamma[6,5]*gamma[7,6]*gamma[9,7] +
    gamma[2,1]*gamma[4,2]*gamma[5,1]*gamma[6,5]*gamma[7,6]*gamma[9,7] +
    gamma[3,1]*gamma[4,3]*gamma[5,1]*gamma[6,5]*gamma[7,6]*gamma[9,7] +
    gamma[2,1]*gamma[3,2]*gamma[4,3]*gamma[5,1]*gamma[6,5]*gamma[7,6]*gamma[9,7] +
    gamma[2,1]*gamma[4,1]*gamma[5,2]*gamma[6,5]*gamma[7,6]*gamma[9,7] +
    gamma[4,2]*gamma[5,2]*gamma[6,5]*gamma[7,6]*gamma[9,7] +
    gamma[2,1]*gamma[3,1]*gamma[4,3]*gamma[5,2]*gamma[6,5]*gamma[7,6]*gamma[9,7] +
    gamma[3,2]*gamma[4,3]*gamma[5,2]*gamma[6,5]*gamma[7,6]*gamma[9,7] +
    gamma[3,1]*gamma[4,1]*gamma[5,3]*gamma[6,5]*gamma[7,6]*gamma[9,7] +
    gamma[2,1]*gamma[3,2]*gamma[4,1]*gamma[5,3]*gamma[6,5]*gamma[7,6]*gamma[9,7] +
    gamma[2,1]*gamma[3,1]*gamma[4,2]*gamma[5,3]*gamma[6,5]*gamma[7,6]*gamma[9,7] +
    gamma[3,2]*gamma[4,2]*gamma[5,3]*gamma[6,5]*gamma[7,6]*gamma[9,7] +
    gamma[4,3]*gamma[5,3]*gamma[6,5]*gamma[7,6]*gamma[9,7] +
    gamma[5,4]*gamma[6,5]*gamma[7,6]*gamma[9,7] + gamma[4,1]*gamma[8,1]*gamma[9,8] +
    gamma[2,1]*gamma[4,2]*gamma[8,1]*gamma[9,8] + gamma[3,1]*gamma[4,3]*gamma[8,1]*gamma[9,8] +
    gamma[2,1]*gamma[3,2]*gamma[4,3]*gamma[8,1]*gamma[9,8] +
    gamma[2,1]*gamma[4,1]*gamma[8,2]*gamma[9,8] + gamma[4,2]*gamma[8,2]*gamma[9,8] +
    gamma[2,1]*gamma[3,1]*gamma[4,3]*gamma[8,2]*gamma[9,8] +
    gamma[3,2]*gamma[4,3]*gamma[8,2]*gamma[9,8] + gamma[3,1]*gamma[4,1]*gamma[8,3]*gamma[9,8] +
    gamma[2,1]*gamma[3,2]*gamma[4,1]*gamma[8,3]*gamma[9,8] +
    gamma[2,1]*gamma[3,1]*gamma[4,2]*gamma[8,3]*gamma[9,8] +
    gamma[3,2]*gamma[4,2]*gamma[8,3]*gamma[9,8] + gamma[4,3]*gamma[8,3]*gamma[9,8] +
    gamma[8,4]*gamma[9,8] + gamma[4,1]*gamma[5,1]*gamma[8,5]*gamma[9,8] +
    gamma[2,1]*gamma[4,2]*gamma[5,1]*gamma[8,5]*gamma[9,8] +
    gamma[3,1]*gamma[4,3]*gamma[5,1]*gamma[8,5]*gamma[9,8] +
    gamma[2,1]*gamma[3,2]*gamma[4,3]*gamma[5,1]*gamma[8,5]*gamma[9,8] +
    gamma[2,1]*gamma[4,1]*gamma[5,2]*gamma[8,5]*gamma[9,8] +
    gamma[4,2]*gamma[5,2]*gamma[8,5]*gamma[9,8] +
    gamma[2,1]*gamma[3,1]*gamma[4,3]*gamma[5,2]*gamma[8,5]*gamma[9,8] +
    gamma[3,2]*gamma[4,3]*gamma[5,2]*gamma[8,5]*gamma[9,8] +
    gamma[3,1]*gamma[4,1]*gamma[5,3]*gamma[8,5]*gamma[9,8] +
    gamma[2,1]*gamma[3,2]*gamma[4,1]*gamma[5,3]*gamma[8,5]*gamma[9,8] +
    gamma[2,1]*gamma[3,1]*gamma[4,2]*gamma[5,3]*gamma[8,5]*gamma[9,8] +
    gamma[3,2]*gamma[4,2]*gamma[5,3]*gamma[8,5]*gamma[9,8] +
    gamma[4,3]*gamma[5,3]*gamma[8,5]*gamma[9,8] + gamma[5,4]*gamma[8,5]*gamma[9,8] +
    gamma[4,1]*gamma[6,1]*gamma[8,6]*gamma[9,8] +
    gamma[2,1]*gamma[4,2]*gamma[6,1]*gamma[8,6]*gamma[9,8] +
    gamma[3,1]*gamma[4,3]*gamma[6,1]*gamma[8,6]*gamma[9,8] +
    gamma[2,1]*gamma[3,2]*gamma[4,3]*gamma[6,1]*gamma[8,6]*gamma[9,8] +
    gamma[2,1]*gamma[4,1]*gamma[6,2]*gamma[8,6]*gamma[9,8] +
    gamma[4,2]*gamma[6,2]*gamma[8,6]*gamma[9,8] +
    gamma[2,1]*gamma[3,1]*gamma[4,3]*gamma[6,2]*gamma[8,6]*gamma[9,8] +
    gamma[3,2]*gamma[4,3]*gamma[6,2]*gamma[8,6]*gamma[9,8] +
    gamma[3,1]*gamma[4,1]*gamma[6,3]*gamma[8,6]*gamma[9,8] +
    gamma[2,1]*gamma[3,2]*gamma[4,1]*gamma[6,3]*gamma[8,6]*gamma[9,8] +
    gamma[2,1]*gamma[3,1]*gamma[4,2]*gamma[6,3]*gamma[8,6]*gamma[9,8] +
    gamma[3,2]*gamma[4,2]*gamma[6,3]*gamma[8,6]*gamma[9,8] +
    gamma[4,3]*gamma[6,3]*gamma[8,6]*gamma[9,8] + gamma[6,4]*gamma[8,6]*gamma[9,8] +
    gamma[4,1]*gamma[5,1]*gamma[6,5]*gamma[8,6]*gamma[9,8] +
    gamma[2,1]*gamma[4,2]*gamma[5,1]*gamma[6,5]*gamma[8,6]*gamma[9,8] +
    gamma[3,1]*gamma[4,3]*gamma[5,1]*gamma[6,5]*gamma[8,6]*gamma[9,8] +
    gamma[2,1]*gamma[3,2]*gamma[4,3]*gamma[5,1]*gamma[6,5]*gamma[8,6]*gamma[9,8] +
    gamma[2,1]*gamma[4,1]*gamma[5,2]*gamma[6,5]*gamma[8,6]*gamma[9,8] +
    gamma[4,2]*gamma[5,2]*gamma[6,5]*gamma[8,6]*gamma[9,8] +
    gamma[2,1]*gamma[3,1]*gamma[4,3]*gamma[5,2]*gamma[6,5]*gamma[8,6]*gamma[9,8] +
    gamma[3,2]*gamma[4,3]*gamma[5,2]*gamma[6,5]*gamma[8,6]*gamma[9,8] +
    gamma[3,1]*gamma[4,1]*gamma[5,3]*gamma[6,5]*gamma[8,6]*gamma[9,8] +
    gamma[2,1]*gamma[3,2]*gamma[4,1]*gamma[5,3]*gamma[6,5]*gamma[8,6]*gamma[9,8] +
    gamma[2,1]*gamma[3,1]*gamma[4,2]*gamma[5,3]*gamma[6,5]*gamma[8,6]*gamma[9,8] +
    gamma[3,2]*gamma[4,2]*gamma[5,3]*gamma[6,5]*gamma[8,6]*gamma[9,8] +
    gamma[4,3]*gamma[5,3]*gamma[6,5]*gamma[8,6]*gamma[9,8] +
    gamma[5,4]*gamma[6,5]*gamma[8,6]*gamma[9,8] + gamma[4,1]*gamma[7,1]*gamma[8,7]*gamma[9,8] +
    gamma[2,1]*gamma[4,2]*gamma[7,1]*gamma[8,7]*gamma[9,8] +
    gamma[3,1]*gamma[4,3]*gamma[7,1]*gamma[8,7]*gamma[9,8] +
    gamma[2,1]*gamma[3,2]*gamma[4,3]*gamma[7,1]*gamma[8,7]*gamma[9,8] +
    gamma[2,1]*gamma[4,1]*gamma[7,2]*gamma[8,7]*gamma[9,8] +
    gamma[4,2]*gamma[7,2]*gamma[8,7]*gamma[9,8] +
    gamma[2,1]*gamma[3,1]*gamma[4,3]*gamma[7,2]*gamma[8,7]*gamma[9,8] +
    gamma[3,2]*gamma[4,3]*gamma[7,2]*gamma[8,7]*gamma[9,8] +
    gamma[3,1]*gamma[4,1]*gamma[7,3]*gamma[8,7]*gamma[9,8] +
    gamma[2,1]*gamma[3,2]*gamma[4,1]*gamma[7,3]*gamma[8,7]*gamma[9,8] +
    gamma[2,1]*gamma[3,1]*gamma[4,2]*gamma[7,3]*gamma[8,7]*gamma[9,8] +
    gamma[3,2]*gamma[4,2]*gamma[7,3]*gamma[8,7]*gamma[9,8] +
    gamma[4,3]*gamma[7,3]*gamma[8,7]*gamma[9,8] + gamma[7,4]*gamma[8,7]*gamma[9,8] +
    gamma[4,1]*gamma[5,1]*gamma[7,5]*gamma[8,7]*gamma[9,8] +
    gamma[2,1]*gamma[4,2]*gamma[5,1]*gamma[7,5]*gamma[8,7]*gamma[9,8] +
    gamma[3,1]*gamma[4,3]*gamma[5,1]*gamma[7,5]*gamma[8,7]*gamma[9,8] +
    gamma[2,1]*gamma[3,2]*gamma[4,3]*gamma[5,1]*gamma[7,5]*gamma[8,7]*gamma[9,8] +
    gamma[2,1]*gamma[4,1]*gamma[5,2]*gamma[7,5]*gamma[8,7]*gamma[9,8] +
    gamma[4,2]*gamma[5,2]*gamma[7,5]*gamma[8,7]*gamma[9,8] +
    gamma[2,1]*gamma[3,1]*gamma[4,3]*gamma[5,2]*gamma[7,5]*gamma[8,7]*gamma[9,8] +
    gamma[3,2]*gamma[4,3]*gamma[5,2]*gamma[7,5]*gamma[8,7]*gamma[9,8] +
    gamma[3,1]*gamma[4,1]*gamma[5,3]*gamma[7,5]*gamma[8,7]*gamma[9,8] +
    gamma[2,1]*gamma[3,2]*gamma[4,1]*gamma[5,3]*gamma[7,5]*gamma[8,7]*gamma[9,8] +
    gamma[2,1]*gamma[3,1]*gamma[4,2]*gamma[5,3]*gamma[7,5]*gamma[8,7]*gamma[9,8] +
    gamma[3,2]*gamma[4,2]*gamma[5,3]*gamma[7,5]*gamma[8,7]*gamma[9,8] +
    gamma[4,3]*gamma[5,3]*gamma[7,5]*gamma[8,7]*gamma[9,8] +
    gamma[5,4]*gamma[7,5]*gamma[8,7]*gamma[9,8] +
    gamma[4,1]*gamma[6,1]*gamma[7,6]*gamma[8,7]*gamma[9,8] +
    gamma[2,1]*gamma[4,2]*gamma[6,1]*gamma[7,6]*gamma[8,7]*gamma[9,8] +
    gamma[3,1]*gamma[4,3]*gamma[6,1]*gamma[7,6]*gamma[8,7]*gamma[9,8] +
    gamma[2,1]*gamma[3,2]*gamma[4,3]*gamma[6,1]*gamma[7,6]*gamma[8,7]*gamma[9,8] +
    gamma[2,1]*gamma[4,1]*gamma[6,2]*gamma[7,6]*gamma[8,7]*gamma[9,8] +
    gamma[4,2]*gamma[6,2]*gamma[7,6]*gamma[8,7]*gamma[9,8] +
    gamma[2,1]*gamma[3,1]*gamma[4,3]*gamma[6,2]*gamma[7,6]*gamma[8,7]*gamma[9,8] +
    gamma[3,2]*gamma[4,3]*gamma[6,2]*gamma[7,6]*gamma[8,7]*gamma[9,8] +
    gamma[3,1]*gamma[4,1]*gamma[6,3]*gamma[7,6]*gamma[8,7]*gamma[9,8] +
    gamma[2,1]*gamma[3,2]*gamma[4,1]*gamma[6,3]*gamma[7,6]*gamma[8,7]*gamma[9,8] +
    gamma[2,1]*gamma[3,1]*gamma[4,2]*gamma[6,3]*gamma[7,6]*gamma[8,7]*gamma[9,8] +
    gamma[3,2]*gamma[4,2]*gamma[6,3]*gamma[7,6]*gamma[8,7]*gamma[9,8] +
    gamma[4,3]*gamma[6,3]*gamma[7,6]*gamma[8,7]*gamma[9,8] +
    gamma[6,4]*gamma[7,6]*gamma[8,7]*gamma[9,8] +
    gamma[4,1]*gamma[5,1]*gamma[6,5]*gamma[7,6]*gamma[8,7]*gamma[9,8] +
    gamma[2,1]*gamma[4,2]*gamma[5,1]*gamma[6,5]*gamma[7,6]*gamma[8,7]*gamma[9,8] +
    gamma[3,1]*gamma[4,3]*gamma[5,1]*gamma[6,5]*gamma[7,6]*gamma[8,7]*gamma[9,8] +
    gamma[2,1]*gamma[3,2]*gamma[4,3]*gamma[5,1]*gamma[6,5]*gamma[7,6]*gamma[8,7]*gamma[9,8] +
    gamma[2,1]*gamma[4,1]*gamma[5,2]*gamma[6,5]*gamma[7,6]*gamma[8,7]*gamma[9,8] +
    gamma[4,2]*gamma[5,2]*gamma[6,5]*gamma[7,6]*gamma[8,7]*gamma[9,8] +
    gamma[2,1]*gamma[3,1]*gamma[4,3]*gamma[5,2]*gamma[6,5]*gamma[7,6]*gamma[8,7]*gamma[9,8] +
    gamma[3,2]*gamma[4,3]*gamma[5,2]*gamma[6,5]*gamma[7,6]*gamma[8,7]*gamma[9,8] +
    gamma[3,1]*gamma[4,1]*gamma[5,3]*gamma[6,5]*gamma[7,6]*gamma[8,7]*gamma[9,8] +
    gamma[2,1]*gamma[3,2]*gamma[4,1]*gamma[5,3]*gamma[6,5]*gamma[7,6]*gamma[8,7]*gamma[9,8] +
    gamma[2,1]*gamma[3,1]*gamma[4,2]*gamma[5,3]*gamma[6,5]*gamma[7,6]*gamma[8,7]*gamma[9,8] +
    gamma[3,2]*gamma[4,2]*gamma[5,3]*gamma[6,5]*gamma[7,6]*gamma[8,7]*gamma[9,8] +
    gamma[4,3]*gamma[5,3]*gamma[6,5]*gamma[7,6]*gamma[8,7]*gamma[9,8] +
    gamma[5,4]*gamma[6,5]*gamma[7,6]*gamma[8,7]*gamma[9,8]
}


eta4eta10 <- function(gamma){

    gamma[10,4] + gamma[10,1]*gamma[4,1] + gamma[10,2]*gamma[2,1]*gamma[4,1] +
    gamma[10,3]*gamma[3,1]*gamma[4,1] + gamma[10,3]*gamma[2,1]*gamma[3,2]*gamma[4,1] +
    gamma[10,2]*gamma[4,2] + gamma[10,1]*gamma[2,1]*gamma[4,2] +
    gamma[10,3]*gamma[2,1]*gamma[3,1]*gamma[4,2] + gamma[10,3]*gamma[3,2]*gamma[4,2] +
    gamma[10,3]*gamma[4,3] + gamma[10,1]*gamma[3,1]*gamma[4,3] +
    gamma[10,2]*gamma[2,1]*gamma[3,1]*gamma[4,3] + gamma[10,2]*gamma[3,2]*gamma[4,3] +
    gamma[10,1]*gamma[2,1]*gamma[3,2]*gamma[4,3] + gamma[10,5]*gamma[4,1]*gamma[5,1] +
    gamma[10,5]*gamma[2,1]*gamma[4,2]*gamma[5,1] + gamma[10,5]*gamma[3,1]*gamma[4,3]*gamma[5,1] +
    gamma[10,5]*gamma[2,1]*gamma[3,2]*gamma[4,3]*gamma[5,1] +
    gamma[10,5]*gamma[2,1]*gamma[4,1]*gamma[5,2] + gamma[10,5]*gamma[4,2]*gamma[5,2] +
    gamma[10,5]*gamma[2,1]*gamma[3,1]*gamma[4,3]*gamma[5,2] +
    gamma[10,5]*gamma[3,2]*gamma[4,3]*gamma[5,2] + gamma[10,5]*gamma[3,1]*gamma[4,1]*gamma[5,3] +
    gamma[10,5]*gamma[2,1]*gamma[3,2]*gamma[4,1]*gamma[5,3] +
    gamma[10,5]*gamma[2,1]*gamma[3,1]*gamma[4,2]*gamma[5,3] +
    gamma[10,5]*gamma[3,2]*gamma[4,2]*gamma[5,3] + gamma[10,5]*gamma[4,3]*gamma[5,3] +
    gamma[10,5]*gamma[5,4] + gamma[10,6]*gamma[4,1]*gamma[6,1] +
    gamma[10,6]*gamma[2,1]*gamma[4,2]*gamma[6,1] + gamma[10,6]*gamma[3,1]*gamma[4,3]*gamma[6,1] +
    gamma[10,6]*gamma[2,1]*gamma[3,2]*gamma[4,3]*gamma[6,1] +
    gamma[10,6]*gamma[2,1]*gamma[4,1]*gamma[6,2] + gamma[10,6]*gamma[4,2]*gamma[6,2] +
    gamma[10,6]*gamma[2,1]*gamma[3,1]*gamma[4,3]*gamma[6,2] +
    gamma[10,6]*gamma[3,2]*gamma[4,3]*gamma[6,2] + gamma[10,6]*gamma[3,1]*gamma[4,1]*gamma[6,3] +
    gamma[10,6]*gamma[2,1]*gamma[3,2]*gamma[4,1]*gamma[6,3] +
    gamma[10,6]*gamma[2,1]*gamma[3,1]*gamma[4,2]*gamma[6,3] +
    gamma[10,6]*gamma[3,2]*gamma[4,2]*gamma[6,3] + gamma[10,6]*gamma[4,3]*gamma[6,3] +
    gamma[10,6]*gamma[6,4] + gamma[10,6]*gamma[4,1]*gamma[5,1]*gamma[6,5] +
    gamma[10,6]*gamma[2,1]*gamma[4,2]*gamma[5,1]*gamma[6,5] +
    gamma[10,6]*gamma[3,1]*gamma[4,3]*gamma[5,1]*gamma[6,5] +
    gamma[10,6]*gamma[2,1]*gamma[3,2]*gamma[4,3]*gamma[5,1]*gamma[6,5] +
    gamma[10,6]*gamma[2,1]*gamma[4,1]*gamma[5,2]*gamma[6,5] +
    gamma[10,6]*gamma[4,2]*gamma[5,2]*gamma[6,5] +
    gamma[10,6]*gamma[2,1]*gamma[3,1]*gamma[4,3]*gamma[5,2]*gamma[6,5] +
    gamma[10,6]*gamma[3,2]*gamma[4,3]*gamma[5,2]*gamma[6,5] +
    gamma[10,6]*gamma[3,1]*gamma[4,1]*gamma[5,3]*gamma[6,5] +
    gamma[10,6]*gamma[2,1]*gamma[3,2]*gamma[4,1]*gamma[5,3]*gamma[6,5] +
    gamma[10,6]*gamma[2,1]*gamma[3,1]*gamma[4,2]*gamma[5,3]*gamma[6,5] +
    gamma[10,6]*gamma[3,2]*gamma[4,2]*gamma[5,3]*gamma[6,5] +
    gamma[10,6]*gamma[4,3]*gamma[5,3]*gamma[6,5] + gamma[10,6]*gamma[5,4]*gamma[6,5] +
    gamma[10,7]*gamma[4,1]*gamma[7,1] + gamma[10,7]*gamma[2,1]*gamma[4,2]*gamma[7,1] +
    gamma[10,7]*gamma[3,1]*gamma[4,3]*gamma[7,1] +
    gamma[10,7]*gamma[2,1]*gamma[3,2]*gamma[4,3]*gamma[7,1] +
    gamma[10,7]*gamma[2,1]*gamma[4,1]*gamma[7,2] + gamma[10,7]*gamma[4,2]*gamma[7,2] +
    gamma[10,7]*gamma[2,1]*gamma[3,1]*gamma[4,3]*gamma[7,2] +
    gamma[10,7]*gamma[3,2]*gamma[4,3]*gamma[7,2] + gamma[10,7]*gamma[3,1]*gamma[4,1]*gamma[7,3] +
    gamma[10,7]*gamma[2,1]*gamma[3,2]*gamma[4,1]*gamma[7,3] +
    gamma[10,7]*gamma[2,1]*gamma[3,1]*gamma[4,2]*gamma[7,3] +
    gamma[10,7]*gamma[3,2]*gamma[4,2]*gamma[7,3] + gamma[10,7]*gamma[4,3]*gamma[7,3] +
    gamma[10,7]*gamma[7,4] + gamma[10,7]*gamma[4,1]*gamma[5,1]*gamma[7,5] +
    gamma[10,7]*gamma[2,1]*gamma[4,2]*gamma[5,1]*gamma[7,5] +
    gamma[10,7]*gamma[3,1]*gamma[4,3]*gamma[5,1]*gamma[7,5] +
    gamma[10,7]*gamma[2,1]*gamma[3,2]*gamma[4,3]*gamma[5,1]*gamma[7,5] +
    gamma[10,7]*gamma[2,1]*gamma[4,1]*gamma[5,2]*gamma[7,5] +
    gamma[10,7]*gamma[4,2]*gamma[5,2]*gamma[7,5] +
    gamma[10,7]*gamma[2,1]*gamma[3,1]*gamma[4,3]*gamma[5,2]*gamma[7,5] +
    gamma[10,7]*gamma[3,2]*gamma[4,3]*gamma[5,2]*gamma[7,5] +
    gamma[10,7]*gamma[3,1]*gamma[4,1]*gamma[5,3]*gamma[7,5] +
    gamma[10,7]*gamma[2,1]*gamma[3,2]*gamma[4,1]*gamma[5,3]*gamma[7,5] +
    gamma[10,7]*gamma[2,1]*gamma[3,1]*gamma[4,2]*gamma[5,3]*gamma[7,5] +
    gamma[10,7]*gamma[3,2]*gamma[4,2]*gamma[5,3]*gamma[7,5] +
    gamma[10,7]*gamma[4,3]*gamma[5,3]*gamma[7,5] + gamma[10,7]*gamma[5,4]*gamma[7,5] +
    gamma[10,7]*gamma[4,1]*gamma[6,1]*gamma[7,6] +
    gamma[10,7]*gamma[2,1]*gamma[4,2]*gamma[6,1]*gamma[7,6] +
    gamma[10,7]*gamma[3,1]*gamma[4,3]*gamma[6,1]*gamma[7,6] +
    gamma[10,7]*gamma[2,1]*gamma[3,2]*gamma[4,3]*gamma[6,1]*gamma[7,6] +
    gamma[10,7]*gamma[2,1]*gamma[4,1]*gamma[6,2]*gamma[7,6] +
    gamma[10,7]*gamma[4,2]*gamma[6,2]*gamma[7,6] +
    gamma[10,7]*gamma[2,1]*gamma[3,1]*gamma[4,3]*gamma[6,2]*gamma[7,6] +
    gamma[10,7]*gamma[3,2]*gamma[4,3]*gamma[6,2]*gamma[7,6] +
    gamma[10,7]*gamma[3,1]*gamma[4,1]*gamma[6,3]*gamma[7,6] +
    gamma[10,7]*gamma[2,1]*gamma[3,2]*gamma[4,1]*gamma[6,3]*gamma[7,6] +
    gamma[10,7]*gamma[2,1]*gamma[3,1]*gamma[4,2]*gamma[6,3]*gamma[7,6] +
    gamma[10,7]*gamma[3,2]*gamma[4,2]*gamma[6,3]*gamma[7,6] +
    gamma[10,7]*gamma[4,3]*gamma[6,3]*gamma[7,6] + gamma[10,7]*gamma[6,4]*gamma[7,6] +
    gamma[10,7]*gamma[4,1]*gamma[5,1]*gamma[6,5]*gamma[7,6] +
    gamma[10,7]*gamma[2,1]*gamma[4,2]*gamma[5,1]*gamma[6,5]*gamma[7,6] +
    gamma[10,7]*gamma[3,1]*gamma[4,3]*gamma[5,1]*gamma[6,5]*gamma[7,6] +
    gamma[10,7]*gamma[2,1]*gamma[3,2]*gamma[4,3]*gamma[5,1]*gamma[6,5]*gamma[7,6] +
    gamma[10,7]*gamma[2,1]*gamma[4,1]*gamma[5,2]*gamma[6,5]*gamma[7,6] +
    gamma[10,7]*gamma[4,2]*gamma[5,2]*gamma[6,5]*gamma[7,6] +
    gamma[10,7]*gamma[2,1]*gamma[3,1]*gamma[4,3]*gamma[5,2]*gamma[6,5]*gamma[7,6] +
    gamma[10,7]*gamma[3,2]*gamma[4,3]*gamma[5,2]*gamma[6,5]*gamma[7,6] +
    gamma[10,7]*gamma[3,1]*gamma[4,1]*gamma[5,3]*gamma[6,5]*gamma[7,6] +
    gamma[10,7]*gamma[2,1]*gamma[3,2]*gamma[4,1]*gamma[5,3]*gamma[6,5]*gamma[7,6] +
    gamma[10,7]*gamma[2,1]*gamma[3,1]*gamma[4,2]*gamma[5,3]*gamma[6,5]*gamma[7,6] +
    gamma[10,7]*gamma[3,2]*gamma[4,2]*gamma[5,3]*gamma[6,5]*gamma[7,6] +
    gamma[10,7]*gamma[4,3]*gamma[5,3]*gamma[6,5]*gamma[7,6] +
    gamma[10,7]*gamma[5,4]*gamma[6,5]*gamma[7,6] + gamma[10,8]*gamma[4,1]*gamma[8,1] +
    gamma[10,8]*gamma[2,1]*gamma[4,2]*gamma[8,1] + gamma[10,8]*gamma[3,1]*gamma[4,3]*gamma[8,1] +
    gamma[10,8]*gamma[2,1]*gamma[3,2]*gamma[4,3]*gamma[8,1] +
    gamma[10,8]*gamma[2,1]*gamma[4,1]*gamma[8,2] + gamma[10,8]*gamma[4,2]*gamma[8,2] +
    gamma[10,8]*gamma[2,1]*gamma[3,1]*gamma[4,3]*gamma[8,2] +
    gamma[10,8]*gamma[3,2]*gamma[4,3]*gamma[8,2] + gamma[10,8]*gamma[3,1]*gamma[4,1]*gamma[8,3] +
    gamma[10,8]*gamma[2,1]*gamma[3,2]*gamma[4,1]*gamma[8,3] +
    gamma[10,8]*gamma[2,1]*gamma[3,1]*gamma[4,2]*gamma[8,3] +
    gamma[10,8]*gamma[3,2]*gamma[4,2]*gamma[8,3] + gamma[10,8]*gamma[4,3]*gamma[8,3] +
    gamma[10,8]*gamma[8,4] + gamma[10,8]*gamma[4,1]*gamma[5,1]*gamma[8,5] +
    gamma[10,8]*gamma[2,1]*gamma[4,2]*gamma[5,1]*gamma[8,5] +
    gamma[10,8]*gamma[3,1]*gamma[4,3]*gamma[5,1]*gamma[8,5] +
    gamma[10,8]*gamma[2,1]*gamma[3,2]*gamma[4,3]*gamma[5,1]*gamma[8,5] +
    gamma[10,8]*gamma[2,1]*gamma[4,1]*gamma[5,2]*gamma[8,5] +
    gamma[10,8]*gamma[4,2]*gamma[5,2]*gamma[8,5] +
    gamma[10,8]*gamma[2,1]*gamma[3,1]*gamma[4,3]*gamma[5,2]*gamma[8,5] +
    gamma[10,8]*gamma[3,2]*gamma[4,3]*gamma[5,2]*gamma[8,5] +
    gamma[10,8]*gamma[3,1]*gamma[4,1]*gamma[5,3]*gamma[8,5] +
    gamma[10,8]*gamma[2,1]*gamma[3,2]*gamma[4,1]*gamma[5,3]*gamma[8,5] +
    gamma[10,8]*gamma[2,1]*gamma[3,1]*gamma[4,2]*gamma[5,3]*gamma[8,5] +
    gamma[10,8]*gamma[3,2]*gamma[4,2]*gamma[5,3]*gamma[8,5] +
    gamma[10,8]*gamma[4,3]*gamma[5,3]*gamma[8,5] + gamma[10,8]*gamma[5,4]*gamma[8,5] +
    gamma[10,8]*gamma[4,1]*gamma[6,1]*gamma[8,6] +
    gamma[10,8]*gamma[2,1]*gamma[4,2]*gamma[6,1]*gamma[8,6] +
    gamma[10,8]*gamma[3,1]*gamma[4,3]*gamma[6,1]*gamma[8,6] +
    gamma[10,8]*gamma[2,1]*gamma[3,2]*gamma[4,3]*gamma[6,1]*gamma[8,6] +
    gamma[10,8]*gamma[2,1]*gamma[4,1]*gamma[6,2]*gamma[8,6] +
    gamma[10,8]*gamma[4,2]*gamma[6,2]*gamma[8,6] +
    gamma[10,8]*gamma[2,1]*gamma[3,1]*gamma[4,3]*gamma[6,2]*gamma[8,6] +
    gamma[10,8]*gamma[3,2]*gamma[4,3]*gamma[6,2]*gamma[8,6] +
    gamma[10,8]*gamma[3,1]*gamma[4,1]*gamma[6,3]*gamma[8,6] +
    gamma[10,8]*gamma[2,1]*gamma[3,2]*gamma[4,1]*gamma[6,3]*gamma[8,6] +
    gamma[10,8]*gamma[2,1]*gamma[3,1]*gamma[4,2]*gamma[6,3]*gamma[8,6] +
    gamma[10,8]*gamma[3,2]*gamma[4,2]*gamma[6,3]*gamma[8,6] +
    gamma[10,8]*gamma[4,3]*gamma[6,3]*gamma[8,6] + gamma[10,8]*gamma[6,4]*gamma[8,6] +
    gamma[10,8]*gamma[4,1]*gamma[5,1]*gamma[6,5]*gamma[8,6] +
    gamma[10,8]*gamma[2,1]*gamma[4,2]*gamma[5,1]*gamma[6,5]*gamma[8,6] +
    gamma[10,8]*gamma[3,1]*gamma[4,3]*gamma[5,1]*gamma[6,5]*gamma[8,6] +
    gamma[10,8]*gamma[2,1]*gamma[3,2]*gamma[4,3]*gamma[5,1]*gamma[6,5]*gamma[8,6] +
    gamma[10,8]*gamma[2,1]*gamma[4,1]*gamma[5,2]*gamma[6,5]*gamma[8,6] +
    gamma[10,8]*gamma[4,2]*gamma[5,2]*gamma[6,5]*gamma[8,6] +
    gamma[10,8]*gamma[2,1]*gamma[3,1]*gamma[4,3]*gamma[5,2]*gamma[6,5]*gamma[8,6] +
    gamma[10,8]*gamma[3,2]*gamma[4,3]*gamma[5,2]*gamma[6,5]*gamma[8,6] +
    gamma[10,8]*gamma[3,1]*gamma[4,1]*gamma[5,3]*gamma[6,5]*gamma[8,6] +
    gamma[10,8]*gamma[2,1]*gamma[3,2]*gamma[4,1]*gamma[5,3]*gamma[6,5]*gamma[8,6] +
    gamma[10,8]*gamma[2,1]*gamma[3,1]*gamma[4,2]*gamma[5,3]*gamma[6,5]*gamma[8,6] +
    gamma[10,8]*gamma[3,2]*gamma[4,2]*gamma[5,3]*gamma[6,5]*gamma[8,6] +
    gamma[10,8]*gamma[4,3]*gamma[5,3]*gamma[6,5]*gamma[8,6] +
    gamma[10,8]*gamma[5,4]*gamma[6,5]*gamma[8,6] + gamma[10,8]*gamma[4,1]*gamma[7,1]*gamma[8,7] +
    gamma[10,8]*gamma[2,1]*gamma[4,2]*gamma[7,1]*gamma[8,7] +
    gamma[10,8]*gamma[3,1]*gamma[4,3]*gamma[7,1]*gamma[8,7] +
    gamma[10,8]*gamma[2,1]*gamma[3,2]*gamma[4,3]*gamma[7,1]*gamma[8,7] +
    gamma[10,8]*gamma[2,1]*gamma[4,1]*gamma[7,2]*gamma[8,7] +
    gamma[10,8]*gamma[4,2]*gamma[7,2]*gamma[8,7] +
    gamma[10,8]*gamma[2,1]*gamma[3,1]*gamma[4,3]*gamma[7,2]*gamma[8,7] +
    gamma[10,8]*gamma[3,2]*gamma[4,3]*gamma[7,2]*gamma[8,7] +
    gamma[10,8]*gamma[3,1]*gamma[4,1]*gamma[7,3]*gamma[8,7] +
    gamma[10,8]*gamma[2,1]*gamma[3,2]*gamma[4,1]*gamma[7,3]*gamma[8,7] +
    gamma[10,8]*gamma[2,1]*gamma[3,1]*gamma[4,2]*gamma[7,3]*gamma[8,7] +
    gamma[10,8]*gamma[3,2]*gamma[4,2]*gamma[7,3]*gamma[8,7] +
    gamma[10,8]*gamma[4,3]*gamma[7,3]*gamma[8,7] + gamma[10,8]*gamma[7,4]*gamma[8,7] +
    gamma[10,8]*gamma[4,1]*gamma[5,1]*gamma[7,5]*gamma[8,7] +
    gamma[10,8]*gamma[2,1]*gamma[4,2]*gamma[5,1]*gamma[7,5]*gamma[8,7] +
    gamma[10,8]*gamma[3,1]*gamma[4,3]*gamma[5,1]*gamma[7,5]*gamma[8,7] +
    gamma[10,8]*gamma[2,1]*gamma[3,2]*gamma[4,3]*gamma[5,1]*gamma[7,5]*gamma[8,7] +
    gamma[10,8]*gamma[2,1]*gamma[4,1]*gamma[5,2]*gamma[7,5]*gamma[8,7] +
    gamma[10,8]*gamma[4,2]*gamma[5,2]*gamma[7,5]*gamma[8,7] +
    gamma[10,8]*gamma[2,1]*gamma[3,1]*gamma[4,3]*gamma[5,2]*gamma[7,5]*gamma[8,7] +
    gamma[10,8]*gamma[3,2]*gamma[4,3]*gamma[5,2]*gamma[7,5]*gamma[8,7] +
    gamma[10,8]*gamma[3,1]*gamma[4,1]*gamma[5,3]*gamma[7,5]*gamma[8,7] +
    gamma[10,8]*gamma[2,1]*gamma[3,2]*gamma[4,1]*gamma[5,3]*gamma[7,5]*gamma[8,7] +
    gamma[10,8]*gamma[2,1]*gamma[3,1]*gamma[4,2]*gamma[5,3]*gamma[7,5]*gamma[8,7] +
    gamma[10,8]*gamma[3,2]*gamma[4,2]*gamma[5,3]*gamma[7,5]*gamma[8,7] +
    gamma[10,8]*gamma[4,3]*gamma[5,3]*gamma[7,5]*gamma[8,7] +
    gamma[10,8]*gamma[5,4]*gamma[7,5]*gamma[8,7] +
    gamma[10,8]*gamma[4,1]*gamma[6,1]*gamma[7,6]*gamma[8,7] +
    gamma[10,8]*gamma[2,1]*gamma[4,2]*gamma[6,1]*gamma[7,6]*gamma[8,7] +
    gamma[10,8]*gamma[3,1]*gamma[4,3]*gamma[6,1]*gamma[7,6]*gamma[8,7] +
    gamma[10,8]*gamma[2,1]*gamma[3,2]*gamma[4,3]*gamma[6,1]*gamma[7,6]*gamma[8,7] +
    gamma[10,8]*gamma[2,1]*gamma[4,1]*gamma[6,2]*gamma[7,6]*gamma[8,7] +
    gamma[10,8]*gamma[4,2]*gamma[6,2]*gamma[7,6]*gamma[8,7] +
    gamma[10,8]*gamma[2,1]*gamma[3,1]*gamma[4,3]*gamma[6,2]*gamma[7,6]*gamma[8,7] +
    gamma[10,8]*gamma[3,2]*gamma[4,3]*gamma[6,2]*gamma[7,6]*gamma[8,7] +
    gamma[10,8]*gamma[3,1]*gamma[4,1]*gamma[6,3]*gamma[7,6]*gamma[8,7] +
    gamma[10,8]*gamma[2,1]*gamma[3,2]*gamma[4,1]*gamma[6,3]*gamma[7,6]*gamma[8,7] +
    gamma[10,8]*gamma[2,1]*gamma[3,1]*gamma[4,2]*gamma[6,3]*gamma[7,6]*gamma[8,7] +
    gamma[10,8]*gamma[3,2]*gamma[4,2]*gamma[6,3]*gamma[7,6]*gamma[8,7] +
    gamma[10,8]*gamma[4,3]*gamma[6,3]*gamma[7,6]*gamma[8,7] +
    gamma[10,8]*gamma[6,4]*gamma[7,6]*gamma[8,7] +
    gamma[10,8]*gamma[4,1]*gamma[5,1]*gamma[6,5]*gamma[7,6]*gamma[8,7] +
    gamma[10,8]*gamma[2,1]*gamma[4,2]*gamma[5,1]*gamma[6,5]*gamma[7,6]*gamma[8,7] +
    gamma[10,8]*gamma[3,1]*gamma[4,3]*gamma[5,1]*gamma[6,5]*gamma[7,6]*gamma[8,7] +
    gamma[10,8]*gamma[2,1]*gamma[3,2]*gamma[4,3]*gamma[5,1]*gamma[6,5]*gamma[7,6]*gamma[8,7] +
    gamma[10,8]*gamma[2,1]*gamma[4,1]*gamma[5,2]*gamma[6,5]*gamma[7,6]*gamma[8,7] +
    gamma[10,8]*gamma[4,2]*gamma[5,2]*gamma[6,5]*gamma[7,6]*gamma[8,7] +
    gamma[10,8]*gamma[2,1]*gamma[3,1]*gamma[4,3]*gamma[5,2]*gamma[6,5]*gamma[7,6]*gamma[8,7] +
    gamma[10,8]*gamma[3,2]*gamma[4,3]*gamma[5,2]*gamma[6,5]*gamma[7,6]*gamma[8,7] +
    gamma[10,8]*gamma[3,1]*gamma[4,1]*gamma[5,3]*gamma[6,5]*gamma[7,6]*gamma[8,7] +
    gamma[10,8]*gamma[2,1]*gamma[3,2]*gamma[4,1]*gamma[5,3]*gamma[6,5]*gamma[7,6]*gamma[8,7] +
    gamma[10,8]*gamma[2,1]*gamma[3,1]*gamma[4,2]*gamma[5,3]*gamma[6,5]*gamma[7,6]*gamma[8,7] +
    gamma[10,8]*gamma[3,2]*gamma[4,2]*gamma[5,3]*gamma[6,5]*gamma[7,6]*gamma[8,7] +
    gamma[10,8]*gamma[4,3]*gamma[5,3]*gamma[6,5]*gamma[7,6]*gamma[8,7] +
    gamma[10,8]*gamma[5,4]*gamma[6,5]*gamma[7,6]*gamma[8,7] + gamma[10,9]*gamma[4,1]*gamma[9,1] +
    gamma[10,9]*gamma[2,1]*gamma[4,2]*gamma[9,1] +
    gamma[10,9]*gamma[3,1]*gamma[4,3]*gamma[9,1] +
    gamma[10,9]*gamma[2,1]*gamma[3,2]*gamma[4,3]*gamma[9,1] +
    gamma[10,9]*gamma[2,1]*gamma[4,1]*gamma[9,2] + gamma[10,9]*gamma[4,2]*gamma[9,2] +
    gamma[10,9]*gamma[2,1]*gamma[3,1]*gamma[4,3]*gamma[9,2] +
    gamma[10,9]*gamma[3,2]*gamma[4,3]*gamma[9,2] + gamma[10,9]*gamma[3,1]*gamma[4,1]*gamma[9,3] +
    gamma[10,9]*gamma[2,1]*gamma[3,2]*gamma[4,1]*gamma[9,3] +
    gamma[10,9]*gamma[2,1]*gamma[3,1]*gamma[4,2]*gamma[9,3] +
    gamma[10,9]*gamma[3,2]*gamma[4,2]*gamma[9,3] + gamma[10,9]*gamma[4,3]*gamma[9,3] +
    gamma[10,9]*gamma[9,4] + gamma[10,9]*gamma[4,1]*gamma[5,1]*gamma[9,5] +
    gamma[10,9]*gamma[2,1]*gamma[4,2]*gamma[5,1]*gamma[9,5] +
    gamma[10,9]*gamma[3,1]*gamma[4,3]*gamma[5,1]*gamma[9,5] +
    gamma[10,9]*gamma[2,1]*gamma[3,2]*gamma[4,3]*gamma[5,1]*gamma[9,5] +
    gamma[10,9]*gamma[2,1]*gamma[4,1]*gamma[5,2]*gamma[9,5] +
    gamma[10,9]*gamma[4,2]*gamma[5,2]*gamma[9,5] +
    gamma[10,9]*gamma[2,1]*gamma[3,1]*gamma[4,3]*gamma[5,2]*gamma[9,5] +
    gamma[10,9]*gamma[3,2]*gamma[4,3]*gamma[5,2]*gamma[9,5] +
    gamma[10,9]*gamma[3,1]*gamma[4,1]*gamma[5,3]*gamma[9,5] +
    gamma[10,9]*gamma[2,1]*gamma[3,2]*gamma[4,1]*gamma[5,3]*gamma[9,5] +
    gamma[10,9]*gamma[2,1]*gamma[3,1]*gamma[4,2]*gamma[5,3]*gamma[9,5] +
    gamma[10,9]*gamma[3,2]*gamma[4,2]*gamma[5,3]*gamma[9,5] +
    gamma[10,9]*gamma[4,3]*gamma[5,3]*gamma[9,5] + gamma[10,9]*gamma[5,4]*gamma[9,5] +
    gamma[10,9]*gamma[4,1]*gamma[6,1]*gamma[9,6] +
    gamma[10,9]*gamma[2,1]*gamma[4,2]*gamma[6,1]*gamma[9,6] +
    gamma[10,9]*gamma[3,1]*gamma[4,3]*gamma[6,1]*gamma[9,6] +
    gamma[10,9]*gamma[2,1]*gamma[3,2]*gamma[4,3]*gamma[6,1]*gamma[9,6] +
    gamma[10,9]*gamma[2,1]*gamma[4,1]*gamma[6,2]*gamma[9,6] +
    gamma[10,9]*gamma[4,2]*gamma[6,2]*gamma[9,6] +
    gamma[10,9]*gamma[2,1]*gamma[3,1]*gamma[4,3]*gamma[6,2]*gamma[9,6] +
    gamma[10,9]*gamma[3,2]*gamma[4,3]*gamma[6,2]*gamma[9,6] +
    gamma[10,9]*gamma[3,1]*gamma[4,1]*gamma[6,3]*gamma[9,6] +
    gamma[10,9]*gamma[2,1]*gamma[3,2]*gamma[4,1]*gamma[6,3]*gamma[9,6] +
    gamma[10,9]*gamma[2,1]*gamma[3,1]*gamma[4,2]*gamma[6,3]*gamma[9,6] +
    gamma[10,9]*gamma[3,2]*gamma[4,2]*gamma[6,3]*gamma[9,6] +
    gamma[10,9]*gamma[4,3]*gamma[6,3]*gamma[9,6] + gamma[10,9]*gamma[6,4]*gamma[9,6] +
    gamma[10,9]*gamma[4,1]*gamma[5,1]*gamma[6,5]*gamma[9,6] +
    gamma[10,9]*gamma[2,1]*gamma[4,2]*gamma[5,1]*gamma[6,5]*gamma[9,6] +
    gamma[10,9]*gamma[3,1]*gamma[4,3]*gamma[5,1]*gamma[6,5]*gamma[9,6] +
    gamma[10,9]*gamma[2,1]*gamma[3,2]*gamma[4,3]*gamma[5,1]*gamma[6,5]*gamma[9,6] +
    gamma[10,9]*gamma[2,1]*gamma[4,1]*gamma[5,2]*gamma[6,5]*gamma[9,6] +
    gamma[10,9]*gamma[4,2]*gamma[5,2]*gamma[6,5]*gamma[9,6] +
    gamma[10,9]*gamma[2,1]*gamma[3,1]*gamma[4,3]*gamma[5,2]*gamma[6,5]*gamma[9,6] +
    gamma[10,9]*gamma[3,2]*gamma[4,3]*gamma[5,2]*gamma[6,5]*gamma[9,6] +
    gamma[10,9]*gamma[3,1]*gamma[4,1]*gamma[5,3]*gamma[6,5]*gamma[9,6] +
    gamma[10,9]*gamma[2,1]*gamma[3,2]*gamma[4,1]*gamma[5,3]*gamma[6,5]*gamma[9,6] +
    gamma[10,9]*gamma[2,1]*gamma[3,1]*gamma[4,2]*gamma[5,3]*gamma[6,5]*gamma[9,6] +
    gamma[10,9]*gamma[3,2]*gamma[4,2]*gamma[5,3]*gamma[6,5]*gamma[9,6] +
    gamma[10,9]*gamma[4,3]*gamma[5,3]*gamma[6,5]*gamma[9,6] +
    gamma[10,9]*gamma[5,4]*gamma[6,5]*gamma[9,6] + gamma[10,9]*gamma[4,1]*gamma[7,1]*gamma[9,7] +
    gamma[10,9]*gamma[2,1]*gamma[4,2]*gamma[7,1]*gamma[9,7] +
    gamma[10,9]*gamma[3,1]*gamma[4,3]*gamma[7,1]*gamma[9,7] +
    gamma[10,9]*gamma[2,1]*gamma[3,2]*gamma[4,3]*gamma[7,1]*gamma[9,7] +
    gamma[10,9]*gamma[2,1]*gamma[4,1]*gamma[7,2]*gamma[9,7] +
    gamma[10,9]*gamma[4,2]*gamma[7,2]*gamma[9,7] +
    gamma[10,9]*gamma[2,1]*gamma[3,1]*gamma[4,3]*gamma[7,2]*gamma[9,7] +
    gamma[10,9]*gamma[3,2]*gamma[4,3]*gamma[7,2]*gamma[9,7] +
    gamma[10,9]*gamma[3,1]*gamma[4,1]*gamma[7,3]*gamma[9,7] +
    gamma[10,9]*gamma[2,1]*gamma[3,2]*gamma[4,1]*gamma[7,3]*gamma[9,7] +
    gamma[10,9]*gamma[2,1]*gamma[3,1]*gamma[4,2]*gamma[7,3]*gamma[9,7] +
    gamma[10,9]*gamma[3,2]*gamma[4,2]*gamma[7,3]*gamma[9,7] +
    gamma[10,9]*gamma[4,3]*gamma[7,3]*gamma[9,7] + gamma[10,9]*gamma[7,4]*gamma[9,7] +
    gamma[10,9]*gamma[4,1]*gamma[5,1]*gamma[7,5]*gamma[9,7] +
    gamma[10,9]*gamma[2,1]*gamma[4,2]*gamma[5,1]*gamma[7,5]*gamma[9,7] +
    gamma[10,9]*gamma[3,1]*gamma[4,3]*gamma[5,1]*gamma[7,5]*gamma[9,7] +
    gamma[10,9]*gamma[2,1]*gamma[3,2]*gamma[4,3]*gamma[5,1]*gamma[7,5]*gamma[9,7] +
    gamma[10,9]*gamma[2,1]*gamma[4,1]*gamma[5,2]*gamma[7,5]*gamma[9,7] +
    gamma[10,9]*gamma[4,2]*gamma[5,2]*gamma[7,5]*gamma[9,7] +
    gamma[10,9]*gamma[2,1]*gamma[3,1]*gamma[4,3]*gamma[5,2]*gamma[7,5]*gamma[9,7] +
    gamma[10,9]*gamma[3,2]*gamma[4,3]*gamma[5,2]*gamma[7,5]*gamma[9,7] +
    gamma[10,9]*gamma[3,1]*gamma[4,1]*gamma[5,3]*gamma[7,5]*gamma[9,7] +
    gamma[10,9]*gamma[2,1]*gamma[3,2]*gamma[4,1]*gamma[5,3]*gamma[7,5]*gamma[9,7] +
    gamma[10,9]*gamma[2,1]*gamma[3,1]*gamma[4,2]*gamma[5,3]*gamma[7,5]*gamma[9,7] +
    gamma[10,9]*gamma[3,2]*gamma[4,2]*gamma[5,3]*gamma[7,5]*gamma[9,7] +
    gamma[10,9]*gamma[4,3]*gamma[5,3]*gamma[7,5]*gamma[9,7] +
    gamma[10,9]*gamma[5,4]*gamma[7,5]*gamma[9,7] +
    gamma[10,9]*gamma[4,1]*gamma[6,1]*gamma[7,6]*gamma[9,7] +
    gamma[10,9]*gamma[2,1]*gamma[4,2]*gamma[6,1]*gamma[7,6]*gamma[9,7] +
    gamma[10,9]*gamma[3,1]*gamma[4,3]*gamma[6,1]*gamma[7,6]*gamma[9,7] +
    gamma[10,9]*gamma[2,1]*gamma[3,2]*gamma[4,3]*gamma[6,1]*gamma[7,6]*gamma[9,7] +
    gamma[10,9]*gamma[2,1]*gamma[4,1]*gamma[6,2]*gamma[7,6]*gamma[9,7] +
    gamma[10,9]*gamma[4,2]*gamma[6,2]*gamma[7,6]*gamma[9,7] +
    gamma[10,9]*gamma[2,1]*gamma[3,1]*gamma[4,3]*gamma[6,2]*gamma[7,6]*gamma[9,7] +
    gamma[10,9]*gamma[3,2]*gamma[4,3]*gamma[6,2]*gamma[7,6]*gamma[9,7] +
    gamma[10,9]*gamma[3,1]*gamma[4,1]*gamma[6,3]*gamma[7,6]*gamma[9,7] +
    gamma[10,9]*gamma[2,1]*gamma[3,2]*gamma[4,1]*gamma[6,3]*gamma[7,6]*gamma[9,7] +
    gamma[10,9]*gamma[2,1]*gamma[3,1]*gamma[4,2]*gamma[6,3]*gamma[7,6]*gamma[9,7] +
    gamma[10,9]*gamma[3,2]*gamma[4,2]*gamma[6,3]*gamma[7,6]*gamma[9,7] +
    gamma[10,9]*gamma[4,3]*gamma[6,3]*gamma[7,6]*gamma[9,7] +
    gamma[10,9]*gamma[6,4]*gamma[7,6]*gamma[9,7] +
    gamma[10,9]*gamma[4,1]*gamma[5,1]*gamma[6,5]*gamma[7,6]*gamma[9,7] +
    gamma[10,9]*gamma[2,1]*gamma[4,2]*gamma[5,1]*gamma[6,5]*gamma[7,6]*gamma[9,7] +
    gamma[10,9]*gamma[3,1]*gamma[4,3]*gamma[5,1]*gamma[6,5]*gamma[7,6]*gamma[9,7] +
    gamma[10,9]*gamma[2,1]*gamma[3,2]*gamma[4,3]*gamma[5,1]*gamma[6,5]*gamma[7,6]*gamma[9,7] +
    gamma[10,9]*gamma[2,1]*gamma[4,1]*gamma[5,2]*gamma[6,5]*gamma[7,6]*gamma[9,7] +
    gamma[10,9]*gamma[4,2]*gamma[5,2]*gamma[6,5]*gamma[7,6]*gamma[9,7] +
    gamma[10,9]*gamma[2,1]*gamma[3,1]*gamma[4,3]*gamma[5,2]*gamma[6,5]*gamma[7,6]*gamma[9,7] +
    gamma[10,9]*gamma[3,2]*gamma[4,3]*gamma[5,2]*gamma[6,5]*gamma[7,6]*gamma[9,7] +
    gamma[10,9]*gamma[3,1]*gamma[4,1]*gamma[5,3]*gamma[6,5]*gamma[7,6]*gamma[9,7] +
    gamma[10,9]*gamma[2,1]*gamma[3,2]*gamma[4,1]*gamma[5,3]*gamma[6,5]*gamma[7,6]*gamma[9,7] +
    gamma[10,9]*gamma[2,1]*gamma[3,1]*gamma[4,2]*gamma[5,3]*gamma[6,5]*gamma[7,6]*gamma[9,7] +
    gamma[10,9]*gamma[3,2]*gamma[4,2]*gamma[5,3]*gamma[6,5]*gamma[7,6]*gamma[9,7] +
    gamma[10,9]*gamma[4,3]*gamma[5,3]*gamma[6,5]*gamma[7,6]*gamma[9,7] +
    gamma[10,9]*gamma[5,4]*gamma[6,5]*gamma[7,6]*gamma[9,7] +
    gamma[10,9]*gamma[4,1]*gamma[8,1]*gamma[9,8] +
    gamma[10,9]*gamma[2,1]*gamma[4,2]*gamma[8,1]*gamma[9,8] +
    gamma[10,9]*gamma[3,1]*gamma[4,3]*gamma[8,1]*gamma[9,8] +
    gamma[10,9]*gamma[2,1]*gamma[3,2]*gamma[4,3]*gamma[8,1]*gamma[9,8] +
    gamma[10,9]*gamma[2,1]*gamma[4,1]*gamma[8,2]*gamma[9,8] +
    gamma[10,9]*gamma[4,2]*gamma[8,2]*gamma[9,8] +
    gamma[10,9]*gamma[2,1]*gamma[3,1]*gamma[4,3]*gamma[8,2]*gamma[9,8] +
    gamma[10,9]*gamma[3,2]*gamma[4,3]*gamma[8,2]*gamma[9,8] +
    gamma[10,9]*gamma[3,1]*gamma[4,1]*gamma[8,3]*gamma[9,8] +
    gamma[10,9]*gamma[2,1]*gamma[3,2]*gamma[4,1]*gamma[8,3]*gamma[9,8] +
    gamma[10,9]*gamma[2,1]*gamma[3,1]*gamma[4,2]*gamma[8,3]*gamma[9,8] +
    gamma[10,9]*gamma[3,2]*gamma[4,2]*gamma[8,3]*gamma[9,8] +
    gamma[10,9]*gamma[4,3]*gamma[8,3]*gamma[9,8] + gamma[10,9]*gamma[8,4]*gamma[9,8] +
    gamma[10,9]*gamma[4,1]*gamma[5,1]*gamma[8,5]*gamma[9,8] +
    gamma[10,9]*gamma[2,1]*gamma[4,2]*gamma[5,1]*gamma[8,5]*gamma[9,8] +
    gamma[10,9]*gamma[3,1]*gamma[4,3]*gamma[5,1]*gamma[8,5]*gamma[9,8] +
    gamma[10,9]*gamma[2,1]*gamma[3,2]*gamma[4,3]*gamma[5,1]*gamma[8,5]*gamma[9,8] +
    gamma[10,9]*gamma[2,1]*gamma[4,1]*gamma[5,2]*gamma[8,5]*gamma[9,8] +
    gamma[10,9]*gamma[4,2]*gamma[5,2]*gamma[8,5]*gamma[9,8] +
    gamma[10,9]*gamma[2,1]*gamma[3,1]*gamma[4,3]*gamma[5,2]*gamma[8,5]*gamma[9,8] +
    gamma[10,9]*gamma[3,2]*gamma[4,3]*gamma[5,2]*gamma[8,5]*gamma[9,8] +
    gamma[10,9]*gamma[3,1]*gamma[4,1]*gamma[5,3]*gamma[8,5]*gamma[9,8] +
    gamma[10,9]*gamma[2,1]*gamma[3,2]*gamma[4,1]*gamma[5,3]*gamma[8,5]*gamma[9,8] +
    gamma[10,9]*gamma[2,1]*gamma[3,1]*gamma[4,2]*gamma[5,3]*gamma[8,5]*gamma[9,8] +
    gamma[10,9]*gamma[3,2]*gamma[4,2]*gamma[5,3]*gamma[8,5]*gamma[9,8] +
    gamma[10,9]*gamma[4,3]*gamma[5,3]*gamma[8,5]*gamma[9,8] +
    gamma[10,9]*gamma[5,4]*gamma[8,5]*gamma[9,8] +
    gamma[10,9]*gamma[4,1]*gamma[6,1]*gamma[8,6]*gamma[9,8] +
    gamma[10,9]*gamma[2,1]*gamma[4,2]*gamma[6,1]*gamma[8,6]*gamma[9,8] +
    gamma[10,9]*gamma[3,1]*gamma[4,3]*gamma[6,1]*gamma[8,6]*gamma[9,8] +
    gamma[10,9]*gamma[2,1]*gamma[3,2]*gamma[4,3]*gamma[6,1]*gamma[8,6]*gamma[9,8] +
    gamma[10,9]*gamma[2,1]*gamma[4,1]*gamma[6,2]*gamma[8,6]*gamma[9,8] +
    gamma[10,9]*gamma[4,2]*gamma[6,2]*gamma[8,6]*gamma[9,8] +
    gamma[10,9]*gamma[2,1]*gamma[3,1]*gamma[4,3]*gamma[6,2]*gamma[8,6]*gamma[9,8] +
    gamma[10,9]*gamma[3,2]*gamma[4,3]*gamma[6,2]*gamma[8,6]*gamma[9,8] +
    gamma[10,9]*gamma[3,1]*gamma[4,1]*gamma[6,3]*gamma[8,6]*gamma[9,8] +
    gamma[10,9]*gamma[2,1]*gamma[3,2]*gamma[4,1]*gamma[6,3]*gamma[8,6]*gamma[9,8] +
    gamma[10,9]*gamma[2,1]*gamma[3,1]*gamma[4,2]*gamma[6,3]*gamma[8,6]*gamma[9,8] +
    gamma[10,9]*gamma[3,2]*gamma[4,2]*gamma[6,3]*gamma[8,6]*gamma[9,8] +
    gamma[10,9]*gamma[4,3]*gamma[6,3]*gamma[8,6]*gamma[9,8] +
    gamma[10,9]*gamma[6,4]*gamma[8,6]*gamma[9,8] +
    gamma[10,9]*gamma[4,1]*gamma[5,1]*gamma[6,5]*gamma[8,6]*gamma[9,8] +
    gamma[10,9]*gamma[2,1]*gamma[4,2]*gamma[5,1]*gamma[6,5]*gamma[8,6]*gamma[9,8] +
    gamma[10,9]*gamma[3,1]*gamma[4,3]*gamma[5,1]*gamma[6,5]*gamma[8,6]*gamma[9,8] +
    gamma[10,9]*gamma[2,1]*gamma[3,2]*gamma[4,3]*gamma[5,1]*gamma[6,5]*gamma[8,6]*gamma[9,8] +
    gamma[10,9]*gamma[2,1]*gamma[4,1]*gamma[5,2]*gamma[6,5]*gamma[8,6]*gamma[9,8] +
    gamma[10,9]*gamma[4,2]*gamma[5,2]*gamma[6,5]*gamma[8,6]*gamma[9,8] +
    gamma[10,9]*gamma[2,1]*gamma[3,1]*gamma[4,3]*gamma[5,2]*gamma[6,5]*gamma[8,6]*gamma[9,8] +
    gamma[10,9]*gamma[3,2]*gamma[4,3]*gamma[5,2]*gamma[6,5]*gamma[8,6]*gamma[9,8] +
    gamma[10,9]*gamma[3,1]*gamma[4,1]*gamma[5,3]*gamma[6,5]*gamma[8,6]*gamma[9,8] +
    gamma[10,9]*gamma[2,1]*gamma[3,2]*gamma[4,1]*gamma[5,3]*gamma[6,5]*gamma[8,6]*gamma[9,8] +
    gamma[10,9]*gamma[2,1]*gamma[3,1]*gamma[4,2]*gamma[5,3]*gamma[6,5]*gamma[8,6]*gamma[9,8] +
    gamma[10,9]*gamma[3,2]*gamma[4,2]*gamma[5,3]*gamma[6,5]*gamma[8,6]*gamma[9,8] +
    gamma[10,9]*gamma[4,3]*gamma[5,3]*gamma[6,5]*gamma[8,6]*gamma[9,8] +
    gamma[10,9]*gamma[5,4]*gamma[6,5]*gamma[8,6]*gamma[9,8] +
    gamma[10,9]*gamma[4,1]*gamma[7,1]*gamma[8,7]*gamma[9,8] +
    gamma[10,9]*gamma[2,1]*gamma[4,2]*gamma[7,1]*gamma[8,7]*gamma[9,8] +
    gamma[10,9]*gamma[3,1]*gamma[4,3]*gamma[7,1]*gamma[8,7]*gamma[9,8] +
    gamma[10,9]*gamma[2,1]*gamma[3,2]*gamma[4,3]*gamma[7,1]*gamma[8,7]*gamma[9,8] +
    gamma[10,9]*gamma[2,1]*gamma[4,1]*gamma[7,2]*gamma[8,7]*gamma[9,8] +
    gamma[10,9]*gamma[4,2]*gamma[7,2]*gamma[8,7]*gamma[9,8] +
    gamma[10,9]*gamma[2,1]*gamma[3,1]*gamma[4,3]*gamma[7,2]*gamma[8,7]*gamma[9,8] +
    gamma[10,9]*gamma[3,2]*gamma[4,3]*gamma[7,2]*gamma[8,7]*gamma[9,8] +
    gamma[10,9]*gamma[3,1]*gamma[4,1]*gamma[7,3]*gamma[8,7]*gamma[9,8] +
    gamma[10,9]*gamma[2,1]*gamma[3,2]*gamma[4,1]*gamma[7,3]*gamma[8,7]*gamma[9,8] +
    gamma[10,9]*gamma[2,1]*gamma[3,1]*gamma[4,2]*gamma[7,3]*gamma[8,7]*gamma[9,8] +
    gamma[10,9]*gamma[3,2]*gamma[4,2]*gamma[7,3]*gamma[8,7]*gamma[9,8] +
    gamma[10,9]*gamma[4,3]*gamma[7,3]*gamma[8,7]*gamma[9,8] +
    gamma[10,9]*gamma[7,4]*gamma[8,7]*gamma[9,8] +
    gamma[10,9]*gamma[4,1]*gamma[5,1]*gamma[7,5]*gamma[8,7]*gamma[9,8] +
    gamma[10,9]*gamma[2,1]*gamma[4,2]*gamma[5,1]*gamma[7,5]*gamma[8,7]*gamma[9,8] +
    gamma[10,9]*gamma[3,1]*gamma[4,3]*gamma[5,1]*gamma[7,5]*gamma[8,7]*gamma[9,8] +
    gamma[10,9]*gamma[2,1]*gamma[3,2]*gamma[4,3]*gamma[5,1]*gamma[7,5]*gamma[8,7]*gamma[9,8] +
    gamma[10,9]*gamma[2,1]*gamma[4,1]*gamma[5,2]*gamma[7,5]*gamma[8,7]*gamma[9,8] +
    gamma[10,9]*gamma[4,2]*gamma[5,2]*gamma[7,5]*gamma[8,7]*gamma[9,8] +
    gamma[10,9]*gamma[2,1]*gamma[3,1]*gamma[4,3]*gamma[5,2]*gamma[7,5]*gamma[8,7]*gamma[9,8] +
    gamma[10,9]*gamma[3,2]*gamma[4,3]*gamma[5,2]*gamma[7,5]*gamma[8,7]*gamma[9,8] +
    gamma[10,9]*gamma[3,1]*gamma[4,1]*gamma[5,3]*gamma[7,5]*gamma[8,7]*gamma[9,8] +
    gamma[10,9]*gamma[2,1]*gamma[3,2]*gamma[4,1]*gamma[5,3]*gamma[7,5]*gamma[8,7]*gamma[9,8] +
    gamma[10,9]*gamma[2,1]*gamma[3,1]*gamma[4,2]*gamma[5,3]*gamma[7,5]*gamma[8,7]*gamma[9,8] +
    gamma[10,9]*gamma[3,2]*gamma[4,2]*gamma[5,3]*gamma[7,5]*gamma[8,7]*gamma[9,8] +
    gamma[10,9]*gamma[4,3]*gamma[5,3]*gamma[7,5]*gamma[8,7]*gamma[9,8] +
    gamma[10,9]*gamma[5,4]*gamma[7,5]*gamma[8,7]*gamma[9,8] +
    gamma[10,9]*gamma[4,1]*gamma[6,1]*gamma[7,6]*gamma[8,7]*gamma[9,8] +
    gamma[10,9]*gamma[2,1]*gamma[4,2]*gamma[6,1]*gamma[7,6]*gamma[8,7]*gamma[9,8] +
    gamma[10,9]*gamma[3,1]*gamma[4,3]*gamma[6,1]*gamma[7,6]*gamma[8,7]*gamma[9,8] +
    gamma[10,9]*gamma[2,1]*gamma[3,2]*gamma[4,3]*gamma[6,1]*gamma[7,6]*gamma[8,7]*gamma[9,8] +
    gamma[10,9]*gamma[2,1]*gamma[4,1]*gamma[6,2]*gamma[7,6]*gamma[8,7]*gamma[9,8] +
    gamma[10,9]*gamma[4,2]*gamma[6,2]*gamma[7,6]*gamma[8,7]*gamma[9,8] +
    gamma[10,9]*gamma[2,1]*gamma[3,1]*gamma[4,3]*gamma[6,2]*gamma[7,6]*gamma[8,7]*gamma[9,8] +
    gamma[10,9]*gamma[3,2]*gamma[4,3]*gamma[6,2]*gamma[7,6]*gamma[8,7]*gamma[9,8] +
    gamma[10,9]*gamma[3,1]*gamma[4,1]*gamma[6,3]*gamma[7,6]*gamma[8,7]*gamma[9,8] +
    gamma[10,9]*gamma[2,1]*gamma[3,2]*gamma[4,1]*gamma[6,3]*gamma[7,6]*gamma[8,7]*gamma[9,8] +
    gamma[10,9]*gamma[2,1]*gamma[3,1]*gamma[4,2]*gamma[6,3]*gamma[7,6]*gamma[8,7]*gamma[9,8] +
    gamma[10,9]*gamma[3,2]*gamma[4,2]*gamma[6,3]*gamma[7,6]*gamma[8,7]*gamma[9,8] +
    gamma[10,9]*gamma[4,3]*gamma[6,3]*gamma[7,6]*gamma[8,7]*gamma[9,8] +
    gamma[10,9]*gamma[6,4]*gamma[7,6]*gamma[8,7]*gamma[9,8] +
    gamma[10,9]*gamma[4,1]*gamma[5,1]*gamma[6,5]*gamma[7,6]*gamma[8,7]*gamma[9,8] +
    gamma[10,9]*gamma[2,1]*gamma[4,2]*gamma[5,1]*gamma[6,5]*gamma[7,6]*gamma[8,7]*gamma[9,8] +
    gamma[10,9]*gamma[3,1]*gamma[4,3]*gamma[5,1]*gamma[6,5]*gamma[7,6]*gamma[8,7]*gamma[9,8] +
    gamma[10,9]*gamma[2,1]*gamma[3,2]*gamma[4,3]*gamma[5,1]*gamma[6,5]*gamma[7,6]*gamma[8,7]*gamma[9,8] +
    gamma[10,9]*gamma[2,1]*gamma[4,1]*gamma[5,2]*gamma[6,5]*gamma[7,6]*gamma[8,7]*gamma[9,8] +
    gamma[10,9]*gamma[4,2]*gamma[5,2]*gamma[6,5]*gamma[7,6]*gamma[8,7]*gamma[9,8] +
    gamma[10,9]*gamma[2,1]*gamma[3,1]*gamma[4,3]*gamma[5,2]*gamma[6,5]*gamma[7,6]*gamma[8,7]*gamma[9,8] +
    gamma[10,9]*gamma[3,2]*gamma[4,3]*gamma[5,2]*gamma[6,5]*gamma[7,6]*gamma[8,7]*gamma[9,8] +
    gamma[10,9]*gamma[3,1]*gamma[4,1]*gamma[5,3]*gamma[6,5]*gamma[7,6]*gamma[8,7]*gamma[9,8] +
    gamma[10,9]*gamma[2,1]*gamma[3,2]*gamma[4,1]*gamma[5,3]*gamma[6,5]*gamma[7,6]*gamma[8,7]*gamma[9,8] +
    gamma[10,9]*gamma[2,1]*gamma[3,1]*gamma[4,2]*gamma[5,3]*gamma[6,5]*gamma[7,6]*gamma[8,7]*gamma[9,8] +
    gamma[10,9]*gamma[3,2]*gamma[4,2]*gamma[5,3]*gamma[6,5]*gamma[7,6]*gamma[8,7]*gamma[9,8] +
    gamma[10,9]*gamma[4,3]*gamma[5,3]*gamma[6,5]*gamma[7,6]*gamma[8,7]*gamma[9,8] +
    gamma[10,9]*gamma[5,4]*gamma[6,5]*gamma[7,6]*gamma[8,7]*gamma[9,8]
}
