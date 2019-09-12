#' Compute construct correlation matrix
#'
#' Calculate the construct correlation matrix correlation. Currently, there
#' is a limit of ten constructs.
#'
#' @param .structural A matrix mimicking the structural relationship
#'   between constructs. Up to 10 constructs are supported.
#' @param gamma A (10 x 10) matrix containing the path coefficients.
#'
#' @return A (10 x 10) matrix of construct correlations.
#'
#' @keywords internal

generateConstructCor <- function(
  path_matrix = path_matrix,
  gamma = NULL
  ){

  ## Compute elements of the construct correlation matrix
  e1e2 <- eta1eta2(gamma)
  e1e3 <- eta1eta3(gamma)
  e1e4 <- eta1eta4(gamma)
  e1e5 <- eta1eta5(gamma)
  e1e6 <- eta1eta6(gamma)
  e1e7 <- eta1eta7(gamma)
  e1e8 <- eta1eta8(gamma)
  e2e3 <- eta2eta3(gamma)
  e2e4 <- eta2eta4(gamma)
  e2e5 <- eta2eta5(gamma)
  e2e6 <- eta2eta6(gamma)
  e2e7 <- eta2eta7(gamma)
  e2e8 <- eta2eta8(gamma)
  e3e4 <- eta3eta4(gamma)
  e3e5 <- eta3eta5(gamma)
  e3e6 <- eta3eta6(gamma)
  e3e7 <- eta3eta7(gamma)
  e3e8 <- eta3eta8(gamma)
  e4e5 <- eta4eta5(gamma)
  e4e6 <- eta4eta6(gamma)
  e4e7 <- eta4eta7(gamma)
  e4e8 <- eta4eta8(gamma)
  e5e6 <- eta5eta6(gamma)
  e5e7 <- eta5eta7(gamma)
  e5e8 <- eta5eta8(gamma)
  e6e7 <- eta6eta7(gamma)
  e6e8 <- eta6eta8(gamma)
  e7e8 <- eta7eta8(gamma)


  # Define matrix for the correlations with the same dimension as the matrix
  # (Upper triangular matrix)
  # with the path coefficients
  res <- matrix(0, nrow = nrow(path_matrix), ncol = ncol(path_matrix),
               dimnames = dimnames(path_matrix))

  # Compute the correlations between two indicators each
  for(i in 1:(nrow(path_matrix)-1)){
    for(j in (i+1):ncol(path_matrix)){
      res[i,j] <- get(paste("e",i,"e",j,sep = ""),environment())
    }
  }

  res <- t(res) + res
  diag(res) <- rep(1, nrow(res))

  return(res)
}
