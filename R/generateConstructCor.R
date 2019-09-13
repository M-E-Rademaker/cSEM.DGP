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
  .structural = NULL,
  .gamma = NULL
  ){

  k <- nrow(.structural)

  if(k > 8) {
    stop("Models containing more than 8 constructs are not supported.", call. = FALSE)
  }
  # Define matrix for the correlations with the same dimension as the matrix
  # (Upper triangular matrix)
  # with the path coefficients
  res <- matrix(0, nrow = nrow(.structural), ncol = ncol(.structural),
                dimnames = dimnames(.structural))

  if(k >= 2){
    res[1, 2] <- eta1eta2(.gamma)
  }
  if(k >= 3){
    res[1, 3] <- eta1eta3(.gamma)
    res[2, 3] <- eta2eta3(.gamma)
  }
  if(k >= 4){
    res[1, 4] <- eta1eta4(.gamma)
    res[2, 4] <- eta2eta4(.gamma)
    res[3, 4] <- eta3eta4(.gamma)
  }
  if(k >= 5){
    res[1, 5] <- eta1eta5(.gamma)
    res[2, 5] <- eta2eta5(.gamma)
    res[3, 5] <- eta3eta5(.gamma)
    res[4, 5] <- eta4eta5(.gamma)
  }
  if(k >= 6){
    res[1, 6] <- eta1eta6(.gamma)
    res[2, 6] <- eta2eta6(.gamma)
    res[3, 6] <- eta3eta6(.gamma)
    res[4, 6] <- eta4eta6(.gamma)
    res[5, 6] <- eta5eta6(.gamma)
  }
  if(k >= 7){
    res[1, 7] <- eta1eta7(.gamma)
    res[2, 7] <- eta2eta7(.gamma)
    res[3, 7] <- eta3eta7(.gamma)
    res[4, 7] <- eta4eta7(.gamma)
    res[5, 7] <- eta5eta7(.gamma)
    res[6, 7] <- eta6eta7(.gamma)
  }
  if(k >= 8){
    res[1, 8] <- eta1eta8(.gamma)
    res[2, 8] <- eta2eta8(.gamma)
    res[3, 8] <- eta3eta8(.gamma)
    res[4, 8] <- eta4eta8(.gamma)
    res[5, 8] <- eta5eta8(.gamma)
    res[6, 8] <- eta6eta8(.gamma)
    res[7, 8] <- eta7eta8(.gamma)
  }

  res <- t(res) + res
  diag(res) <- rep(1, nrow(res))

  return(res)
}
