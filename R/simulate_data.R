simulateData <- function(.info_frame, .skewness, .kurtosis, .N, .empirical,
                         .handle_negative_definite){
  if(is.null(.skewness) && is.null(.kurtosis)){
    .info_frame <- MASS::mvrnorm(.N, mu = rep(0, nrow(.info_frame)),
                                 Sigma = .info_frame, empirical = .empirical)
    return(.info_frame)
  }else if(length(.skewness) != nrow(.info_frame)){
    stop("Please provide a list of skewness and kurtosis values for all of the
         indicators. Normally distributed indicators should have
         a 0 for the skewness and kurtosis values.")
  }else if(length(.kurtosis) != nrow(.info_frame)){
    stop("Please provide a list of skewness and kurtosis values for all of
         the indicators. Normally distributed indicators should have
         a 0 for the skewness and kurtosis values.")
  }else{
    # Generate a Matrix with the coefficients for the Fleishman transformation
    coefficients <- matrix(0, nrow = length(.skewness), ncol = 4,
                           dimnames = list(names(.skewness), c("a", "b", "c", "d")))

    # Insert the coefficients calculated by the Fleishman approach

    for(i in 1: length(.skewness)){
      k <- rownames(coefficients)[i]
      coefficients[k,] <- Fleishman(.skewness = as.matrix(.skewness)[k,],
                                    .kurtosis = as.matrix(.kurtosis)[k,])
    }

    # Generate a matrix for the correlations of the standard normal variables
    cor_dependent <- matrix(0, nrow = nrow(.info_frame), ncol = ncol(.info_frame),
                            dimnames = dimnames(.info_frame))
    for(i in 1:(nrow(.info_frame)-1)){
      for(j in (i+1):ncol(.info_frame)){
        cor_dependent[i,j] <- ValeMaurelli(.cor = .info_frame[i,j],
                            .coef <- rbind(coefficients[i,], coefficients[j,]))
      }
    }
    cor_dependent <- cor_dependent + t(cor_dependent)
    diag(cor_dependent) <- 1


    # Check if semi-positve definite
    if (!matrixcalc::is.positive.semi.definite(cor_dependent)) {
      if(.handle_negative_definite %in% c("drop", "set_NA")) {
        NA
      } else if(.handle_negative_definite == "stop") {
        stop("Correlation matrix for the generation of the nonnormal data is not semi-positive definite.",
        " Please check your combinations of values for skewness and kurtosis.",
             call. = FALSE)
      }
    } else {
      cor_dependent
    }


    # Generate the standard normal variables with the correlation matrix
    # calculated by the Vale Maurelli approach
    dat_X <- MASS::mvrnorm(.N, rep(0, nrow(.info_frame)), cor_dependent)
    colnames(dat_X) <- colnames(.info_frame)

    dat <- dat_X

    #
    for(i in 1:ncol(cor_dependent)){
      dat[,i] <- coefficients[i,"a"] + coefficients[i,"b"]*dat_X[,i] +
        coefficients[i,"c"]*dat_X[,i]^2 + coefficients[i,"d"]*dat_X[,i]^3

    }
    .info_frame <- dat
    return(.info_frame)
  }

}

# Calculate the coefficients of the Fleishman approach
  Fleishman <- function(.skewness, .kurtosis){
    coef <- function(.x, .mom) {
      c(F1 = (.x[1]*.x[1] + 6*.x[1]*.x[3] + 2*.x[2]*.x[2] + 15*.x[3]*.x[3] - 1),
        F2 = (2*.x[2]*(.x[1]*.x[1] + 24*.x[1]*.x[3] + 105*.x[3]*.x[3]
                       + 2) - as.numeric(.mom[1])),
        F3 = (24*(.x[1]*.x[3] + .x[2]*.x[2]*(1 + .x[1]*.x[1]+ 28*.x[1]*.x[3]) +
                    .x[3]*.x[3]*(12 + 48*.x[1]*.x[3] + 141*.x[2]*.x[2] +
                                   225*.x[3]*.x[3])) - as.numeric(.mom[2])))
    }

    .mom <- rbind(.skewness, .kurtosis)
    res <- tryCatch(rootSolve::multiroot(coef, start = c(1,0,0), .mom = .mom,
                               maxiter = 100, rtol = 1e-15),
                    warning = function(w){
                      stop("Please check your combinations of skewness and kurtosis values.",
                      " Please be aware of that skewness^2 < 0.0629576*kurtosis + 0.0717247 must hold.")
                    })
    return(c(-res$root[2], res$root))
  }

# Calculate the correlation of the standard normal variables with the Vale Maurrelli approach
  ValeMaurelli <- function(.cor, .coef){
    corr <- function(.x, .cor, .coef){
      F1 = .x*(.coef[1,"b"]*.coef[2,"b"] + 3*.coef[1,"b"]*.coef[2,"d"] +
            3*.coef[1,"d"]*.coef[2,"b"] + 9*.coef[1,"d"]*.coef[2,"d"]) +
            .x*.x*2*.coef[1,"c"]*.coef[2,"c"] + .x*.x*.x*6*.coef[1,"d"]*.coef[2,"d"] - .cor
    }
    res <- rootSolve::multiroot(corr, start = 0.5, .coef = .coef, .cor = .cor, maxiter = 100, rtol = 1e-5)
    return(res$root)
  }

