################################################################################
#
#   Purpose: central file to be sourced to several test-xxx.R files
#
################################################################################
## What is allowed:
#
#   A maximum of 5 exogenous constructs is allowed if a structural model is supplied;
#   otherwise an unlimited number is possible as long as all construct correlations
#   are supplied by the user:
#     1. If there is 1 exogenous construct  : a maximum of 7 endogenous constructs is allowed
#     2. If there are 2 exogenous constructs: a maximum of 6 endogenous constructs is allowed
#     3. If there are 3 exogenous constructs: a maximum of 5 endogenous constructs is allowed
#     4. If there are 4 exogenous constructs: a maximum of 4 endogenous constructs is allowed
#     5. If there are 5 exogenous constructs: a maximum of 4 endogenous constructs is allowed
#
## Function to compare estimates vs population parameters
comparecSEM <- function(.object, .what, .pop_parameters) {
  # .object: cSEM object
  # .what: what to compare
  # .pop_parameters: a vector of population values

  x <- cSEM::summarize(.object)

  if(inherits(.object, "cSEMResults_2ndorder")) {
    x1 <- x$First_stage$Estimates
    x2 <- x$Second_stage$Estimates
  } else {
    x1 <- NULL
    x2 <- x$Estimates
  }

  if(.what == "Path_estimates") {
    est <- x2$Path_estimates[, c("Name", "Estimate")]

  } else if(.what == "Loading_estimates") {
    # Only for common factors
    est <- rbind(x1$Loading_estimates[x1$Loading_estimates$Construct_type == "Common factor",
                                      c("Name", "Estimate")],
                 x2$Loading_estimates[x2$Loading_estimates$Construct_type == "Common factor",
                                      c("Name", "Estimate")])

  } else if(.what == "Weight_estimates") {
    ## Compare only weights for composites, since only those weights are
    ## specified when creating the DGP
    x1$Weight_estimates
    est <- rbind(
      x1$Weight_estimates[x1$Weight_estimates$Construct_type == "Composite",
                          c("Name", "Estimate")],
      x2$Weight_estimates[x2$Weight_estimates$Construct_type == "Composite",
                          c("Name", "Estimate")])
  } else if(.what == "Residual_correlation") {
    est <- x2$Residual_correlation[, c("Name", "Estimate")]
  } else if(.what == "Indicator_correlation") {
    est <- x2$Indicator_correlation[, c("Name", "Estimate")]
  } else if(.what == "Exo_construct_correlation") {
    est <- x2$Exo_construct_correlation[, c("Name", "Estimate")]
  } else {
    stop("Error")
  }

  df <- data.frame(est,
                   "Pop_value" = unname(.pop_parameters),
                   stringsAsFactors = FALSE)
  df
}

#===============================================================================
### Some data generating processes that cSEM.DGP can handle --------------------
#===============================================================================
### 1 exogenous construct and <= 7 endogenous constructs------------------------
dgp_1exo_ok <- " # less than 7 endogenous; random order to check ordering
eta1 ~ 0.5*xi1
eta2 ~ 0.4*eta1 + 0.3*xi1
eta4 ~ 0.4*eta1 + 0.3*eta2 + 0.2*eta3
eta3 ~ 0.4*eta2

eta4 =~ 0.7*y41 + 0.8*y42 + 0.6*y43
eta1 =~ 0.7*y11 + 0.8*y12
eta2 =~ 0.7*y21 + 0.8*y22
eta3 <~ 2*y31 + 1*y32
xi1  =~ 0.7*x1  + 0.8*x2

y42 ~~ 0.2*y43
y31 ~~ 0.35*y32
"

model_1exo_ok <- " # less than 7 endogenous; random order to check ordering
eta1 ~ xi1
eta2 ~ eta1 + xi1
eta4 ~ eta1 + eta2 + eta3
eta3 ~ eta2

eta4 =~ y41 + y42 + y43
eta1 =~ y11 + y12
eta2 =~ y21 + y22
eta3 <~ y31 + y32
xi1  =~  x1 +  x2

y42 ~~ y43
"

pop_paths         <- c(0.5, 0.3, 0.4, 0.4, 0.4, 0.3, 0.2)
pop_loadings      <- c(0.7, 0.8, 0.7, 0.8, 0.7, 0.8, 0.7, 0.8, 0.6)
w_unscaled        <- c(2, 1)
pop_indicator_cor <- c(0.35)
Sigma_jj          <- matrix(c(1, 0.35, 0.35, 1), nrow = length(w_unscaled))
pop_weights       <- w_unscaled / c(sqrt(w_unscaled %*% Sigma_jj %*% w_unscaled))
pop_residual_cor  <- c(0.2)

dat_1exo <- generateData(dgp_1exo_ok, .empirical = TRUE)
out <- csem(dat_1exo, model_1exo_ok)

path      <- comparecSEM(out, .what = "Path_estimates", pop_paths)
loadings  <- comparecSEM(out, .what = "Loading_estimates", pop_loadings)
weights   <- comparecSEM(out, .what = "Weight_estimates", pop_weights)
residual_cor <- comparecSEM(out, .what = "Residual_correlation", pop_residual_cor)
indicator_cor <- comparecSEM(out, .what = "Indicator_correlation", pop_indicator_cor)

test_that("1 exo construct; <= 7 endo constructs; random order in model; works", {
  expect_equal(path$Estimate, path$Pop_value)
  expect_equal(loadings$Estimate, loadings$Pop_value)
  expect_equal(weights$Estimate, weights$Pop_value)
  expect_equal(residual_cor$Estimate, residual_cor$Pop_value)
  expect_equal(indicator_cor$Estimate, indicator_cor$Pop_value)
})


### 2 exogenous construct and <= 5 endogenous constructs------------------------
dgp_3exo_ok <- " # less than 5 endogenous; random order to check ordering
eta2 ~ 0.4*eta1 + 0.3*xi1
eta1 ~ 0.5*xi1 + 0.4*xi2 + 0.25*xi3
eta3 ~ 0.4*eta2

eta1 =~ 0.7*y11 + 0.8*y12 + 0.7*y13 + 0.5*y14
eta2 =~ 0.7*y21 + 0.8*y22
eta3 =~ 0.7*y31 + 0.75*y32
xi1  =~ 0.7*x11 + 0.8*x12
xi2  =~ 0.7*x21 + 0.8*x22
xi3  =~ 0.7*x31 + 0.8*x32

y11 ~~ 0.2*y14
y11 ~~ 0.1*y13
y13 ~~ 0.3*y14

xi1 ~~ 0.5*xi2
xi3 ~~ 0.3*xi2
xi1 ~~ 0.4*xi3
"

pop_paths         <- c(0.5, 0.4, 0.25, 0.3, 0.4, 0.4)
pop_loadings      <- c(0.7, 0.8, 0.7, 0.8, 0.7, 0.8, 0.7, 0.8, 0.7, 0.5, 0.7, 0.8, 0.7, 0.75)
pop_residual_cor  <- c(0.1, 0.2, 0.3)
pop_exo_construct_cor <- c(0.5, 0.4, 0.3)

dat_3exo <- generateData(dgp_3exo_ok, .empirical = TRUE)
out <- csem(dat_3exo, dgp_3exo_ok)

path      <- comparecSEM(out, .what = "Path_estimates", pop_paths)
loadings  <- comparecSEM(out, .what = "Loading_estimates", pop_loadings)
residual_cor <- comparecSEM(out, .what = "Residual_correlation", pop_residual_cor)
exo_construct_cor <- comparecSEM(out, .what = "Exo_construct_correlation", pop_exo_construct_cor)

test_that("1 exo construct; <= 7 endo constructs; random order in model; works", {
  expect_equal(path$Estimate, path$Pop_value)
  expect_equal(loadings$Estimate, loadings$Pop_value)
  expect_equal(residual_cor$Estimate, residual_cor$Pop_value)
  expect_equal(exo_construct_cor$Estimate, exo_construct_cor$Pop_value)
})

### 3 exogenous constructs and 3 endogenous; 1 second-order --------------------
dgp2ndorder <- "
# Structural model
eta1 ~ 0.5*xi1 + 0.4*xi2 + 0.4*xi3
eta2 ~ 0.5*eta1
eta3 ~ 0.4*eta2 + 0.6*xi2

# Measurement model
xi1  =~ 0.7*x11 + 0.8*x12
xi2  =~ 0.7*x21 + 0.8*x22
xi3  =~ 0.7*x31 + 0.8*x32
c1   =~ 0.6*y41 + 0.85*y42
c2   =~ 0.6*y51 + 0.85*y52
eta3 =~ 0.8*y21 + 0.8*y22
c3   =~ 0.6*y61 + 0.85*y62
eta1 =~ 0.6*y11 + 0.7*y12 + 0.8*y13

# Exogenous construct correlation
xi1 ~~ 0.3*xi2
xi3 ~~ 0.3*xi2
xi1 ~~ 0.4*xi3

# 2nd order specification
eta2 =~ 0.4*c1 + 0.6*c2 + 0.2*c3
"

model_2ndorder <- "
# Structural model
eta1 ~ xi1 + xi2 + xi3
eta2 ~ eta1
eta3 ~ eta2 + xi2

# Measurement model
xi1  =~ x11 + x12
xi2  =~ x21 + x22
xi3  =~ x31 + x32
c1   =~ y41 + y42
c2   =~ y51 + y52
eta3 =~ y21 + y22
c3   =~ y61 + y62
eta1 =~ y11 + y12 + y13

# 2nd order specification
eta2 =~ c1 + c2 + c3
"

pop_paths         <- c(0.5, 0.4, 0.4, 0.5, 0.6, 0.4)
pop_loadings      <- c(0.7, 0.8, 0.7, 0.8, 0.7, 0.8, 0.6, 0.85, 0.6, 0.85, 0.6,
                       0.85, 0.6, 0.7, 0.8, 0.8, 0.8, 0.4, 0.6, 0.2)
pop_exo_construct_cor <- c(0.3, 0.4, 0.3)

dat_2ndorder <- generateData(dgp2ndorder, .empirical = TRUE)
out <- csem(dat_2ndorder, model_2ndorder)

path      <- comparecSEM(out, .what = "Path_estimates", pop_paths)
loadings  <- comparecSEM(out, .what = "Loading_estimates", pop_loadings)
exo_construct_cor <- comparecSEM(out, .what = "Exo_construct_correlation", pop_exo_construct_cor)

test_that("3 exo constructs; <= 7 endo constructs; 2ndorder; works", {
  expect_equal(path$Estimate, path$Pop_value)
  expect_equal(loadings$Estimate, loadings$Pop_value)
  expect_equal(exo_construct_cor$Estimate, exo_construct_cor$Pop_value)
})

### 4 Construct correlations are supplied instead of a structural model --------
dgp_only_correlations <- "
# Construct correlations
EXPE ~~ 0.3*IMAG

# Composite model
EXPE =~ 0.7*expe1 + 0.8*expe2
IMAG <~ 0.7*imag1 + 0.9*imag2

# Indicator correlation
imag1 ~~ 0.4*imag2
"

pop_loadings      <- c(0.7, 0.8, 0.7, 0.8)
pop_construct_cor <- 0.3
pop_indicator_cor <- 0.4

dat_only_correlations <- generateData(dgp_only_correlations, .empirical = TRUE)
out <- csem(dat_only_correlations, dgp_only_correlations,
            .PLS_weight_scheme_inner = "centroid")

loadings      <- comparecSEM(out, .what = "Loading_estimates", pop_loadings)
construct_cor <- comparecSEM(out, .what = "Exo_construct_correlation", pop_construct_cor)
indicator_cor <- comparecSEM(out, .what = "Indicator_correlation", pop_indicator_cor)

test_that("Correlation instead of structural model works", {
  expect_equal(loadings$Estimate, loadings$Pop_value)
  expect_equal(construct_cor$Estimate, construct_cor$Pop_value)
  expect_equal(indicator_cor$Estimate, indicator_cor$Pop_value)
})

#===============================================================================
### Data generating processes that cSEM.DGP can not handle ---------------------
#===============================================================================
### 1 Exogenous construct and > 7 endogenous constructs ------------------------

dgp_1exo_error <- " # more than 7 endogenous constructs
eta1 ~ 0.4*xi1
eta2 ~ 0.4*eta1 + 0.4*xi1
eta3 ~ 0.4*eta2 + 0.3*eta1
eta4 ~ 0.4*eta1 + 0.2*eta2 + 0.4*eta3
eta5 ~ 0.4*eta4
eta6 ~ 0.4*eta4 + 0.2*eta5
eta7 ~ 0.4*eta6
eta8 ~ 0.4*eta7

xi1  =~ 0.7*x1  + 0.8*x2
eta1 =~ 0.7*y11 + 0.8*y12
eta2 =~ 0.7*y21 + 0.8*y22
eta3 =~ 0.7*y31 + 0.8*y32
eta4 =~ 0.7*y41 + 0.8*y42
eta5 =~ 0.7*y51 + 0.8*y52
eta6 =~ 0.7*y61 + 0.8*y62
eta7 =~ 0.7*y71 + 0.8*y72
eta8 =~ 0.7*y81 + 0.8*y82
"

test_that("1 exo construct; > 7 endo constructs; fails",
          expect_error(generateData(dgp_1exo_error)))

### More than 5 exogenous construct --------------------------------------------

dgp_5exo_error <- " # more than 5 exogenous
eta1 ~ 0.4*xi1 + 0.4*xi2 + 0.4*xi3 + 0.4*xi4
eta2 ~ 0.4*eta1 + 0.4*xi5 + 0.5*xi6

xi1  =~ 0.7*x1  + 0.8*x2
xi2  =~ 0.7*y11 + 0.8*y12
xi3  =~ 0.7*y21 + 0.8*y22
xi4  =~ 0.7*y31 + 0.8*y32
xi5  =~ 0.7*y41 + 0.8*y42
xi6  =~ 0.7*y51 + 0.8*y52
eta1 =~ 0.7*y71 + 0.8*y72
eta2 =~ 0.7*y81 + 0.8*y82
"

test_that("1 exo construct; > 7 endo constructs; fails",
          expect_error(generateData(dgp_5exo_error)))
