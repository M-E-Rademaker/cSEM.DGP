# ==============================================================================
# Without variable parameters
# ==============================================================================
## DGP with constructs modeled as common factors
dgp <- "
# Structural model
eta2 ~ 0.4*eta1
eta3 ~ 0.4*eta1 + 0.35*eta2

# Measurement model
eta1 =~ 0.8*y11 + 0.9*y12 + 0.8*y13
eta2 =~ 0.7*y21 + 0.7*y22 + 0.9*y23
eta3 =~ 0.9*y31 + 0.8*y32 + 0.7*y33
"

dat <- generateData(dgp, .return_type = "cor")
dat

## DGP with a construct modeled as a composite
# If the model contains composites, within-block indicator correlation
# needs to be set as well.
dgp <- "
# Structural model
eta2 ~ 0.2*eta1
eta3 ~ 0.4*eta1 + 0.35*eta2

# Measurement model
eta1 <~ 0.7*y11 + 0.9*y12 + 0.8*y13
eta2 =~ 0.7*y21 + 0.7*y22 + 0.9*y23
eta3 =~ 0.9*y31 + 0.8*y32 + 0.7*y33

# Within block indicator correlation of eta1
y11 ~~ 0.2*y12
y11 ~~ 0.3*y13
y12 ~~ 0.5*y13
"

dat <- generateData(dgp, .return_type = "matrix")
dat[1:4, ]

# ==============================================================================
# With variable parameters
# ==============================================================================
### Linear DGP -----------------------------------------------------------------
# Add a label and assign values to for each name
dgp <- "
# Structural model
eta2 ~ 0.2*eta1
eta3 ~ gamma*eta1 + 0.35*eta2

# Measurement model
eta1 <~ 0.7*y11 + 0.9*y12 + 0.8*y13
eta2 =~ 0.7*y21 + 0.7*y22 + 0.9*y23
eta3 =~ 0.9*y31 + 0.8*y32 + 0.7*y33

# Within block indicator correlation
y11 ~~ 0.2*y12
y11 ~~ 0.3*y13
y12 ~~ epsilon*y13
"

dat <- generateData(dgp,
                    "gamma" = c(-0.4, -0.2, 0, 0.2, 0.4),
                    "epsilon" = c(0.1, 0.2, 0.3), .return_type = "data.frame")
dat

### DGP containing a second order construct ------------------------------------
# Second order constructs are supported as well.
dgp_2ndorder <- "
## Path model / Regressions
eta2 ~ 0.5*eta1
eta3 ~ 0.35*eta1 + 0.4*eta2

## Composite model
eta1 <~ 0.8*y41 + 0.6*y42 + 0.6*y43
eta2 <~ 2*y51 + 3*y52 + 5*y53
c1   <~ 0.8*y11 + 0.4*y12
c2   <~ 0.5*y21 + 0.3*y22 + 0.2*y23 + 0.4*y24

## Higher order composite
eta3 <~ 0.4*c1 + 0.4*c2

## Composite indicator correlations
# eta1
y41 ~~ 0.5*y42
y41 ~~ 0.5*y43
y42 ~~ 0.5*y43

# eta2
y51 ~~ 0.2*y52
y51 ~~ 0.3*y53
y52 ~~ 0.4*y53

# eta3 (the 2nd order construct)
c1 ~~ 0.49*c2

# c1-c2
y11 ~~ 0.3125*y12

y21 ~~ 0.4*y22
y21 ~~ 0.3*y23
y21 ~~ 0.31*y24
y22 ~~ 0.28*y23
y22 ~~ 0.31*y24
y23 ~~ 0.3*y24
"

dat <- generateData(dgp_2ndorder, .return_type = "data.frame", .empirical = TRUE)
dat

## Estimate using cSEM
aa <- cSEM::csem(dat, dgp_2ndorder)
cSEM::summarize(aa,) ## parameters estimates are identical to the DGP
                     ## since .empirical = TRUE was used.
