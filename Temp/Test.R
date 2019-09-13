model <- "
# Structural model
eta2 ~ gamma1*eta1
eta3 ~ 0.4*eta1 + 0.35*eta2

# Measurement model
eta1 =~ lambda1*y11 + 0.9*y12 + 0.8*y13
eta2 =~ 0.7*y21 + 0.7*y22 + 0.9*y23
eta3 =~ 0.9*y31 + 0.8*y32 + 0.7*y33
"

Models <- generatecSEMModel(model,
                            "gamma1" = c(0.3, 0.6),
                            "lambda1" = c(0.8, 0.85, 0.9))
Models

model_pop <- "
# Structural model
eta2 ~ 0.6*eta1
eta3 ~ 0.4*eta1 + 0.35*eta2

# Measurement model
eta1 =~ 0.8*y11 + 0.9*y12 + 0.8*y13
eta2 =~ 0.7*y21 + 0.7*y22 + 0.9*y23
eta3 =~ 0.9*y31 + 0.8*y32 + 0.7*y33
"

model <- "
# Structural model
eta2 ~ eta1
eta3 ~ eta1 + eta2

# Measurement model
eta1 =~ y11 + y12 + y13
eta2 =~ y21 + y22 + y23
eta3 =~ y31 + y32 + y33
"

dat <- generateData(model_pop, .return_type = "matrix", .N = 1000, .empirical = FALSE)

require(cSEM)
a <- csem(dat, model)
summarize(a)


needs to be set as well.
Here too, variable values may be used

model <- "
# Structural model
eta2 ~ 0.2*eta1
eta3 ~ 0.4*eta1 + 0.35*eta2

# Measurement model
eta1 <~ 0.7*y11 + 0.9*y12 + 0.8*y13
eta2 =~ 0.7*y21 + 0.7*y22 + 0.9*y23
eta3 =~ 0.9*y31 + 0.8*y32 + 0.7*y33

# Within block indicator correlation
y11 ~~ 0.2*y12
y11 ~~ 0.3*y13
y12 ~~ epsilon*y13
"

dat <- generateData(model, "epsilon" = c(0.1, 0.2, 0.3), .return_type = "cor")
dat
