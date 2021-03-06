% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/generateConstructCor.R
\name{generateConstructCor}
\alias{generateConstructCor}
\title{Internal: Compute the construct correlation matrix}
\usage{
generateConstructCor(.Gamma = NULL, .B = NULL, .Phi = NULL)
}
\arguments{
\item{.Gamma}{A matrix containing the path coefficients from the exogenous on
the endogenous constructs.}

\item{.B}{A matrix containing the path coefficients from the endogenous on
the endogenous constructs.}

\item{.Phi}{A symmetric matrix of correlations between exogenous constructs}
}
\value{
A matrix of construct correlations.
}
\description{
Calculate the construct correlation matrix.
}
\keyword{internal}
