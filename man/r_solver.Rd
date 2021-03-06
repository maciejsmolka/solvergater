% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/r_solver.R
\name{r_solver}
\alias{r_solver}
\title{Gateway for solver written in R}
\usage{
r_solver(
  qoi = function(x) 0,
  jacobian = function(x) rep(0, length(x)),
  nparams = NULL,
  required_args = NULL
)
}
\arguments{
\item{qoi}{function to act as the quantity of interest
(should return scalar).}

\item{jacobian}{function to act as the Jacobian matrix for the quantity of
interest (should return vector
of the same lentgh as its parameter and should be the real Jacobian function
for \code{qoi}).}

\item{nparams}{numeric, number of parameters, unspecified if \code{NULL}.}

\item{required_args}{character, mandatory args for \code{\link[=run]{run()}}.}
}
\value{
An object of classes \code{r_solver} and \code{solver}
}
\description{
(Mock) gateway to solver that does not perform any external calls
and computes the quantity of interest and its jacobian using provided
functions. Provided mainly for testing purposes.
}
\examples{
s <- r_solver(qoi = function(x) sum(x^2), jacobian = function(x) 2 * x)
run(s, c(10, 1.5))
}
