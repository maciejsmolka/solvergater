% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/objective.R
\name{objective_functions}
\alias{objective_functions}
\title{Solver objective functions}
\usage{
objective_functions(solver, data, ...)
}
\arguments{
\item{solver}{object of class \code{solver}.}

\item{data}{observed ('exact') data.}

\item{...}{additional args passed to \code{\link[=objective]{objective()}} or \code{\link[memoise:memoise]{memoise::memoise()}},
note that some solvers can  have some mandatory parameters
(see \code{\link[=required_args]{required_args()}}),
e.g. solvers created with \code{\link[=adaptive_shell_solver]{adaptive_shell_solver()}} require \code{precision}.}
}
\value{
List with one or two components:
\describe{
\item{\code{value}}{objective value function,}
\item{\code{gradient}}{objective gradient function, missing if solver does not compute
Jacobian matrix.}
}
}
\description{
Function factory producing functions for use in optimization. These are
objective function and its gradient. They can be used, e.g., as parameters
\code{fn} and \code{gr} of \code{\link[stats:optim]{stats::optim()}}. Note that the functions compute their
values through \code{\link[=objective]{objective()}}, however, calls to the latter are
\emph{memoised}, so actual solver should never be called twice for the same
data (\code{x}). In particular, computing value and gradient for a given \code{x}
requires only a single solver run. To obtain non-memoised functions
use \code{\link[=make_functions]{make_functions()}} composed with \code{\link[=objective]{objective()}} (see example in
\code{\link[=make_functions]{make_functions()}}).
}
\examples{
s <- fake_adaptive_solver(4, 5)
result <- run(s, c(10, 10, 10, 10), precision = 5.0, silent = TRUE)
observed_data <- result$qoi
x <- c(10.5, 9.44, 10.21, 8.14)
solver_funs <- objective_functions(s, observed_data, precision = 30.0,
silent = TRUE)
solver_funs$value(x)
solver_funs$gradient(x)
}
\seealso{
\code{\link[memoise:memoise]{memoise::memoise()}}
}
