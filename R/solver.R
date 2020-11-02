# solver class constructor
# solver is an abstract class
new_solver <- function(x, nparams = NULL, provides_gradient = FALSE, ...,
                       class = character()) {
  stopifnot(is.list(x))
  stopifnot(is.numeric(nparams) | is.null(nparams))
  structure(
    x,
    nparams = nparams,
    provides_gradient = provides_gradient,
    ...,
    class = c(class, "solver")
  )
}

#' Get number of solver parameters
#'
#' @param solver object of class `solver`
#'
#' @export
nparams <- function(solver) {
  attr(solver, "nparams")
}

#' Set number of solver parameters
#'
#' @param solver object of class `solver`
#' @param value numeric, can be `NULL`
#'
#' @export
'nparams<-' <- function(solver, value) {
  stopifnot(is.numeric(value) | is.null(value))
  'attr<-'(solver, "nparams", value)
}

#' Does this solver provide derivative info?
#'
#' @param solver object of class `solver`
#'
#' @return logical, TRUE if the solver computes the objective gradient
#'
#' @export
#'
#' @examples
#' s <- r_solver()
#' provides_gradient(s)
provides_gradient <- function(solver) {
  attr(solver, "provides_gradient")
}
