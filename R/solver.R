# solver class constructor
# solver is an abstract class
new_solver <- function(x, nparams = NULL, ..., class = character()) {
  stopifnot(is.list(x))
  stopifnot(is.numeric(nparams) | is.null(nparams))
  structure(
    x,
    nparams = nparams,
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

#' @describeIn compute_objective Checks if number of solver parameters (when not
#' `NULL` equals length of given point and delegates to default method).
#'
#' @export
compute_objective.solver <- function(solver, x, precision, ...) {
  npars <- nparams(solver)
  if (!is.null(npars) && length(x) != npars) {
    stop("Dimension of 'x' must equal number of parameters in 'solver'",
         call. = FALSE)
  }
  NextMethod("compute_objective")
}
