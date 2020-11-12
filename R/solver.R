#' Solver attribute accessors
#'
#' Functions to get and set values of the most useful attributes of `solver`
#' objects.
#'
#' @name solver_attributes
#' @param solver object of class `solver`
#' @param value numeric scalar, can be `NULL`
#'
#' @examples
#' s <- r_solver(nparams = 10)
#' provides_jacobian(s)
#' nparams(s)
NULL

#' @export
#' @rdname solver_attributes
#'
provides_jacobian <- function(solver) {
  attr(solver, "provides_jacobian")
}

#' @export
#' @rdname solver_attributes
#'
nparams <- function(solver) {
  attr(solver, "nparams")
}

#' @rdname solver_attributes
#' @export
"nparams<-" <- function(solver, value) {
  stopifnot(is.numeric(value) | is.null(value))
  "attr<-"(solver, "nparams", value)
}

# solver class constructor
# solver is an abstract class
new_solver <- function(x, nparams = NULL, provides_jacobian = FALSE, ...,
                       class = character()) {
  stopifnot(is.list(x))
  stopifnot(is.numeric(nparams) | is.null(nparams))
  structure(
    x,
    nparams = nparams,
    provides_jacobian = provides_jacobian,
    ...,
    class = c(class, "solver")
  )
}
