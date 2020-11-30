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
  attr(solver, "nparams") <- value
  solver
}

#' @rdname solver_attributes
#' @export
required_args <- function(solver) {
  union("x", attr(solver, "required_args_nox"))
}

#' Get the count of solver runs
#'
#' @param solver object of class `solver`
#' @export
run_count <- function(solver) {
  solver$run_count()
}

# solver class constructor
# solver is an abstract class
# required_args need not include "x" ("x" is always required)
new_solver <- function(x = list(), nparams = NULL, provides_jacobian = FALSE,
                       required_args = NULL, ..., class = character()) {
  stopifnot(is.list(x))
  stopifnot(is.numeric(nparams) | is.null(nparams))
  rcount <- 0
  x$add_run <- function() {
    rcount <<- rcount + 1
    NULL
  }
  x$run_count <- function() {
    rcount
  }
  structure(
    x,
    nparams = nparams,
    provides_jacobian = provides_jacobian,
    required_args_nox = setdiff(required_args, "x"),
    ...,
    class = c(class, "solver")
  )
}
