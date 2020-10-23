#' Compute the objective
#'
#' Computes the objective value and gradient using approprate type of external
#' solver.
#'
#' @param solver solver object
#' @param x numeric vector representing the point to compute objective at
#' @param precision length 1 numeric vector indicating desired solver accuracy (solver-dependent)
#' @param ... additional arguments passed to other functions
#'
#' @return List with components:
#' * `value` objective value at `x`, default implementation returns `NA`;
#' * `gradient` gradient of the objective at `x`, default implementation returns `NA`,
#' `NA` is also returned if the solver does not provide gradient info.
#'
#' @export
#'
#' @examples
#' compute_objective(x = 10)
#' compute_objective(NULL, c(2, 4))
compute_objective <- function(solver, x, precision, ...) {
  UseMethod("compute_objective")
}

#' @export
compute_objective.default <- function(solver = NULL, x = NULL, precision = NULL, ...) {
  return(list(value = NA, gradient = NA))
}

#' Assert that point is not `NULL`
#'
#' @param x numeric vector representing point
#'
#' @export
assert_point_not_null <- function(x) {
  assert_arg_not_null(x, "point")
}

#' Assert that precision is not `NULL`
#'
#' @param precision lenght 1 numeric vector representing precision
#'
#' @export
assert_precision_not_null <- function(precision) {
  assert_arg_not_null(precision, "precision")
}

#' Assert that an arg is not `NULL`
#'
#' @param arg_value argument value
#' @param arg_name argument name
#'
#' @export
assert_arg_not_null <- function(arg_value, arg_name) {
  if (is.null(arg_value)) {
    stop("Argument value not provided: ", arg_name)
  }
}
