#' Compute the objective
#'
#' Computes the objective value and gradient using approprate type of external
#' solver.
#'
#' @param solver solver object
#' @param x numeric vector representing the point to compute objective at
#' @param precision length 1 numeric vector indicating desired solver accuracy (solver-dependent)
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
compute_objective <- function(solver, x, precision) {
  UseMethod("compute_objective")
}

#' @export
compute_objective.default <- function(solver = NULL, x = NULL, precision = NULL) {
  return(list(value = NA, gradient = NA))
}
