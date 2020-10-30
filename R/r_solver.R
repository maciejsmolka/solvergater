#' Gateway for solver written in R
#'
#' (Mock) gateway to solver that does not perform any external calls
#' and computes the objective and its gradient using provided functions.
#' Provided mainly for testing purposes.
#'
#' @param objective function to act as the objective (should return length-one vector)
#' @param gradient function to act as the objective gradient (should return vector
#' of the same lentgh as its parameter and should be the real gradient function
#' for `objective`)
#' @param nparams numeric, number of parameters, unspecified if `NULL`
#'
#' @return An object of classes `r_solver` and `solver`
#'
#' @export
#'
#' @examples
#' s <- r_solver(objective = function(x) sum(x^2), gradient = function(x) 2 * x)
#' compute_objective(s, c(10, 1.5))
r_solver <- function(
  objective = function(x) 0,
  gradient = function(x) rep(0, length(x)),
  nparams = NULL
  ) {
  validate_r_solver(
    new_r_solver(
      list(objective = objective, gradient = gradient),
      nparams = nparams
    )
  )
}

new_r_solver <- function(x, nparams) {
  new_solver(x, nparams, class = "r_solver")
}

validate_r_solver <- function(x) {
  if (is.null(x$objective)) {
    stop("Objective function must be provided", call. = FALSE)
  }
  x
}

#' @describeIn compute_objective Computes output using provided functions, ignores
#' `precision`.
#' @export
compute_objective.r_solver <- function(solver, x, precision = NULL, ...) {
  assert_point_not_null(x)
  NextMethod("compute_objective")
  grad <- if (provides_gradient(solver)) solver$gradient(x) else NA
  list(value = solver$objective(x), gradient = grad)
}

#' @describeIn provides_gradient `TRUE` if gradient function has been provided
#' @export
provides_gradient.r_solver <- function(solver) {
  !is.null(solver$gradient)
}
