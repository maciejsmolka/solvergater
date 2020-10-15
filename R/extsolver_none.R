#' Simulated solver not performing external calls
#'
#' Simulated solver that does not perform any external calls
#' and computes the objective and its gradient using provided functions.
#' Provided for test purposes.
#'
#' @param objective function to act as the objective (should return length-one vector)
#' @param gradient function to act as the objective gradient (should return vector
#' of the same lentgh as its parameter and should be the real gradient function
#' for `objective`)
#'
#' @return An object of classes `extsolver_none` and `extsolver` with two components
#' * `objective` the objective function itself
#' * `gradient` the objective gradient function
#'
#' @export
#'
#' @examples
#' s <- extsolver_none(objective = function(x) sum(x^2), gradient = function(x) 2 * x)
#' compute_objective(s, c(10, 1.5))
extsolver_none <- function(objective = function(x) 0,
                           gradient = function(x) rep(0, length(x))) {
  if (is.null(objective)) {
    stop("Objective function must be provided")
  }
  structure(
    list(
      objective = objective,
      gradient = gradient
    ),
    class = c("extsolver_none", "extsolver")
  )
}

#' @describeIn compute_objective Computes output using provided functions, ignores
#' `precision`.
#' @export
compute_objective.extsolver_none <- function(solver, x, precision = NULL) {
  grad <- if (is.null(solver$gradient)) NA else solver$gradient(x)
  list(value = solver$objective(x), gradient = grad)
}
