#' Gateway for solver written in R
#'
#' (Mock) gateway to solver that does not perform any external calls
#' and computes the quantity of interest and its jacobian using provided
#' functions. Provided mainly for testing purposes.
#'
#' @param qoi function to act as the quantity of interest
#' (should return scalar)
#' @param jacobian function to act as the Jacobian matrix for the quantity of
#' interest (should return vector
#' of the same lentgh as its parameter and should be the real Jacobian function
#' for `qoi`)
#' @param nparams numeric, number of parameters, unspecified if `NULL`
#'
#' @return An object of classes `r_solver` and `solver`
#'
#' @export
#'
#' @examples
#' s <- r_solver(qoi = function(x) sum(x^2), jacobian = function(x) 2 * x)
#' run(s, c(10, 1.5))
r_solver <- function(
  qoi = function(x) 0,
  jacobian = function(x) rep(0, length(x)),
  nparams = NULL
  ) {
  validate_r_solver(
    new_r_solver(
      list(qoi = qoi, jacobian = jacobian),
      nparams = nparams,
      provides_jacobian = is.function(jacobian)
    )
  )
}

new_r_solver <- function(x, nparams, provides_jacobian) {
  new_solver(x, nparams = nparams, provides_jacobian = provides_jacobian,
             class = "r_solver")
}

validate_r_solver <- function(x) {
  if (is.null(x$qoi)) {
    stop("Quantity of interest must be provided", call. = FALSE)
  }
  x
}
