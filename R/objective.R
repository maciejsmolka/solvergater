#' Solver objective functions
#'
#' Function factory producing functions for use in optimization. These are
#' objective function and its gradient. They can be used, e.g., as parameters
#' `fn` and `gr` of [stats::optim()].
#'
#' @param solver object of class `solver`.
#' @param data observed ('exact') data.
#' @param misfit_fn function to compute misfit between `data` and result of simulation.
#' @param ... additional args passed to [run()], note that some solvers can require
#' some parameters, e.g. `shell_solver` requires `precision`.
#'
#' @return List with one or two components:
#' * `value` objective value function,
#' * `gradient` objective gradient function, missing if solver does not compute
#' Jacobian matrix.
#'
#' @export
#'
#' @examples
#' s <- fake_simple_solver(4, 5)
#' observed_data <- run(s, c(10, 10, 10, 10), precision = 5.0, silent = TRUE)$qoi
#' x <- c(10.5, 9.44, 10.21, 8.14)
#' solver_funs <- objective_functions(s, observed_data, precision = 30.0, silent = TRUE)
#' solver_funs$value(x)
#' solver_funs$gradient(x)
objective_functions <- function(solver, data, misfit_fn = lsq_misfit, ...) {
  solver_obj <- objective(solver, data, misfit_fn = misfit_fn, ...)
  value <- function(x) {
    solver_obj(x)$value
  }
  result <- list(value = value)
  if (provides_jacobian(solver)) {
    gradient <- function(x) {
      solver_obj(x)$gradient
    }
    result$gradient <- gradient
  }
  result
}

#' Solver objective
#'
#' Function factory producing solver objective function returning value and
#' (possibly) gradient at the same time.
#'
#' @param solver object of class `solver`.
#' @param data observed ('exact') data.
#' @param misfit_fn function to compute misfit between `data` and result of simulation.
#' @param ... additional args passed to [run()], note that some solvers can require
#' some parameters, e.g. `shell_solver` requires `precision`.
#'
#' @return Function with numeric parameter `x` returning list with one or two
#' components:
#' * `value` numeric, objective value,
#' * `gradient` numeric, objective gradient, missing if solver does not compute
#' Jacobian matrix.
#'
#' @export
#'
#' @examples
#' s <- fake_simple_solver(4, 5)
#' observed_data <- run(s, c(10, 10, 10, 10), precision = 5.0, silent = TRUE)$qoi
#' x <- c(10.5, 9.44, 10.21, 8.14)
#' solver_obj <- objective(s, observed_data, precision = 30.0, silent = TRUE)
#' solver_obj(x)
objective <- function(solver, data, misfit_fn = lsq_misfit, ...) {
  function(x) {
    computed <- run(solver, x, ...)
    misfit_fn(computed$qoi, data, jacobian = computed$jacobian)
  }
}

#' Least square misfit
#'
#' Least square (quadratic, i.e. Euclidean-distance) misfit (aka loss function)
#' between `x` and `data` as function of `x`. Additional argument `jacobian`
#' is the Jacobian matrix of the (vector)
#' quantity of interest with respect to problem parameters. Such matrices are
#' computed by, e.g., solvers based on adjoint state method.
#'
#' @param x numeric or complex vector, computed (simulated) data.
#' @param data numeric or complex vector, observed data, must have the same length
#' as `x` (this is the number of quantities of interest).
#' @param jacobian numeric or complex matrix, can be `NULL`, its number of rows
#' must equal the length
#' of `x` and `data`, its number of columns is the number of parameters in
#' considered problem.
#'
#' @return List with one or two components:
#' * `value` numeric scalar;
#' * `gradient` numeric vector with length equal to `ncol(jacobian)`, missing
#' if `jacobian` is `NULL`.
#'
#' @export
#'
lsq_misfit <- function(x, data, jacobian = NULL) {
  stopifnot(is.numeric(x) | is.complex(x))
  stopifnot(is.numeric(data) | is.complex(data))
  stopifnot(length(x) == length(data))
  value <- sum(Re((x - data) * Conj(x - data)))
  result <- list(value = value)
  if (!is.null(jacobian)) {
    stopifnot(is.matrix(jacobian))
    stopifnot(is.numeric(jacobian) | is.complex(jacobian))
    stopifnot(length(x) == nrow(jacobian))
    gradient <- 2 * Re(Conj(x - data) %*% jacobian)
    result$gradient <- as.vector(gradient)
  }
  result
}
