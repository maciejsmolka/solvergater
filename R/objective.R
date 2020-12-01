#' Solver objective functions
#'
#' Function factory producing functions for use in optimization. These are
#' objective function and its gradient. They can be used, e.g., as parameters
#' `fn` and `gr` of [stats::optim()]. Note that the functions compute their
#' values through [objective()], however, calls to the latter are
#' *memoised*, so actual solver should never be called twice for the same
#' data (`x`). In particular, computing value and gradient for a given `x`
#' requires only a single solver run. To obtain non-memoised functions
#' use [make_functions()] composed with [objective()] (see example in
#' [make_functions()]).
#'
#' @param solver object of class `solver`.
#' @param data observed ('exact') data.
#' @param ... additional args passed to [objective()] or [memoise::memoise()],
#' note that some solvers can  have some mandatory parameters
#' (see [required_args()]),
#' e.g. solvers created with [adaptive_shell_solver()] require `precision`.
#'
#' @return List with one or two components:
#' \describe{
#' \item{`value`}{objective value function,}
#' \item{`gradient`}{objective gradient function, missing if solver does not compute
#' Jacobian matrix.}
#' }
#'
#' @seealso [memoise::memoise()]
#'
#' @export
#'
#' @examples
#' s <- fake_adaptive_solver(4, 5)
#' result <- run(s, c(10, 10, 10, 10), precision = 5.0, silent = TRUE)
#' observed_data <- result$qoi
#' x <- c(10.5, 9.44, 10.21, 8.14)
#' solver_funs <- objective_functions(s, observed_data, precision = 30.0,
#' silent = TRUE)
#' solver_funs$value(x)
#' solver_funs$gradient(x)
objective_functions <- function(solver, data, ...) {
  obj <- objective(solver, data, ...)
  mem_obj <- do_memoise(obj, ...)
  make_functions(mem_obj, provides_gradient = differentiable(obj))
}

#' Solver objective
#'
#' Function factory producing solver objective function returning value and
#' (possibly) gradient at the same time.
#'
#' @param solver object of class `solver`.
#' @param data observed ('exact') data.
#' @param misfit_fn function to compute misfit between `data` and result of
#' simulation.
#' @param ... additional args passed to [run()], note that some solvers can
#' have some mandatory parameters (see [required_args()]),
#' e.g. solvers created with [adaptive_shell_solver()] require `precision`.
#'
#' @return Function with numeric parameter `x` returning list with one or two
#' components:
#' \describe{
#' \item{`value`}{numeric, objective value,}
#' \item{`gradient`}{numeric, objective gradient, missing if solver does not compute
#' Jacobian matrix.}
#' }
#'
#' @export
#'
#' @examples
#' s <- fake_adaptive_solver(4, 5)
#' result <- run(s, c(10, 10, 10, 10), precision = 5.0, silent = TRUE)
#' observed_data <- result$qoi
#' x <- c(10.5, 9.44, 10.21, 8.14)
#' solver_obj <- objective(s, observed_data, precision = 30.0, silent = TRUE)
#' solver_obj(x)
objective <- function(solver, data, misfit_fn = squared_error, ...) {
  f <- function(x) {
    computed <- run(solver, x, ...)
    if (any(is.na(computed$qoi))) {
      result <- list(value = NA)
      if (provides_jacobian(solver)) {
        result$gradient <- NA
      }
    } else {
      jac <- computed$jacobian
      if (any(is.na(jac))) {
        jac <- NULL
      }
      result <- misfit_fn(computed$qoi, data, jacobian = jac)
      if (provides_jacobian(solver) && is.null(result$gradient)) {
        result$gradient <- NA
      }
    }
    result
  }
  attr(f, "differentiable") <- provides_jacobian(solver)
  f
}

#' Make two functions out of objective
#'
#' Separates function created with [objective()] into value function
#' and gradient function. No memoisation is done here.
#'
#' @param objective objective function, as created with [objective()].
#' @param provides_gradient logical, does `objective` prodive gradient?
#'
#' @return same as in [objective_functions()].
#'
#' @export
#'
#' @examples
#' s <- fake_adaptive_solver(4, 5)
#' result <- run(s, c(10, 10, 10, 10), precision = 5.0, silent = TRUE)
#' observed_data <- result$qoi
#' x <- c(10.5, 9.44, 10.21, 8.14)
#' obj <- objective(s, observed_data, precision = 30.0, silent = TRUE)
#' solver_funs <- make_functions(obj)
#' solver_funs$value(x)
#' solver_funs$gradient(x)
make_functions <- function(objective, provides_gradient = TRUE) {
  val <- function(x) {
    objective(x)$value
  }
  result <- list(value = val)
  if (provides_gradient) {
    grad <- function(x) {
      objective(x)$gradient
    }
    result$gradient <- grad
  }
  result
}

#' Is this function differentiable?
#'
#' @param f function object
#'
#' @export
differentiable <- function(f) {
  attr(f, "differentiable")
}

#' Squared error
#'
#' Squared error (aka loss function) of `x` with respect to `data`,
#' as function of `x`. Additional argument `jacobian`
#' is the Jacobian matrix of the (vector)
#' quantity of interest with respect to problem parameters. Such matrices are
#' computed by, e.g., solvers based on adjoint state method.
#'
#' @param x numeric or complex vector, computed (simulated) data.
#' @param data numeric or complex vector, observed data, must have the same
#' length as `x` (this is the number of quantities of interest).
#' @param jacobian numeric or complex matrix, can be `NULL`, its number of rows
#' must equal the length
#' of `x` and `data`, its number of columns is the number of parameters in
#' considered problem.
#'
#' @return List with one or two components:
#' \describe{
#' \item{`value`}{numeric scalar;}
#' \item{`gradient`}{numeric vector with length equal to `ncol(jacobian)`, missing
#' if `jacobian` is `NULL`.}
#' }
#'
#' @export
#'
squared_error <- function(x, data, jacobian = NULL) {
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

do_memoise <- function(f, ...) {
  arg_names <- names(formals(memoise::memoise))
  arg_names <- arg_names[!(arg_names %in% c("f", "..."))]
  largs <- list(...)
  largs <- largs[names(largs) %in% arg_names | names(largs) == ""]
  largs$f <- f
  largs$envir <- environment(f)
  do.call(memoise::memoise, largs)
}
