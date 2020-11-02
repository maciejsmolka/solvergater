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
  list(value = NA, gradient = NA)
}

#' @describeIn compute_objective Checks if number of solver parameters (when not
#' `NULL` equals length of given point) and delegates to default method.
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

#' @describeIn compute_objective Computes output using provided functions, ignores
#' `precision`.
#' @export
compute_objective.r_solver <- function(solver, x, precision = NULL, ...) {
  assert_point_not_null(x)
  NextMethod("compute_objective")
  grad <- if (provides_gradient(solver)) solver$gradient(x) else NA
  list(value = solver$objective(x), gradient = grad)
}

#' @describeIn compute_objective Runs solver executable and reads values from output
#' file(s). If solver process exits with non-zero status code, a warning is issued
#' and list of `NA`'s is returned.
#'
#' @export
compute_objective.shell_solver <- function(solver, x, precision, ...) {
  assert_point_not_null(x)
  assert_precision_not_null(precision)
  NextMethod("compute_objective")
  cmd <- paste(solver$cmd, solver$combine_args(x, precision))
  message("Solver command: ", cmd)
  status <- system(cmd, ignore.stdout = solver$ignore.stdout,
                   ignore.stderr = solver$ignore.stderr)
  if (status != 0) {
    warning("Solver exited with status ", status, call. = FALSE)
    return(list(value = NA, gradient = NA))
  } else {
    message("Solver exited normally")
  }
  val <- read_value(solver)
  grad <- read_gradient(solver)
  list(value = val, gradient = grad)
}

