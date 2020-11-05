#' Run the solver
#'
#' Runs the solver executable and extracts the quantity of interest and its
#' Jacobian matrix (if available).
#'
#' @param solver solver object
#' @param x numeric vector representing the point to compute objective at
#' @param ... additional arguments passed to other functions
#'
#' @return List with components:
#' * `qoi` quantity of interest (QOI) at `x`, default implementation returns `NA`;
#' * `jacobian` QOI Jacobian matrix at `x`, default implementation returns `NA`,
#' `NA` is also returned if the solver does not provide Jacobian info.
#'
#' @export
#'
#' @examples
#' run(x = 10)
#' run(NULL, c(2, 4))
run <- function(solver, x, ...) {
  UseMethod("run")
}

#' @export
run.default <- function(solver = NULL, x = NULL, ...) {
  list(qoi = NA, jacobian = NA)
}

#' @describeIn run Checks if number of solver parameters (when not
#' `NULL` equals length of given point) and delegates to default method.
#'
#' @export
run.solver <- function(solver, x, ...) {
  npars <- nparams(solver)
  if (!is.null(npars) && length(x) != npars) {
    stop("Dimension of 'x' must equal number of parameters in 'solver'",
         call. = FALSE)
  }
  NextMethod("run")
}

#' @describeIn run Computes output using provided functions.
#'
#' @export
run.r_solver <- function(solver, x, ...) {
  assert_point_not_null(x)
  NextMethod("run")
  jac <- if (provides_jacobian(solver)) solver$jacobian(x) else NA
  list(qoi = solver$qoi(x), jacobian = jac)
}

#' @describeIn run Runs solver executable and reads values from output
#' file(s). If solver process exits with non-zero status code, a warning is issued
#' and list of `NA`'s is returned.
#'
#' @param precision positive numeric scalar, expected solver accuracy (if applicable)
#'
#' @export
run.shell_solver <- function(solver, x, precision, ...) {
  assert_point_not_null(x)
  assert_precision_not_null(precision)
  NextMethod("run")
  cmd <- paste(solver$cmd, solver$combine_args(x, precision))
  message("Solver command: ", cmd)
  status <- run_in(cmd, solver$wd, ignore.stdout = solver$ignore.stdout,
                   ignore.stderr = solver$ignore.stderr)
  if (status != 0) {
    warning("Solver exited with status ", status, call. = FALSE)
    return(list(qoi = NA, jacobian = NA))
  } else {
    message("Solver exited normally")
  }
  qoi <- read_qoi(solver)
  jac <- read_jacobian(solver)
  list(qoi = qoi, jacobian = jac)
}
