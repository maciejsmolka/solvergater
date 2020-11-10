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
#' @param ignore.stdout logical, if not `NULL` overrides default setting in
#' `shell_solver` object
#' @param ignore.stderr logical, if not `NULL` overrides default setting in
#' `shell_solver` object
#' @param silent logical, suppress diagnostic messages (not warnings), `TRUE` forces
#' `ignore.stdout = TRUE` and `ignore.stderr = TRUE`
#'
#' @export
run.shell_solver <- function(solver, x, precision, ignore.stdout = NULL,
                             ignore.stderr = NULL, silent = FALSE, ...) {
  assert_point_not_null(x)
  assert_precision_not_null(precision)
  if (silent) {
    ignore.stdout <- TRUE
    ignore.stderr <- TRUE
  }
  NextMethod("run")
  cmd <- paste(solver$cmd, solver$combine_args(x, precision))
  s_message("Solver command: ", cmd, silent = silent)
  do_ignore_stdout <- solver$ignore.stdout
  if (!is.null(ignore.stdout)) {
    do_ignore_stdout <- ignore.stdout
  }
  do_ignore_stderr <- solver$ignore.stderr
  if (!is.null(ignore.stderr)) {
    do_ignore_stderr <- ignore.stderr
  }
  result <- do_run(solver, cmd, ignore.stdout = do_ignore_stdout,
                   ignore.stderr = do_ignore_stderr, silent = silent)
  if (result$status != 0) {
    warning("Solver exited with status ", result$status, call. = FALSE)
  } else {
    s_message("Solver exited normally", silent = silent)
  }
  result[["status"]] <- NULL
  result
}

# Run given command in given directory
do_run <- function(solver, cmd, ignore.stdout, ignore.stderr, silent) {
  if (!is.null(solver$wd)) {
    old_wd <- getwd()
    s_message("Entering ", solver$wd, silent = silent)
    setwd(solver$wd)
  }
  status <- system(cmd, ignore.stdout = ignore.stdout, ignore.stderr = ignore.stderr)
  if (status != 0) {
    return(list(status = status, qoi = NA, jacobian = NA))
  }
  qoi <- read_qoi(solver)
  jac <- read_jacobian(solver)
  if (!is.null(solver$wd)) {
    s_message("Exiting ", solver$wd, silent = silent)
    setwd(old_wd)
  }
  list(status = 0, qoi = qoi, jacobian = jac)
}

# Read QOI file (2nd arg needed if run from outside solver working dir)
read_qoi <- function(solver, make_path_absolute = FALSE) {
  qoi_file <- solver$qoi_file
  if (make_path_absolute) {
    qoi_file <- output_file(solver, "qoi")
  }
  if (!file.exists(qoi_file)) {
    stop("Quantity-of-interest file does not exist: ", qoi_file, call. = FALSE)
  }
  solver$read_qoi(qoi_file)
}

# Read QOI Jacobian file (2nd arg needed if run from outside solver working dir)
read_jacobian <- function(solver, make_path_absolute = FALSE) {
  if (!provides_jacobian(solver)) {
    return(NA)
  }
  jacobian_file <- solver$jacobian_file
  if (make_path_absolute) {
    jacobian_file <- output_file(solver, "jacobian")
  }
  if (!file.exists(jacobian_file)) {
    return(NA)
  }
  solver$read_jacobian(jacobian_file)
}
