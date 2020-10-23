#' Simple external solver gateway
#'
#' Gateway to simple external solver that stores objective value and gradient
#' in separate files with simple format (real numbers separated with whitespaces).
#'
#' @param cmd command to run the solver executable
#' @param value_file name of file containing the computed objective value, cannot
#' be `NULL`
#' @param gradient_file name of file containing the computed objective gradient,
#' `NULL` indicates that the solver does not provide gradient info
#' @param value_read_fn function reading the objective value from file
#' @param gradient_read_fn function reading the objective gradient from file
#' @param arg_combine_fn function producing appropriate length-1 character vector from
#' point and precision for solver command line
#'
#' @return An object of classes `extsolver_simple` and `extsolver`
#'
#' @export
#'
#' @examples
#' rscript_path <- file.path(R.home(), "bin", "Rscript")
#' solver_path <- file.path(find.package("solvergater"), "exec", "fake_simple.R")
#' solver_cmd <- paste(rscript_path, solver_path)
#' s <- extsolver_simple(solver_cmd, value_file = "output_value", gradient_file = "output_gradient")
#' old_wd <- getwd()
#' setwd(tempdir())
#' compute_objective(s, c(20, 5), 10)
#' setwd(old_wd)
extsolver_simple <- function(
  cmd,
  value_file,
  gradient_file = NULL,
  value_read_fn = function(file) scan(file, quiet = TRUE),
  gradient_read_fn = if (!is.null(gradient_file)) value_read_fn,
  arg_combine_fn = function(x, precision) paste(c(x, precision), collapse = " ")
  ) {
  if (is.null(cmd)) {
    stop("Command to run solver must be provided", call. = FALSE)
  }
  if (is.null(value_file)) {
    stop("Value file name must be provided", call. = FALSE)
  }
  structure(
    list(
      cmd = cmd,
      value_file = value_file,
      gradient_file = gradient_file,
      read_value = value_read_fn,
      read_gradient = gradient_read_fn,
      combine_args = arg_combine_fn
    ),
    class = c("extsolver_simple", "extsolver")
  )
}

#' @describeIn compute_objective Runs solver executable and reads values from output
#' file(s). If solver process exits with non-zero status code, a warning is issued
#' and list of `NA`'s is returned. In `...` one can pass `ignore.stdout` and
#' `ignore.stderr` that are in turn passed to [base::system()].
#' @export
compute_objective.extsolver_simple <- function(solver, x, precision, ...) {
  assert_point_not_null(x)
  assert_precision_not_null(precision)
  cmd <- paste(solver$cmd, solver$combine_args(x, precision))
  args <- list(...)
  if (is.null(args$ignore.stdout)) {
    args$ignore.stdout <- FALSE
  }
  if (is.null(args$ignore.stderr)) {
    args$ignore.stderr <- FALSE
  }
  message("Solver command: ", cmd)
  status <- system(cmd, ignore.stdout = args$ignore.stdout,
                   ignore.stderr = args$ignore.stderr)
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

#' @describeIn provides_gradient `TRUE` if gradient file has been provided
#' @export
provides_gradient.extsolver_simple <- function(solver) {
  !is.null(solver$gradient_file)
}

read_value <- function(solver) {
  if (!file.exists(solver$value_file)) {
    stop("Value file does not exist: ", solver$value_file, call. = FALSE)
  }
  solver$read_value(solver$value_file)
}

read_gradient <- function(solver) {
  if (!provides_gradient(solver)) {
    return(NA)
  }
  if (!file.exists(solver$gradient_file)) {
    return(NA)
  }
  solver$read_gradient(solver$gradient_file)
}
